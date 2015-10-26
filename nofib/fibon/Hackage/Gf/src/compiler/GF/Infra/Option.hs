module GF.Infra.Option
    (
     -- * Option types
     Options, 
     Flags(..), 
     Mode(..), Phase(..), Verbosity(..), OutputFormat(..), 
     SISRFormat(..), Optimization(..), CFGTransform(..), HaskellOption(..),
     Dump(..), Printer(..), Recomp(..),
     -- * Option parsing 
     parseOptions, parseModuleOptions, fixRelativeLibPaths,
     -- * Option pretty-printing
     optionsGFO,
     optionsPGF,
     -- * Option manipulation
     addOptions, concatOptions, noOptions,
     modifyFlags,
     helpMessage,
     -- * Checking specific options
     flag, cfgTransform, haskellOption, readOutputFormat,
     isLexicalCat, renameEncoding,
     -- * Setting specific options
     setOptimization, setCFGTransform,
     -- * Convenience methods for checking options    
     verbAtLeast, dump
    ) where

import Control.Monad
import Data.Char (toLower, isDigit)
import Data.List
import Data.Maybe
import GF.Infra.GetOpt
--import System.Console.GetOpt
import System.FilePath
import System.IO

import GF.Data.ErrM

import Data.Set (Set)
import qualified Data.Set as Set




usageHeader :: String
usageHeader = unlines 
 ["Usage: gfc [OPTIONS] [FILE [...]]",
  "",
  "How each FILE is handled depends on the file name suffix:",
  "",
  ".gf Normal or old GF source, will be compiled.",
  ".gfo Compiled GF source, will be loaded as is.",
  ".gfe Example-based GF source, will be converted to .gf and compiled.",
  ".ebnf Extended BNF format, will be converted to .gf and compiled.",
  ".cf Context-free (BNF) format, will be converted to .gf and compiled.",
  "",
  "If multiple FILES are given, they must be normal GF source, .gfo or .gfe files.",
  "For the other input formats, only one file can be given.",
  "",
  "Command-line options:"]


helpMessage :: String
helpMessage = usageInfo usageHeader optDescr


-- FIXME: do we really want multi-line errors?
errors :: [String] -> Err a
errors = fail . unlines

-- Types

data Mode = ModeVersion | ModeHelp | ModeInteractive | ModeRun | ModeCompiler
  deriving (Show,Eq,Ord)

data Verbosity = Quiet | Normal | Verbose | Debug
  deriving (Show,Eq,Ord,Enum,Bounded)

data Phase = Preproc | Convert | Compile | Link
  deriving (Show,Eq,Ord)

data OutputFormat = FmtPGFPretty
                  | FmtJavaScript 
                  | FmtHaskell 
                  | FmtProlog
                  | FmtProlog_Abs
                  | FmtLambdaProlog
                  | FmtBNF
                  | FmtEBNF
                  | FmtRegular
                  | FmtNoLR
                  | FmtSRGS_XML
                  | FmtSRGS_XML_NonRec
                  | FmtSRGS_ABNF 
                  | FmtSRGS_ABNF_NonRec
                  | FmtJSGF 
                  | FmtGSL 
                  | FmtVoiceXML
                  | FmtSLF
                  | FmtRegExp
                  | FmtFA
  deriving (Eq,Ord)

data SISRFormat = 
    -- | SISR Working draft 1 April 2003
    --   <http://www.w3.org/TR/2003/WD-semantic-interpretation-20030401/>
    SISR_WD20030401    
  | SISR_1_0
 deriving (Show,Eq,Ord)

data Optimization = OptStem | OptCSE | OptExpand | OptParametrize
  deriving (Show,Eq,Ord)

data CFGTransform = CFGNoLR 
                  | CFGRegular
                  | CFGTopDownFilter 
                  | CFGBottomUpFilter 
                  | CFGStartCatOnly
                  | CFGMergeIdentical
                  | CFGRemoveCycles
  deriving (Show,Eq,Ord)

data HaskellOption = HaskellNoPrefix | HaskellGADT | HaskellLexical
  deriving (Show,Eq,Ord)

data Warning = WarnMissingLincat
  deriving (Show,Eq,Ord)

data Dump = DumpSource | DumpRebuild | DumpExtend | DumpRename | DumpTypeCheck | DumpRefresh | DumpOptimize | DumpCanon
  deriving (Show,Eq,Ord)

-- | Pretty-printing options
data Printer = PrinterStrip -- ^ Remove name qualifiers.
  deriving (Show,Eq,Ord)

data Recomp = AlwaysRecomp | RecompIfNewer | NeverRecomp
  deriving (Show,Eq,Ord)

data Flags = Flags {
      optMode            :: Mode,
      optStopAfterPhase  :: Phase,
      optVerbosity       :: Verbosity,
      optProf            :: Bool,
      optShowCPUTime     :: Bool,
      optEmitGFO         :: Bool,
      optOutputFormats   :: [OutputFormat],
      optSISR            :: Maybe SISRFormat,
      optHaskellOptions  :: Set HaskellOption,
      optLexicalCats     :: Set String,
      optGFODir          :: Maybe FilePath,
      optOutputFile      :: Maybe FilePath,
      optOutputDir       :: Maybe FilePath,
      optGFLibPath       :: Maybe FilePath,
      optRecomp          :: Recomp,
      optPrinter         :: [Printer],
      optProb            :: Bool,
      optRetainResource  :: Bool,
      optName            :: Maybe String,
      optAbsName         :: Maybe String,
      optCncName         :: Maybe String,
      optResName         :: Maybe String,
      optPreprocessors   :: [String],
      optEncoding        :: String,
      optOptimizations   :: Set Optimization,
      optCFGTransforms   :: Set CFGTransform,
      optLibraryPath     :: [FilePath],
      optStartCat        :: Maybe String,
      optSpeechLanguage  :: Maybe String,
      optLexer           :: Maybe String,
      optUnlexer         :: Maybe String,
      optWarnings        :: [Warning],
      optDump            :: [Dump]
    }
  deriving (Show)

newtype Options = Options (Flags -> Flags)

instance Show Options where
    show (Options o) = show (o defaultFlags)

-- Option parsing

parseOptions :: [String]                   -- ^ list of string arguments
             -> Err (Options, [FilePath])
parseOptions args 
  | not (null errs) = errors errs
  | otherwise       = do opts <- liftM concatOptions $ sequence optss
                         return (opts, files)
  where
    (optss, files, errs) = getOpt RequireOrder optDescr args

parseModuleOptions :: [String]                   -- ^ list of string arguments
                   -> Err Options
parseModuleOptions args = do
  (opts,nonopts) <- parseOptions args
  if null nonopts 
    then return opts
    else errors $ map ("Non-option among module options: " ++) nonopts

fixRelativeLibPaths curr_dir lib_dir (Options o) = Options (fixPathFlags . o)
  where
    fixPathFlags f@(Flags{optLibraryPath=path}) = f{optLibraryPath=concatMap (\dir -> [curr_dir </> dir, lib_dir </> dir]) path}

-- Showing options

-- | Pretty-print the options that are preserved in .gfo files.
optionsGFO :: Options -> [(String,String)]
optionsGFO opts = optionsPGF opts
      ++ [("coding", flag optEncoding opts)]

-- | Pretty-print the options that are preserved in .pgf files.
optionsPGF :: Options -> [(String,String)]
optionsPGF opts = 
         maybe [] (\x -> [("language",x)]) (flag optSpeechLanguage opts)
      ++ maybe [] (\x -> [("startcat",x)]) (flag optStartCat opts)

-- Option manipulation

flag :: (Flags -> a) -> Options -> a
flag f (Options o) = f (o defaultFlags)

addOptions :: Options -> Options -> Options
addOptions (Options o1) (Options o2) = Options (o2 . o1)

noOptions :: Options
noOptions = Options id

concatOptions :: [Options] -> Options
concatOptions = foldr addOptions noOptions

modifyFlags :: (Flags -> Flags) -> Options
modifyFlags = Options

-- Default options

defaultFlags :: Flags
defaultFlags = Flags {
      optMode            = ModeInteractive,
      optStopAfterPhase  = Compile,
      optVerbosity       = Normal,
      optProf            = False,
      optShowCPUTime     = False,
      optEmitGFO         = True,
      optOutputFormats   = [],
      optSISR            = Nothing,
      optHaskellOptions  = Set.empty,
      optLexicalCats     = Set.empty,
      optGFODir          = Nothing,
      optOutputFile      = Nothing,
      optOutputDir       = Nothing,
      optGFLibPath       = Nothing,
      optRecomp          = RecompIfNewer,
      optPrinter         = [],
      optProb            = False,
      optRetainResource  = False,

      optName            = Nothing,
      optAbsName         = Nothing,
      optCncName         = Nothing,
      optResName         = Nothing,
      optPreprocessors   = [],
      optEncoding        = "latin1",
      optOptimizations   = Set.fromList [OptStem,OptCSE,OptExpand,OptParametrize],
      optCFGTransforms   = Set.fromList [CFGRemoveCycles, CFGBottomUpFilter, 
                                         CFGTopDownFilter, CFGMergeIdentical],
      optLibraryPath     = [],
      optStartCat        = Nothing,
      optSpeechLanguage  = Nothing,
      optLexer           = Nothing,
      optUnlexer         = Nothing,
      optWarnings        = [],
      optDump            = []
    }

-- Option descriptions

optDescr :: [OptDescr (Err Options)]
optDescr = 
    [
     Option ['?','h'] ["help"] (NoArg (mode ModeHelp)) "Show help message.",
     Option ['V'] ["version"] (NoArg (mode ModeVersion)) "Display GF version number.",
     Option ['v'] ["verbose"] (OptArg verbosity "N") "Set verbosity (default 1). -v alone is the same as -v 2.",
     Option ['q','s'] ["quiet"] (NoArg (verbosity (Just "0"))) "Quiet, same as -v 0.",
     Option [] ["batch"] (NoArg (mode ModeCompiler)) "Run in batch compiler mode.",
     Option [] ["interactive"] (NoArg (mode ModeInteractive)) "Run in interactive mode (default).",
     Option [] ["run"] (NoArg (mode ModeRun)) "Run in interactive mode, showing output only (no other messages).",
     Option ['E'] [] (NoArg (phase Preproc)) "Stop after preprocessing (with --preproc).",
     Option ['C'] [] (NoArg (phase Convert)) "Stop after conversion to .gf.",
     Option ['c'] [] (NoArg (phase Compile)) "Stop after compiling to .gfo (default) .",
     Option [] ["make"] (NoArg (liftM2 addOptions (mode ModeCompiler) (phase Link))) "Build .pgf file and other output files and exit.",
     Option [] ["prof"] (NoArg (prof True)) "Dump profiling information when compiling to PMCFG",
     Option [] ["cpu"] (NoArg (cpu True)) "Show compilation CPU time statistics.",
     Option [] ["no-cpu"] (NoArg (cpu False)) "Don't show compilation CPU time statistics (default).",
     Option [] ["emit-gfo"] (NoArg (emitGFO True)) "Create .gfo files (default).",
     Option [] ["no-emit-gfo"] (NoArg (emitGFO False)) "Do not create .gfo files.",
     Option [] ["gfo-dir"] (ReqArg gfoDir "DIR") "Directory to put .gfo files in (default = '.').",
     Option ['f'] ["output-format"] (ReqArg outFmt "FMT") 
        (unlines ["Output format. FMT can be one of:",
                  "Multiple concrete: pgf (default), gar, js, prolog, ...",
                  "Single concrete only: cf, bnf, lbnf, gsl, srgs_xml, srgs_abnf, ...",
                  "Abstract only: haskell, prolog_abs, ..."]),
     Option [] ["sisr"] (ReqArg sisrFmt "FMT") 
        (unlines ["Include SISR tags in generated speech recognition grammars.",
                  "FMT can be one of: old, 1.0"]),
     Option [] ["haskell"] (ReqArg hsOption "OPTION") 
            ("Turn on an optional feature when generating Haskell data types. OPTION = " 
             ++ concat (intersperse " | " (map fst haskellOptionNames))),
     Option [] ["lexical"] (ReqArg lexicalCat "CAT[,CAT[...]]") 
            "Treat CAT as a lexical category.",
     Option ['o'] ["output-file"] (ReqArg outFile "FILE") 
           "Save output in FILE (default is out.X, where X depends on output format.",
     Option ['D'] ["output-dir"] (ReqArg outDir "DIR") 
           "Save output files (other than .gfo files) in DIR.",
     Option [] ["gf-lib-path"] (ReqArg gfLibPath "DIR") 
           "Overides the value of GF_LIB_PATH.",
     Option [] ["src","force-recomp"] (NoArg (recomp AlwaysRecomp)) 
                 "Always recompile from source.",
     Option [] ["gfo","recomp-if-newer"] (NoArg (recomp RecompIfNewer)) 
                 "(default) Recompile from source if the source is newer than the .gfo file.",
     Option [] ["gfo","no-recomp"] (NoArg (recomp NeverRecomp)) 
                 "Never recompile from source, if there is already .gfo file.",
     Option [] ["strip"] (NoArg (printer PrinterStrip))
                 "Remove name qualifiers when pretty-printing.",
     Option [] ["retain"] (NoArg (set $ \o -> o { optRetainResource = True })) "Retain opers.",
     Option [] ["prob"] (NoArg (prob True)) "Read probabilities from '--# prob' pragmas.",
     Option ['n'] ["name"] (ReqArg name "NAME") 
           (unlines ["Use NAME as the name of the output. This is used in the output file names, ",
                     "with suffixes depending on the formats, and, when relevant, ",
                     "internally in the output."]),
     Option [] ["abs"] (ReqArg absName "NAME")
            ("Use NAME as the name of the abstract syntax module generated from "
             ++ "a grammar in GF 1 format."),
     Option [] ["cnc"] (ReqArg cncName "NAME")
            ("Use NAME as the name of the concrete syntax module generated from "
             ++ "a grammar in GF 1 format."),
     Option [] ["res"] (ReqArg resName "NAME")
            ("Use NAME as the name of the resource module generated from "
             ++ "a grammar in GF 1 format."),
     Option ['i'] [] (ReqArg addLibDir "DIR") "Add DIR to the library search path.",
     Option [] ["path"] (ReqArg setLibPath "DIR:DIR:...") "Set the library search path.",
     Option [] ["preproc"] (ReqArg preproc "CMD") 
                 (unlines ["Use CMD to preprocess input files.",
                           "Multiple preprocessors can be used by giving this option multiple times."]),
     Option [] ["coding"] (ReqArg coding "ENCODING") 
                ("Character encoding of the source grammar, ENCODING = utf8, latin1, cp1251, ..."),
     Option [] ["startcat"] (ReqArg startcat "CAT") "Grammar start category.",
     Option [] ["language"] (ReqArg language "LANG") "Set the speech language flag to LANG in the generated grammar.",
     Option [] ["lexer"] (ReqArg lexer "LEXER") "Use lexer LEXER.",
     Option [] ["unlexer"] (ReqArg unlexer "UNLEXER") "Use unlexer UNLEXER.",
     Option [] ["optimize"] (ReqArg optimize "OPT") 
                "Select an optimization package. OPT = all | values | parametrize | none",
     Option [] ["stem"] (onOff (toggleOptimize OptStem) True) "Perform stem-suffix analysis (default on).",
     Option [] ["cse"] (onOff (toggleOptimize OptCSE) True) "Perform common sub-expression elimination (default on).",
     Option [] ["cfg"] (ReqArg cfgTransform "TRANS") "Enable or disable specific CFG transformations. TRANS = merge, no-merge, bottomup, no-bottomup, ...",
     dumpOption "source" DumpSource,
     dumpOption "rebuild" DumpRebuild,
     dumpOption "extend" DumpExtend,
     dumpOption "rename" DumpRename,
     dumpOption "tc" DumpTypeCheck,
     dumpOption "refresh" DumpRefresh,
     dumpOption "opt" DumpOptimize,
     dumpOption "canon" DumpCanon

    ]
 where phase       x = set $ \o -> o { optStopAfterPhase = x }
       mode        x = set $ \o -> o { optMode = x }
       verbosity mv  = case mv of
                           Nothing -> set $ \o -> o { optVerbosity = Verbose }
                           Just v  -> case readMaybe v >>= toEnumBounded of
                                        Just i  -> set $ \o -> o { optVerbosity = i }
                                        Nothing -> fail $ "Bad verbosity: " ++ show v
       prof        x = set $ \o -> o { optProf = x }
       cpu         x = set $ \o -> o { optShowCPUTime = x }
       emitGFO     x = set $ \o -> o { optEmitGFO = x }
       gfoDir      x = set $ \o -> o { optGFODir = Just x }
       outFmt      x = readOutputFormat x >>= \f ->
                         set $ \o -> o { optOutputFormats = optOutputFormats o ++ [f] }
       sisrFmt     x = case x of
                         "old" -> set $ \o -> o { optSISR = Just SISR_WD20030401 }
                         "1.0" -> set $ \o -> o { optSISR = Just SISR_1_0 }
                         _     -> fail $ "Unknown SISR format: " ++ show x
       hsOption    x = case lookup x haskellOptionNames of
                         Just p  -> set $ \o -> o { optHaskellOptions = Set.insert p (optHaskellOptions o) }
                         Nothing -> fail $ "Unknown Haskell option: " ++ x
                                            ++ " Known: " ++ show (map fst haskellOptionNames)
       lexicalCat  x = set $ \o -> o { optLexicalCats = foldr Set.insert (optLexicalCats o) (splitBy (==',') x) }
       outFile     x = set $ \o -> o { optOutputFile = Just x }
       outDir      x = set $ \o -> o { optOutputDir = Just x }
       gfLibPath   x = set $ \o -> o { optGFLibPath = Just x }
       recomp      x = set $ \o -> o { optRecomp = x }
       printer     x = set $ \o -> o { optPrinter = x : optPrinter o }
       prob        x = set $ \o -> o { optProb = x }

       name        x = set $ \o -> o { optName = Just x }
       absName     x = set $ \o -> o { optAbsName = Just x }
       cncName     x = set $ \o -> o { optCncName = Just x }
       resName     x = set $ \o -> o { optResName = Just x }
       addLibDir   x = set $ \o -> o { optLibraryPath = x:optLibraryPath o }
       setLibPath  x = set $ \o -> o { optLibraryPath = splitInModuleSearchPath x }
       preproc     x = set $ \o -> o { optPreprocessors = optPreprocessors o ++ [x] }
       coding      x = set $ \o -> o { optEncoding = x }
       startcat    x = set $ \o -> o { optStartCat = Just x }
       language    x = set $ \o -> o { optSpeechLanguage = Just x }
       lexer       x = set $ \o -> o { optLexer = Just x }
       unlexer     x = set $ \o -> o { optUnlexer = Just x }

       optimize    x = case lookup x optimizationPackages of
                         Just p  -> set $ \o -> o { optOptimizations = p }
                         Nothing -> fail $ "Unknown optimization package: " ++ x

       toggleOptimize x b = set $ setOptimization' x b

       cfgTransform x = let (x', b) = case x of
                                        'n':'o':'-':rest -> (rest, False)
                                        _                -> (x, True)
                         in case lookup x' cfgTransformNames of
                              Just t  -> set $ setCFGTransform' t b
                              Nothing -> fail $ "Unknown CFG transformation: " ++ x'
                                                ++ " Known: " ++ show (map fst cfgTransformNames)

       dumpOption s d = Option [] ["dump-"++s] (NoArg (set $ \o -> o { optDump = d:optDump o})) ("Dump output of the " ++ s ++ " phase.")

       set = return . Options

outputFormats :: [(String,OutputFormat)]
outputFormats = 
    [("pgf_pretty",   FmtPGFPretty),
     ("js",           FmtJavaScript),
     ("haskell",      FmtHaskell),
     ("prolog",       FmtProlog),
     ("prolog_abs",   FmtProlog_Abs),
     ("lambda_prolog",FmtLambdaProlog),
     ("bnf",          FmtBNF),
     ("ebnf",         FmtEBNF),
     ("regular",      FmtRegular),
     ("nolr",         FmtNoLR),
     ("srgs_xml",     FmtSRGS_XML),
     ("srgs_xml_nonrec",     FmtSRGS_XML_NonRec),
     ("srgs_abnf",    FmtSRGS_ABNF),
     ("srgs_abnf_nonrec",    FmtSRGS_ABNF_NonRec),
     ("jsgf",         FmtJSGF),
     ("gsl",          FmtGSL),
     ("vxml",         FmtVoiceXML),
     ("slf",          FmtSLF),
     ("regexp",       FmtRegExp),
     ("fa",           FmtFA)]

instance Show OutputFormat where
    show = lookupShow outputFormats

instance Read OutputFormat where
    readsPrec = lookupReadsPrec outputFormats

optimizationPackages :: [(String, Set Optimization)]
optimizationPackages = 
    [("all",         Set.fromList [OptStem,OptCSE,OptExpand,OptParametrize]),
     ("values",      Set.fromList [OptStem,OptCSE,OptExpand]),
     ("noexpand",    Set.fromList [OptStem,OptCSE]),
     
     -- deprecated
     ("all_subs",    Set.fromList [OptStem,OptCSE,OptExpand,OptParametrize]),
     ("parametrize", Set.fromList [OptStem,OptCSE,OptExpand,OptParametrize]),
     ("none",        Set.fromList [OptStem,OptCSE,OptExpand])
    ]

cfgTransformNames :: [(String, CFGTransform)]
cfgTransformNames = 
    [("nolr",         CFGNoLR),
     ("regular",      CFGRegular),
     ("topdown",      CFGTopDownFilter),
     ("bottomup",     CFGBottomUpFilter),
     ("startcatonly", CFGStartCatOnly),
     ("merge",        CFGMergeIdentical),
     ("removecycles", CFGRemoveCycles)]

haskellOptionNames :: [(String, HaskellOption)]
haskellOptionNames =
    [("noprefix", HaskellNoPrefix),
     ("gadt",     HaskellGADT),
     ("lexical",  HaskellLexical)]

-- | This is for bacward compatibility. Since GHC 6.12 we
-- started using the native Unicode support in GHC but it
-- uses different names for the code pages.
renameEncoding :: String -> String
renameEncoding "utf8"                      = "UTF-8"
renameEncoding "latin1"                    = "CP1252"
renameEncoding ('c':'p':s) | all isDigit s = 'C':'P':s
renameEncoding s                           = s

lookupShow :: Eq a => [(String,a)] -> a -> String
lookupShow xs z = fromMaybe "lookupShow" $ lookup z [(y,x) | (x,y) <- xs]

lookupReadsPrec :: [(String,a)] -> Int -> ReadS a
lookupReadsPrec xs _ s = [(z,rest) | (x,rest) <- lex s, (y,z) <- xs, y == x]

onOff :: Monad m => (Bool -> m a) -> Bool -> ArgDescr (m a)
onOff f def = OptArg g "[on,off]"
  where g ma = maybe (return def) readOnOff ma >>= f
        readOnOff x = case map toLower x of
                        "on"  -> return True
                        "off" -> return False
                        _     -> fail $ "Expected [on,off], got: " ++ show x

readOutputFormat :: Monad m => String -> m OutputFormat
readOutputFormat s = 
    maybe (fail $ "Unknown output format: " ++ show s) return $ lookup s outputFormats

-- FIXME: this is a copy of the function in GF.Devel.UseIO.
splitInModuleSearchPath :: String -> [FilePath]
splitInModuleSearchPath s = case break isPathSep s of
  (f,_:cs) -> f : splitInModuleSearchPath cs
  (f,_)    -> [f]
  where
    isPathSep :: Char -> Bool
    isPathSep c = c == ':' || c == ';'

-- 
-- * Convenience functions for checking options
--

verbAtLeast :: Options -> Verbosity -> Bool
verbAtLeast opts v = flag optVerbosity opts >= v

dump :: Options -> Dump -> Bool
dump opts d = flag ((d `elem`) . optDump) opts

cfgTransform :: Options -> CFGTransform -> Bool
cfgTransform opts t = Set.member t (flag optCFGTransforms opts)

haskellOption :: Options -> HaskellOption -> Bool
haskellOption opts o = Set.member o (flag optHaskellOptions opts)

isLexicalCat :: Options -> String -> Bool
isLexicalCat opts c = Set.member c (flag optLexicalCats opts)

-- 
-- * Convenience functions for setting options
--

setOptimization :: Optimization -> Bool -> Options
setOptimization o b = modifyFlags (setOptimization' o b)

setOptimization' :: Optimization -> Bool -> Flags -> Flags
setOptimization' o b f = f { optOptimizations = toggle o b (optOptimizations f)}

setCFGTransform :: CFGTransform -> Bool -> Options
setCFGTransform t b = modifyFlags (setCFGTransform' t b)

setCFGTransform' :: CFGTransform -> Bool -> Flags -> Flags
setCFGTransform' t b f = f { optCFGTransforms = toggle t b (optCFGTransforms f) }

toggle :: Ord a => a -> Bool -> Set a -> Set a
toggle o True  = Set.insert o
toggle o False = Set.delete o

--
-- * General utilities
--

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing

toEnumBounded :: (Bounded a, Enum a, Ord a) => Int -> Maybe a
toEnumBounded i = let mi = minBound
                      ma = maxBound `asTypeOf` mi                      
                   in if i >= fromEnum mi && i <= fromEnum ma 
                        then Just (toEnum i `asTypeOf` mi)
                        else Nothing

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p s = case break p s of
                (l, _ : t@(_ : _)) -> l : splitBy p t
                (l, _) -> [l]

instance Functor OptDescr where
    fmap f (Option cs ss d s) = Option cs ss (fmap f d) s

instance Functor ArgDescr where
    fmap f (NoArg x)    = NoArg (f x)
    fmap f (ReqArg g s) = ReqArg (f . g) s
    fmap f (OptArg g s) = OptArg (f . g) s
