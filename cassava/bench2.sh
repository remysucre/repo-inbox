cp Data/Csv/bangEnc.hs Data/Csv/Encode.hs && cp Data/Csv/bangPar.hs Data/Csv/Parser.hs ;
cabal bench > bang.log; 
cp Data/Csv/nobangEnc.hs Data/Csv/Encode.hs && cp Data/Csv/nobangPar.hs Data/Csv/Parser.hs ;
cabal bench > nobang.log
