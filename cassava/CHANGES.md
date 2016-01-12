## Version 0.4.5.0

 * Fix defect in `Foldable(foldr)` implementation failing to skip
   unconvertable records (#102)

 * Documentation fixes

## Version 0.4.4.0

 * Added record instances for larger tuples.

 * Support attoparsec 0.13.

 * Add field instances for short bytestrings.

## Version 0.4.3.0

 * Documentation overhaul with more examples.

 * Add Data.Csv.Builder, a low-level bytestring builder API.

 * Add a high-level builder API to Data.Csv.Incremental.

 * Generalize the default FromNamedRecord/ToNamedRecord instances.

 * Improved support for deriving instances using GHC.Generics.

 * Added some control over quoting.

## Version 0.4.2.4

 * Support attoparsec 0.13.

## Version 0.4.2.3

 * Support GHC 7.10.

## Version 0.4.2.2

 * Support blaze-builder 0.4.

 * Make sure inlining doesn't prevent rules from firing.

 * Fix incorrect INLINE pragmas.

## Version 0.4.2.1

 * Support deepseq-1.4.

## Version 0.4.2.0

 * Minor performance improvements.

 * Add 8 and 9 tuple instances for From/ToRecord.

 * Support text-1.2.

## Version 0.4.1.0

 * Ignore whitespace when converting numeric fields.

 * Accept \r as a line terminator.

 * Support attoparsec-0.12.
