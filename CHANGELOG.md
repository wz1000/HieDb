# Revision history for hiedb

## 0.4.0.0 -- 2021-06-29

* Index module exports and add `getAllIndexedExports` and `getExportsForModule`

## 0.3.0.1 -- 2021-01-27

* Add additional sqlite indexes to prevent accidently quadratic behaviour while indexing

## 0.3.0.0 -- 2021-01-20

* Introduce `SourceFile` type
* Add `deleteMissingRealFiles` to garbage collect missing/deleted files
* Enforce `is_real => hs_src IS NOT NULL` constraint in database.
* Add option to show context for source spans
* Coloured output and other output improvements
* Garbage collection of typenames
* Added flag to reindex all files
* 'addRefsFrom' now returns a boolean indicating if the file was indexed or skipped

## 0.2.0.0 -- 2021-01-06

* Use fingerprints/hashes instead of modtimes to maintin database consistency
* Type references are only reported for bind sites
* Type references are computed for all files
* Total time taken to index is reported
* `search` is now called `findReferences`
* `findTypeRefs` has a similar type signature to `findReferences`

## 0.1.0.0 -- 2020-11-08

* First version.
