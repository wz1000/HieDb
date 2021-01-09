# Revision history for hiedb

## 0.2.0.0 -- 2021-01-06

* Use fingerprints/hashes instead of modtimes to maintin database consistency
* Type references are only reported for bind sites
* Type references are computed for all files
* Total time taken to index is reported
* `search` is now called `findReferences`
* `findTypeRefs` has a similar type signature to `findReferences`

## 0.1.0.0 -- 2020-11-08

* First version.
