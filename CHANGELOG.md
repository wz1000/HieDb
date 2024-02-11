# Revision history for hiedb

## 0.6.0.0 -- 2024-12-11

* Add index on column `unit` of table `mods`
* Add new table `imports` which indexes import statements
* Add new cli options that allow selectively skipping indexing of some things:
    `--skip-refs`              Skip refs table when indexing
    `--skip-decls`             Skip decls table when indexing
    `--skip-defs`              Skip defs table when indexing
    `--skip-exports`           Skip exports table when indexing
    `--skip-imports`           Skip imports table when indexing
    `--skip-types`             Skip types and typerefs table when indexing
    `--skip-typerefs`          Skip typerefs table when indexing
* Fix a bug where duplicate entries were inserted into `typerefs` table during indexing
* Fix a bug in `searchDef` query which was mistakenly not including ':' when searching by occurrence names

## 0.5.0.1 -- 2024-01-12

* Fix incorrect Show Symbol instance in 0.5.0.0

## 0.5.0.0 -- 2024-01-12

* Handle duplicate record fields in GHC 9.8 instead of crashing

## 0.4.4.0 -- 2023-11-13
* Add `--src-base-dir` option allowing for src file indexing in `mods`
* 9.8.1 support
* Add `lookupHieFileFromHash`
* Add `lookupPackage`
* Add `removeDependencySrcFiles`

## 0.4.3.0 -- 2023-03-13

* Support GHC 9.6
* Set sqlite pragma busy_timeout to 500ms to mitigate frequent failures on concurrent access

## 0.4.2.0 -- 2022-09-12

* Support GHC 9.4
* Add upper bounds for base and algebraic graphs

## 0.4.1.0 -- 2021-06-29

* Support GHC 9.2 (@fendor)
* Remove `ghc-api-compat` dependency (@fendor)

## 0.4.0.0 -- 2021-06-29

* Index module exports
* New queries `getAllIndexedExports`, `getExportsForModule`, and `findExporters`
* Support for ghc-9.0
* An new `addRefsFromLoaded_unsafe` to index a module with cleanup or transactional behaviour
* Include test data in source tarball
* Use terminal-size for printing in some cases, making verbose indexing faster in some cases

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
