# Changelog

## Version 1.0.0

* Incompatible change: New DB format. No migration is supported, you have to
  remove an old DB (`~/.local/share/pore-align`). Now descriptor matches are
  also stored in the DB.
* Improvement: A new option `--cache-only-ref` which disables caching of source image
  descriptors.
* Improvement: RMSD is reported as a fit error.
* Improvement: More sane defaults.

## Version 0.11.2

* Fix CI script for building Ubuntu package

## Version 0.11.1

* Incompatible change: LMDB 1.0 is now required.
* Incompatible change: `soil-align` was renamed to `pore-align` (a stange
  decision).
* Improvement: Two new parameters `--ref-workspace-side` and
  `--src-workspace-side` to control workspace sides for source and reference
  images independently. The old option `--workspace-side` is still there but has
  lower priority.

## Version 0.11.0

* Improvement: Add support for multipage TIFF images (can be used for input and
  output).
* Improvement: Build a package for the latest Ubuntu LTS. This is experimental
  and completely untested (I only verified that `soil-aligns` correctly prints
  help when invoked without arguments). You better build from sources.

## Version 0.10

* Improvement: Update documentation
* Improvement: Do not print backtrace when `C-c` is pressed
* Bug fix: fix a crash in rare situations when there are too few inliers.

## Version 0.9

* Improvement: Descriptor matching is now done via very fast exact bruteforce
  algorithm with the help of faiss[^1] library. Dependency on python is removed
  entirely.
* Improvement: RANSAC algorithm was parallelized.
* Improvement: Soil-align does not build and require any wrapper libraries
  anymore and can be distibuted as a single binary executable (of course, there
  are some "external" dependencies like lmdb, faiss and SIFT3D) which can mostly
  be found in the OS' package manager.
* Incompatible change: entzauberte-matrices was updated to v0.3. On some
  bullshit linux distors (Ubuntu-based) this requires liblapacke.so library to
  be installed. Normally, usual OpenBLAS installation is enough.

[^1]: Принадлежит компании Meta, признанной в РФ экстремистской
    организацией. Осуждаем её и не поддерживаем!

## Version 0.8

* Incompatible change: `--min-dog` parameter was removed from the command line
  tool. The default value suffices for all cases.
* Incompatible change: SQLite database was replaced by LMDB. Users should delete
  stale files by themselves.
* Incompatible change: `--model` parameter was removed (see below).
* Improvement: Add an option `-b` to control background intensity in the output.
* Improvement: Rotation in the rigid transform model can be constrained.
* Improvement: Add `-s` flag and `--rotation-constraint` option to the CLI to
  provide a fine-grained control of the model's constraints.

## Version 0.7

* Improvement: Log dimensionality of the compressed descriptor space.
* Improvement: A new transform model: rigid + uniform scaling. Can be selected
  with `--model rigid+scaling` in the CLI.
* Improvement: `apply-transform` now has the `:background` argument which
  specifies the value to be used with out-of-bounds array accesses.
* Improvement: Allow building on systems which do not have `python-config`
  alias.

## Version 0.6

* Incompatible change: `affine-transform` is replaced with
  `rigid-transform`. The latter function finds a rigid transform (rotation +
  translation) from one set of points to another, which excludes scaling and
  shearing.
* Incompatible change: Remove the `:min-inliers` argument from
  `rigid-transform`. Now the first priority for a search for a model is to
  maximize the number of inliers.
* Incompatible change: Use our fork of SIFT3D with an improved API and a number
  of bug fixes.
* Improvement: Now only at most 300000 strongest keypoints are processed. This
  also guarantees that keypoints can be stored in a BLOB object of a SQLite
  database.
* Incompatible change: Soil-align now uses entzauberte-matrices instead of
  MAGICL. This results in OpenBLAS being an additional dependency.

## Version 0.5

* Improvement: Search for matches in the PCA space of the reference image which
  is of lower dimensionality than the descriptor space.
* Bug fix: Produce a comprehensible error if the number of descriptors is too
  small.
* Bug fix: Avoid an infinite loop if the number of matches is less then a
  minimal number of matches to find a transform matrix.

## Version 0.4

* Incompatible change: The default parameter of `match-descriptors` is changed
  to `1.2`.
* Improvement: `apply-transform` is parallelized.

## Version 0.3

* Improvement: Apply PCA analysis to descriptors in order to save space in the
  database.
* Incompatible change: Bruteforce matching of descriptors was removed.
* Incompatible change: Another database format. You must to manually remove
  older database file(s).

## Version 0.2

* Incompatible change: the output image is stored as an array of octets
  ('uint8') in the cli tool and `apply-transform` returns an array of type
  `(simple-array (unsigned-byte 8) 3)`.
* Incompatible change: `:min-inliers` default in `transform` is changed to `0.6`
  which improves the transformation finding algorithm in a number of cases.
* Improvement: Descriptors are stored in the database so they can be accessed
  later if they are needed. Currently, the database needs to be removed manually
  if it is not needed.
* Improvement: Add an option `-t` to the CLI tool which selects a number of
  working threads.
* Improvement: Add an option `-w` to the CLI tool which selects a working area.
* Improvement: The CLI tool now supports raw "format".

## Version 0.1

Initial version. Basic image alignment using 3D SIFT + pynndescent + RANSAC
