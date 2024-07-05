Changelog
=========

Unreleased
----------

*   `HasNonEmpty` instances for `()`, `All`, `Any`, and pairs where both sides'
	types have non empty.

Version 0.3.4.x
---------------

* **0.3.4.0**: `ToJSON` and `FromJSON` instances (*August 4, 2020*)
* **0.3.4.1**: `Ord` instance to `NESeq` (@mitchelwrosen) (*August 22, 2020*)
* **0.3.4.2**: Compatibility with GHC 9 (@andremarianiello) (*August 25, 2021*)
* **0.3.4.3**: (*August 25, 2021*)
    * Fix `intersperse` for singleton non-empty sequences. (@eddiemundo)
    * Fix `deleteMax` for singleton containers.
* **0.3.4.4**: (*September 25, 2021*)
    * `Alt` instances for `NEMap` and `NEIntMap`
    * `Invariant` instance for `NEMap`, `NEIntMap`, and `NESeq`.
* **0.3.4.5**: Future-proof against Prelude exporting `foldl'` (@Bodgrim) (*December 6, 2023*)

Version 0.3.3.0
---------------

*December 3, 2019*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.3.3.0>

*   Add `overNonEmpty` and `onNonEmpty` in *Data.Containers.NonEmpty*.

Version 0.3.1.0
---------------

*October 21, 2019*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.3.3.0>

*   Add `HasNonEmpty` instance for *nonempty-vector*
*   Changed `splitLookup` to use `These` instead of a tuple of `Maybe`s.

Version 0.3.1.0
---------------

*June 13, 2019*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.3.1.0>

*   Add `absurdNEMap` to *Data.Map.NonEmpty*.  This is the only type that would
    benefit from such a specialized function, whereas all other types would do
    just as well with `absurd . fold1 :: Foldable1 f => f Void -> a`.

Version 0.3.0.0
---------------

*June 10, 2019*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.3.0.0>

*   Switch back from *data-or* to *these*, due to changes in the organization
    of *these* that get rid of the high dependency footprint.

Version 0.2.0.0
---------------

*May 14, 2019*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.2.0.0>

*   ([#2][]) Switch from *these* to *data-or*, for lighter dependency footprint.  Much
    thanks to @fosskers for putting in the heavy work.

[#2]: https://github.com/mstksg/nonempty-containers/pull/2

Version 0.1.1.0
---------------

*December 8, 2018*

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.1.1.0>

*   `Comonad` instances added for `Map k` and `IntMap`, based on [Faucelme's
    suggestion][comonad]

[comonad]: https://www.reddit.com/r/haskell/comments/a1qjcy/nonemptycontainers_nonempty_variants_of/eat5r4h/

Version 0.1.0.0
---------------

<https://github.com/mstksg/nonempty-containers/releases/tag/v0.1.0.0>

*   Initial release
