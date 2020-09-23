# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

New features:

Bugfixes:

Other improvements:

## [v5.0.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v5.0.1) - 2019-10-16

* Fix CodePoint.anyChar parser, so that it fails rather than splitting up surrogate pairs (#46, @rintcius)

## [v5.0.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v5.0.0) - 2018-07-27

Splits CodePoint and CodeUnit functions into separate namespaces #44

## [v4.0.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v4.0.1) - 2018-06-23

- Adds metadata including contributor guidelines
- Pushes latest release to Pursuit

## [v4.0.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v4.0.0) - 2018-05-31

Updates for 0.12

**Breaking Changes:**

Changed `many1`, `sepBy1`, `sepEndBy1`, `endBy1`, and `many1Till` to return `NonEmptyList`

## [v3.1.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v3.1.0) - 2017-11-17

Add `Semigroup` instance (@dwhitney)

## [v3.0.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v3.0.1) - 2017-04-24

Fix overflow caused by manyTill implementation (@justinwoo)

## [v3.0.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v3.0.0) - 2017-04-02

Updates for 0.11.1

## [v2.2.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v2.2.0) - 2017-03-13

Add `many1Till` (@newlandsvalley)

## [v2.1.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v2.1.0) - 2017-03-01

Add `regex` combinator (@newlandsvalley)

## [v2.0.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v2.0.1) - 2017-02-16

Avoid `Discard` constraints (@mlang)

## [v2.0.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v2.0.0) - 2016-10-28

- Updated dependencies for PureScript 0.10
- `many` is now stack safe (@nwolverson)

## [v1.0.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v1.0.1) - 2016-06-18

Make lowerCaseChar, upperCaseChar rewind on failure (@romansergey)

## [v1.0.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v1.0.0) - 2016-06-03

- Updates for 1.0 core libraries
- Add `MonadRec` instance

## [v0.6.7](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.7) - 2016-02-04

Fix warnings (@jonsterling)

## [v0.6.6](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.6) - 2016-01-26

Remove use of regexes (@libscott)

## [v0.6.5](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.5) - 2016-01-21

Add some new parsers for various character classes. Fix `whitespace`.

## [v0.6.4](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.4) - 2016-01-11

Make package dependencies explicit (@chrisdotcode)

## [v0.6.3](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.3) - 2015-10-18

Fix an issue in `whiteSpace` (@sharkdp)

## [v0.6.2](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.2) - 2015-10-14

## [v0.6.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.1) - 2015-10-14

Added `satisfy`, `char`, `whiteSpace`, `skipSpaces`, `oneOf`, `noneOf`, and `Eq` instance (@Thimoteus)

## [v0.6.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.6.0) - 2015-09-23

Bump versions (@nwolverson)

## [v0.5.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.5.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.5.0-rc.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.5.0-rc.1) - 2015-06-19

Release for compiler version 0.7.

## [v0.4.1](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.4.1) - 2015-04-29

- Added `lookAhead` and `manyTill` combinators
- Added `anyDigit` parser

## [v0.4.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.4.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.3.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.3.0) - 2015-01-10

- Updated dependencies (@garyb)

## [v0.2.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.2.0) - 2014-10-27

- Update for addition of `Data.Char` (@garyb)

## [v0.1.0](https://github.com/purescript-contrib/purescript-string-parsers/releases/tag/v0.1.0) - 2014-08-11

Initial semver release.
