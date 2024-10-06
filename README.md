# Advent of Code

This repository is an archive for my [Advent of Code](https://adventofcode.com/) solutions in [Haskell](https://www.haskell.org/). Visit the various folders to view the solutions for a given year.

## Goals

I strive to write solutions with the following characteristics.

* **Correct**: I want my stars!
* **Purely functional**. The solutions should be pure functions that take the contents of the input file and output two strings that solve the two problem parts. The only `IO` is for reading the input file, printing the answers and timing the solutions. For convenience, I write all my solutions as a function `Text -> IO()` (rather than something like `Text -> (Text, Text)`). This is mainly to allow the possibility of debugging while solving the problems.
    * One exception to purity is the use of errors. Since the input files are of a rather specific format, parsing them purely functionally would introduce a lot of `Maybe`s into the code. If you try to run a solution on something that's _not_ an input file, you'll probably see a runtime error.
    * Other exceptions to purity can occur when a solution using only immutable data structures simply isn't feasible. Examples of this are days 15 and 23 of 2020. In these cases, the solution uses a mutable (and while we're at it, unboxed) vector in a State Thread.
* **Readable**. I do not strive to write solutions that are as short/compact as possible, but rather solutions with appropriate abtractions that will be intelligible to other people familiar with Haskell. (This includes myself in the future.)
* **Reasonably fast**: within the rule of taking at most 15 seconds on 10 year old hardware. I would like the solutions to complete in at most a few seconds on newer hardware.
* **General**. If someone else runs my solutions on their input, they should give the correct answers. Of course, I use patterns from the input that are needed to make the problem feasible (as happens sometimes in AoC). On the other hand, I prefer not to hardcode things that are clearly unique to my input.

## Setup

Each year consists of a cabal project with a `utils` library containing common functionality, and for each day xx, an executable `dayxx`. The `utils` library differs slightly between years, depending on the need for that year's puzzles. In order to run the solution for a given day:

```bash
cabal run dayxx /path/to/input/file
```

If you save all input files as `dayxx/input.txt`, the script `runSolutions.sh` runs all the solutions.

## Haskell

The solutions should be able to run with `ghc 9.4.8` and `cabal 3.10.3.0`. All solutions have been tested on a 64-bit system, where `Int` ranges from `-2^63` to `2^63-1`. On other systems, some solutions may not work due to overflows. In this case, replacing Int by Integer in appropriate places should do the trick.
