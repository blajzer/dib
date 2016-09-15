# Introduction
Dib is a light-weight, forward build system embedded in Haskell.
Dib represents the build products as a chain of operations starting at the input.
Reverse build systems such as Make and Jam instead attempt to figure out the
operations to perform starting at the desired output and tracing back through
a set of rules to find the correct input file. Dib has no such notion of "rules"
and the general thought process for writing a build script answers the question
"I have these files, how do I build them into the thing I want?", versus a reverse
build system which answers (recursively) "I want this product, what files do I
need to use as input?".

# Building/Installing
Dib uses cabal for building and installing. Clone this repository and then do `cabal install`
to install it. You'll want to make sure your cabal binary directory is in your path for
easy access to the `dib` executable.

Run `cabal haddock` to build the documentation, which is guaranteed to be more
up-to-date than this README.

# Concepts
* __Target__ - The most granluar unit of a build. Represents a desired outcome:
  e.g. an executable, a folder of files, etc... Contains `Stage`s, which do
  the actual work. Somewhat unfortunately, a `Target`'s name is its only identifier
  in the cache database, so debug/release and multiplatform `Target` variants
  should be named accordingly to prevent full-rebuilds when switching between them.
* __Stage__ - A portion of a pipeline for transforming input data into output data.
  These separate major portions of a pipeline: e.g. building source code into
  object files, linking object files into an executable, copying some data into place.
  `Target`s can have multiple `Stage`s, which are executed in sequence, the output
  of one is used as the input to the next.
* __Gatherer__ - Used to generate the initial input `SrcTransform`s for the first
  `Stage` of a `Target`.
* __SrcTransform__ - Represents a mapping from input to output. Comes in four varieties:
  `OneToOne`, `OneToMany`, `ManyToOne`, `ManyToMany`. Some examples: compiling a
  C source file into an object file initially begins as a `OneToOne`, but is converted
  into a `ManyToOne` through dependency scanning (adding the dependencies to the input
  exploits the internal timestamp database for free). Copying files from one location to
  another would just be a simple `OneToOne`. A tool that takes in one file and
  generates a bunch of output files would use `OneToMany`.

# Getting Started
Dib is both a library and an executable. The executable exists to cause a rebuild
of the build script whenever it changes, and also as a convenience for invoking both
the build and execution correctly. It's recommended that it be used for everything
except extraordinary use cases. It can also generate an initial build script
through the use of `dib --init`. Run the dib executable with no options for more
information on the available templates.

The most trivial, do nothing build script looks like the following:

```
module Main where
import Dib

targets = []
main = dib targets
```

Refer to the documentation for the built-in builders.

A build script is expected to declare the available `Target`s and then pass them
to the `dib` function. Only the top-level `Target`s need to be passed to `dib`;
it will scrape out the dependencies from there. The first `Target` in the list
is the default `Target` to build if the dib executable is called with no arguments.

## Additional Information
Arguments can be passed on the command line to the dib executable. These can be
retrieved in the build with `getArgDict`. The user is also free to use environment variables
as parameter input.

The invocation might look like the following: `dib <target> <key>=<value> <key>=<value> ...`.
Please note that there are no spaces between the keys and values. Quoted strings are
untested and unlikely to work correctly. The `Target` is optional, and can appear
anywhere in the command. If no `Target` is specified, the default will be used.
