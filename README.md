# algorithm-practice

To build the package, install stack then simply run

`stack setup && stack build`

in the main repo directory. This may take some time to download necessary dependencies.

Test the correctness of a few sorting algorithms by running `stack test`.

The performance of various sorting algorithms can be checked by running `stack exec sort-benchmark`. A command line option `--output sort.html` can be provided to generate performance plots.
