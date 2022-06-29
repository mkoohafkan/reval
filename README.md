# reval: repeated function evaluation for sensitivity analysis
<!-- badges: start -->
[![R-CMD-check](https://github.com/mkoohafkan/reval/workflows/R-CMD-check/badge.svg)](https://github.com/mkoohafkan/reval/actions)
<!-- badges: end -->

`reval` is a small utility package for repeated function evaluations
in R. It is designed to simplify scenario testing and sensitivity
analysis. The package supports one-factor-at-a-time (OFAT) sensitivity
analysis, evaluation of parameter sets and (sampled) parameter
permutation. The actual evaluation is done using
`furrr:::future_pmap()` to take advantage of parallel processing
functionality as specified via `future::plan()`.
