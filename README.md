# reval: repeated function evaluation for sensitivity analysis
<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/reval)](http://cran.r-project.org/package=reval)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/mkoohafkan/reval/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mkoohafkan/reval/actions/workflows/R-CMD-check.yaml)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/reval)](http://cran.r-project.org/package=reval)
<!-- badges: end -->

`reval` is a small utility package for repeated function evaluations
in R. It is designed to simplify scenario testing and sensitivity
analysis. The package supports one-factor-at-a-time (OFAT) sensitivity
analysis, evaluation of parameter sets and (sampled) parameter
permutation. The actual evaluation is done using
`furrr::future_pmap()` to take advantage of parallel processing
functionality as specified via `future::plan()`.
