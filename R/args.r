#' Permutation Argument Set
#' 
#' Generate an argument table based on permutations.
#'
#' @param ... Named arguments to a function.
#' @param .n the number of argument permutations to evaluate
#'   (sampling without replacement). If missing, all possible
#'   permutations are returned.
#' @return A tibble of argument combinations.
#'
#' @examples
#' args_permute(x = 1:5, y = 1:2)
#' args_permute(x = 1:10, y = 1:10, z = 1:10, .n = 10)
#'
#' @importFrom purrr map2
#' @importFrom dplyr as_tibble
#' @export
args_permute = function(..., .n) {
  args = list(...)
  arg.idx = map(args, seq_along)
  idx.grid = expand.grid(arg.idx)
  if (missing(.n)) {
    idx.sample = seq(nrow(idx.grid))
  } else if (.n < 0 || is.na(.n) || length(.n) < 1) {
    stop("argument '.n' must non-negative integer")
  } else if (.n > nrow(idx.grid)) {
    stop("cannot take a sample larger than the population")
  } else {
    idx.sample = sample(seq(nrow(idx.grid)), size = .n)
  }
  permarg.idx = idx.grid[idx.sample,]
  as_tibble(map2(args, permarg.idx, ~ .x[.y]))
}


#' One Factor At a Time Argument Set
#' 
#' Generate an argument table based on OFAT.
#'
#' @inheritParams args_permute
#' @return A tibble of argument combinations.
#'
#' @examples
#' args_ofat(x = 1:5, y = 1:3)
#' args_ofat(x = 1:3, y = 1:3, z = 1:3)
#'
#' @importFrom purrr map2 map imap_dfr
#' @importFrom dplyr as_tibble select distinct bind_cols
#' @importFrom rlang .data set_names
#' @export
args_ofat = function(...) {
  args = list(...)
  arg.idx = map(args, seq_along)
  default.idx = map(arg.idx, ~ 1L)
  ofat.idx = imap_dfr(arg.idx, 
    ~ bind_cols(as_tibble(set_names(list(.x), .y)), 
      select(as_tibble(default.idx), -.data[[.y]])))
  ofat.idx = ofat.idx[names(args)]
  distinct(as_tibble(map2(args, ofat.idx, ~ .x[.y])))
}

#' Argument Set
#' 
#' Generate an argument table from a set of arguments, following
#' the standard rules for vector recycling in R.
#'
#' @inheritParams args_permute
#' @return A tibble of argument combinations.
#'
#' @examples
#' args_set(x = 1:10, y = 1:10)
#' args_set(x = 1:10, y = 1:5, z = 1:2)
#' # mismatched argument lengths will generate a warning
#' \dontrun{
#' args_set(x = 1:10, y = 1:3)
#' } 
#'
#' @importFrom dplyr as_tibble
#' @importFrom purrr map map_int map2
#' @export
args_set = function(...) {
  args = list(...)
  arg.idx = map(args, seq_along)
  arg.lengths = map_int(args, length)
  max.length = max(arg.lengths)
  if (any(max.length %% arg.lengths > 0L)) {
    warning("longer object length is not a ", 
      "multiple of shorter object length")
  }
  set.idx = map(arg.idx, rep, length.out = max.length)
  as_tibble(map2(args, set.idx, ~ .x[.y])) 
}
