#' Repeated evaluations (Backwards Compatibility)
#'
#' Evaluate a function repeatedly across argument sets or permutations. This
#' function is included for backwards compatibility with prior
#' versions of `package:reval` and will be defunct in future releases.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be varied when evaluating \code{fun}, where each 
#'   argument in '\code{...}' is a (named) vector or list of values. Lists of
#'   multi-value objects (e.g. data.frames) should be named explicitly and may 
#'   otherwise produce unexpected or incorrect names.
#' @param method The sensitivity analysis method to be used. Can be either 
#'   one-factor-at-a-time ("ofat") evaluation, evaluation of parameter sets
#'   ("set"), or (sampled) permutations of parameter sets ("permute"). When
#'   \code{method = "ofat"}, the first element of each argument in '\code{...}'
#'   is assumed to be the "default" value of that argument.
#' @param sample If \code{method = "permute"}, the number of parameter 
#'   permutations to evaluate (sampling without replacement). 
#'   If \code{sample < 1} (the default) then all possible permutations are 
#'   evaluated.
#' @param default.args Named list of additional arguments passed to \code{fun}.
#' @param collate Whether to collate the results or not. If \code{TRUE}, output 
#'   elements will be coerced into data.frames using \code{as.data.frame}.
#'   Otherwise, the raw outputs will be returned as a named list.
#' @param collate.id If \code{collate = TRUE}, the method used to 
#'   store the evaluation identifiers. If \code{collate.id = "single"}, a 
#'   single column named 'id' is used. If \code{collate.id = "multi"}, 
#'   one column is created for each argument in '\code{...}', e.g. 
#'   'arg1', 'arg2', etc. 
#' @param collate.prepend A character string prepended to the identifier 
#'   column. If \code{collate.id = "single"}, the identifier column will be
#'   named \code{<collate.prepend>id}. If \code{collate.id = "multi"}, 
#'   identifier columns will be named as \code{<collate.prepend><arg>} where
#'   \code{arg} is an element of '\code{...}'.
#' @param collate.fun If \code{collate = TRUE}, an optional function 
#'   for reshaping the output of each evaluation prior to coercing and 
#'   collating the outputs. 
#' @param clusters Number of clusters to use for parallel (multisession)
#'   processing. Default is 1 (serial computation).
#' @param packages Not used, included for backwards compatibility.
#'
#' @return If \code{collate = TRUE}, a data.frame. Otherwise, a named list.
#'
#' @importFrom future plan sequential multisession
#' @importFrom purrr pmap_chr map_dfr imap
#' @importFrom furrr future_pmap
#' @importFrom dplyr left_join select
#' @importFrom rlang .data
evalmany = function(fun, ..., method = c("ofat", "permute", "set"), 
  sample = 0L, default.args = list(), collate = TRUE, 
  collate.id = c("single", "multi"), collate.prepend = "", 
  collate.fun = identity, clusters = 1L, packages = NULL) {
  .Deprecated(msg = paste(
    "function 'evalmany' is deprecated in favor of a new workflow.",
    "See the Quickstart vignette for more information.")
  )
  
  args = list(...)
  pargs = c(args, default.args)

  # argument checks
  if (any(names(pargs) == "")) {
    stop("All arguments to 'fun' must be named")
  }
  if (any(grepl(";", names(pargs)))) {
    stop('argument names must not contain ";".')
  }

  # make argument list
  method = match.arg(method, c("ofat", "permute", "set"))
  arg.table = switch(method,
    "ofat" = do.call(args_ofat, pargs),
    "set" = do.call(args_set, pargs),
    "permute" = do.call(args_permute, c(pargs, ".n" = sample))
  )
  
  # make plan
  current.plan = plan()
  on.exit(plan(current.plan), add = TRUE)
  if (clusters > 1L) {
    plan(multisession, workers = clusters)
  } else {
    plan(sequential)
  }
  result = future_pmap(arg.table, fun)
  result.names = pmap_chr(imap(args, ~ paste(.y, "=", .x)),
    paste, sep = " ; ")
  names(result) = result.names

  if (!collate) {
    return(result)
  # collation
  } else {
    collate.id = match.arg(collate.id, c("single", "multi"))
    id.name = paste0(collate.prepend, "id")
    result = map_dfr(result, collate.fun, .id = id.name)
    if (collate.id == "single") {
      return(result)
    } else {
      arg.table[id.name] = result.names
      result = left_join(arg.table, result, by = id.name)
      return(select(result, - .data[[id.name]]))
    }
  }
}
