#' Repeated evaluations along one dimension
#'
#' Evaluate a function repeatedly, varying one argument at a time.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be varied when evaluating \code{fun}, where each 
#'   argument in \code{...} is a vector or list of values.
#' @param method The sensitivity analysis method to be used. Currently supports 
#'   one-factor-at-a-time ("OFAC") evaluation.
#' @param default.args the default values of arguments passed to \code{fun}.
#' @return A list of lists: The top-level names are the arguments in \code{...}
#'   and the bottom-level names are the names of each element in each argument 
#'   in \code{...}.
#'
#' @details If the elements of an argument in \code{...} are not named, the 
#'   values of the elements will used via \code{paste}.
#' @examples
#' eval_many(rnorm, mean=c(5,10), sd=c(2, 3), default.args=list(n=5))
#' eval_many(rnorm, mean=c(5,10), sd=c(2, 3), default.args=list(n=5, mean=20, sd=5))
#'
#' \dontrun{
#' formulas = list(y ~ x, y ~ x^2, y ~ x + z)
#' datasets = list(
#'   A = data.frame(x=1:10, y = 1:10 + rlnorm(10)),
#'   B = data.frame(x=1:10, y = 1:10 + rnorm(10, mean=2))
#' )
#' eval_many(lm, data=datasets, default.args=list(formula = formulas[[1]]))
#' eval_many(lm, formula=formulas, default.args=list(datasets$B))
#' }
#' @export
eval_many = function(fun, ..., method = "OFAC", default.args = list()){
  method = match.arg(method, c("OFAC"))
  # setup inputs
  pargs = list(...)
  for(n in names(pargs))
    if(is.null(names(pargs[[n]])))
      names(pargs[[n]]) = paste(pargs[[n]])
  # setup outputs
  ret = vector("list", length=length(pargs))
  names(ret) = names(pargs)
  for(n in names(ret)){
    ret[[n]] = vector("list", length=length(pargs[[n]]))
    names(ret[[n]]) = names(pargs[[n]])
  }
  # evaluate
  if(method == "OFAC"){
    errs = list()
    for(n in names(ret))
      for(v in names(ret[[n]])){
        args = default.args
        args[[n]] = pargs[[n]][[v]]
        tryCatch({
          ret[[n]][[v]] = do.call(fun, args)
        }, error=function(cond) {
          message("When evaluating ", n, " = ", v, ": ", cond)
        }, warning=function(cond) {
          message("When evaluating ", n, " = ", v, ": ", cond)
        })
    }
  }
  return(ret)  
}




#' Collate results of many evaluations
#'
#' Collate a list of lists of vectors or dataframes into one dataframe.
#'
#' @param l The list of lists of vectors or dataframes, 
#'   e.g. output of \code{eval_many}. Tries to coerce each element into a 
#'   dataframe with \code{as.data.frame} before collating.
#' @param reshape.fun A function for reshaping each element in \code{l} before
#'   collating. 
#' @param ... Other arguments to be passed to \code{reshape.fun}.
#' @param variable.name The name of the column used to store the variable names.
#' @param value.name The name of the column used to store the variable values.
#' @return A dataframe collated from \code{l}.
#'
#' @details Loops through each element in each list, ignoring \code{NULL} 
#'   elements. For complex elements, the argument \code{reshape.fun} can
#'   be used to extract information (see examples).
#' @examples
#' res = eval_many(rnorm, mean=c(5,10), sd=c(2, 3), default.args=list(n=5))
#'
#' collate_many(res)
#' collate_many(res, reshape.fun=t, variable.name="parameter", 
#'   value.name = "parameter.value")
#'
#' \dontrun{
#' datasets = list(
#'   A = data.frame(x=1:10, y = 1:10 + rlnorm(10)),
#'   B = data.frame(x=1:10, y = 1:10 + rnorm(10, mean=2))
#' )
#' res = eval_many(lm, data=datasets, default.args=list(formula = y ~ x))
#' collate_many(res, reshape.fun = function(x) t(x$coefficients))
#' }
#' @export
collate_many = function(l, reshape.fun=identity, ..., 
  variable.name="variable", value.name="value"){
  res = NULL
  for(n in names(l)){
    nres = NULL
    for(v in names(l[[n]])){
      if(!is.null(l[[n]][[v]])){
        vres = as.data.frame(reshape.fun(l[[n]][[v]], ...))
        vres[value.name] = v
        nres = rbind(nres, vres)
      }
    }
    nres[variable.name] = n
    res = rbind(res, nres)
  }
  return(res)
}
