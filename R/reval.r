
# eval_permutate = function(fun, pargs, default.args, sample=TRUE, sample.size){}
# collate_perumate = function(l, reshape.fun, id.name){}

eval_ofac = function(fun, pargs, default.args){
  # setup inputs
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
  return(ret)  
}

eval_set = function(fun, pargs, default.args){
  expand.arguments <- function(...){
    dotList <- list(...)
    max.length <- max(sapply(dotList, length))
    lapply(dotList, rep, length=max.length)
  }
  # setup inputs
  setpargs = do.call(expand.arguments, pargs) # recycle values for sets  
  for(n in names(setpargs))
    if(is.null(names(setpargs[[n]])))
      names(setpargs[[n]]) = paste(setpargs[[n]])
  numsets = length(setpargs[[1]])
  numpargs = length(setpargs)
  # setup outputs
  n = vector("list", length=numsets)
  for(i in seq(numsets))
    n[[i]] = vector("list", length=numpargs)
  setnames = rep(NA, numsets)
  for(j in seq(numsets)){
    for(i in seq(numpargs))
      n[[j]][[i]] = paste(names(setpargs)[i], names(setpargs[[i]])[j], sep=" = ")
    setnames[j] = paste(n[[j]], collapse = " ; ")
    }
  ret = vector("list", length=numsets)
  names(ret) = setnames
  # evaluate
  for(i in seq(numsets)){
   args = default.args
     for(p in names(setpargs))
       args[[p]] = setpargs[[p]][[i]]
    tryCatch({
      ret[[i]] = do.call(fun, args)
    }, error=function(cond) {
      message("When evaluating ", setnames[i], ": ", cond)
    }, warning=function(cond) {
      message("When evaluating ", setnames[i], ": ", cond)
    })
  }
  return(ret)
}

collate_set = function(l, reshape.fun, id.name){
  res = NULL
  for(n in names(l))
    if(!is.null(l[[n]])){
      nres = as.data.frame(reshape.fun(l[[n]]))
      nres[id.name] = n
      res = rbind(res, nres)
    }
  res
}

collate_ofac = function(l, reshape.fun, id.name){
  res = NULL
  for(n in names(l)){
    nres = NULL
    for(v in names(l[[n]])){
      if(!is.null(l[[n]][[v]])){
        vres = as.data.frame(reshape.fun(l[[n]][[v]]))
        vres[id.name] = paste(n, "=", v)
        nres = rbind(nres, vres)
      }
    }
    res = rbind(res, nres)
  }
  return(res)
}

#' Repeated evaluations along one dimension
#'
#' Evaluate a function repeatedly, varying one argument at a time.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be varied when evaluating \code{fun}, where each 
#'   argument in \code{...} is a vector or list of values.
#' @param method The sensitivity analysis method to be used. Currently supports 
#'   one-factor-at-a-time ("ofac") evaluation and evaluation of parameter sets
#'   ("set").
#' @param default.args the default values of arguments passed to \code{fun}.
#'   for \code{method = "ofac"}, must include a default value for each argument
#'   in \code{...}.
#' @param collate Whether to collate the results or not. If TRUE, output 
#'   elements will be coerced into data.frames using \code{as.data.frame}.
#' @param reshape.fun A function for reshaping the output of each evaluation
#'   prior to coercing and collating the outputs. 
#' @param id.name The name of the column used to store the evaluation 
#'   identifiers. Defaults to the value of \code{method}.
#'
#' @return If \code{collate = TRUE}, a data.frame. Otherwise, a list structure 
#'   depending on the value of \code{method}:
#'   \item{"ofac"}{A list of lists: The top-level names are 
#'     the arguments in \code{...} and the bottom-level names are the names of 
#'     each element of that argument. If the elements of an argument in 
#'     \code{...} are not named, the values of the elements will used via 
#'     \code{paste}.}
#'   \item{"set"}{A simple list.}
#'
#' @examples
#' eval_many(rnorm, mean = c(5, 9), sd = c(2, 3), default.args = list(n = 5))
#' eval_many(rnorm, mean = c(5, 9), sd = c(2, 3), reshape.fun = t, 
#'   default.args = list(n = 5, mean = 0, sd = 5))
#' # vector recyclying
#' eval_many(rnorm, mean = c(0, 5, 3), sd = c(0, 1), method = "set", 
#'   default.args = list(n = 5), reshape.fun = t, id.name = "set")
#' 
#' \donttest{\dontrun{
#' formulas = list(y ~ x, y ~ x^2, y ~ x + z)   
#' datasets = list(
#'   A = data.frame(x=1:10, y = 1:10 + rlnorm(10)),
#'   B = data.frame(x=1:10, y = 1:10 + rnorm(10, mean = 2))
#' )
#'
#' # raw output
#' eval_many(lm, formula = formulas, data = datasets[1], method = "set", 
#'   collate = FALSE)
#'
#' # data extraction
#' eval_many(lm, formula = formulas, data = datasets[1], method = "set", 
#'   reshape.fun = function(x) t(x$coefficients))
#' eval_many(lm, formula = formulas, data = datasets, method = "ofac", 
#'   default.args = list(formula = formulas[[1]], data = datasets[[1]]), 
#'   reshape.fun = function(x) t(x$coefficients))
#' }}
#' @export
eval_many = function(fun, ..., method = "ofac", default.args = list(), 
  collate = TRUE, reshape.fun = identity, id.name){
  method = match.arg(method, c("ofac", "set"))
  if(missing(id.name))
    id.name = method
  pargs = list(...)
  if(method == "ofac")
    res = eval_ofac(fun, pargs, default.args)
  else if(method == "set")
    res = eval_set(fun, pargs, default.args)
  else
    stop("how did you get here...?")
  # format outputs
  if(collate == FALSE)
    res
  else
    if(method == "ofac")
      collate_ofac(res, reshape.fun, id.name)
    else if(method == "set")
      collate_set(res, reshape.fun, id.name)
    else
      stop("you shouldn't be here...")
}
