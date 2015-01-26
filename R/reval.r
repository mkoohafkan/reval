eval_set = function(fun, pargs, default.args){
  recycle.grid <- function(...){
    dotList <- list(...)
    max.length <- max(sapply(dotList, length))
    lapply(dotList, rep, length=max.length)
  }
  # setup inputs
  setpargs = do.call(recycle.grid, pargs) # recycle values for sets  
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

eval_permute = function(fun, pargs, default.args, size){
  arg.grid = expand.grid(pargs)
  if(is.na(size))
    size = nrow(arg.grid)  
  permargs = as.list(arg.grid[sample(seq(nrow(arg.grid)), size = size),])
  attributes(permargs) <- NULL 
  names(permargs) = names(arg.grid)
  eval_set(fun, permargs, default.args)
}

collate_set = function(l, reshape.fun, collate.id){
  id.name = "set"
  res = NULL
  for(n in names(l))
    if(!is.null(l[[n]])){
      nres = as.data.frame(reshape.fun(l[[n]]))
      nres[id.name] = n
      res = rbind(res, nres)
    }
  if(collate.id == "single")
    res
  else
    parse_set(res, id.name)
}

eval_ofac = function(fun, pargs, default.args){
  base.args = lapply(pargs, function(x) x[1])
  ret = list()
  for(p in names(pargs)){
    thisargs = base.args
    thisargs[[p]] = pargs[[p]]
    ret = append(ret, eval_set(fun, thisargs, default.args))
  }
  return(ret)
}

parse_set = function(df, n){
  x = df[[n]]
  l = NULL
  variables = strsplit(x, " ; ") 
  for(i in seq(length(variables))){
    value = strsplit(variables[[i]], " = ")
    v = sapply(value, function(x) x[2])
    names(v) = sapply(value, function(x) x[1])
    l = rbind(l, v)
  }
  rownames(l) = NULL
  out = cbind(df, l)
  out[[n]] = NULL
  out
}


#' Repeated evaluations
#'
#' Evaluate a function repeatedly across argument sets or permutations.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be varied when evaluating \code{fun}, where each 
#'   argument in \code{...} is a vector or list of values. 
#' @param method The sensitivity analysis method to be used. Can be either 
#'   one-factor-at-a-time ("ofac") evaluation, evaluation of parameter sets
#'   ("set"), or (sampled) permutations of parameter sets ("permute"). When
#'   \code{method = "ofac"}, the first element of each argument in \code{...}
#'   is assumed to be the "default" value of that argument.
#' @param sample If \code{method = "permute"}, the number of parameter 
#'   permutations to evaluate. If NA, all permutations will be evaluated.
#' @param default.args the default values of arguments passed to \code{fun}.
#' @param collate Whether to collate the results or not. If TRUE, output 
#'   elements will be coerced into data.frames using \code{as.data.frame}.
#' @param reshape.fun A function for reshaping the output of each evaluation
#'   prior to coercing and collating the outputs. 
#' @param collate.id Only used when \code{collate = TRUE}. The method used to 
#'   store the evaluation identifiers. If \code{collate.id = "single"}, a single 
#'   column column is used. If \code{collate.id = "multi"}, one column is created 
#'   for each argument in \code{...}. \code{collate.id = "multi"} can at times 
#'   simplify data exploration and visualization.
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
#' set.seed(1459)
#' eval_many(rnorm, mean = c(5, 9), sd = c(2, 3), default.args = list(n = 5))
#' eval_many(rnorm, mean = c(5, 9), sd = c(2, 3), default.args = list(n = 5),
#'   reshape.fun = t, collate.id = "multi")
#' eval_many(rnorm, mean=seq(20), sd = seq(1, 4, by = 0.1), 
#'  default.args = list(n = 5), method = "permute", sample = 10,
#'   reshape.fun = t)
#'
#' # vector recycling
#' eval_many(rnorm, mean = c(0, 5, 3), sd = c(0, 1), method = "set", 
#'   default.args = list(n = 5), reshape.fun = t, collate.id = "multi")
#' 
#' \donttest{\dontrun{
#' formulas = list(y ~ x, y ~ x^2, y ~ x + z)   
#' datasets = list(
#'   A = data.frame(x=1:10, y = 1:10 + rnorm(10)),
#'   B = data.frame(x=1:10, y = 1:10 + rnorm(10, mean = 2))
#' )
#'
#' # raw output
#' eval_many(lm, formula = formulas, data = datasets[1], method = "set", 
#'   collate = FALSE)
#'
#' # data extraction
#' eval_many(lm, formula = formulas, data = datasets, method = "permute",
#'   reshape.fun = function(x) t(x$coefficients), collate.id = "multi")
#' }}
#' @export
eval_many = function(fun, ..., method = c("ofac", "permute", "set"), 
  sample = NA, default.args = list(), collate = TRUE, 
  reshape.fun = identity, collate.id = c("single", "multi")){
  method = match.arg(method, c("ofac", "permute", "set"))
  collate.id = match.arg(collate.id, c("single", "multi"))
  pargs = list(...)
  if(method == "ofac")
    res = eval_ofac(fun, pargs, default.args)
  else if(method == "set")
    res = eval_set(fun, pargs, default.args)
  else
    res = eval_permute(fun, pargs, default.args, size = sample)
  # format outputs
  if(collate == FALSE)
    res
  else
    collate_set(res, reshape.fun, collate.id)
}
