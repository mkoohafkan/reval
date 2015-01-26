%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{quickstart}



Quickstart for reval
====================

Introduction
------------
This vignette will get you stated using `reval` by showing a moderately-complex 
example. Additional examples are available in the function documentation for
`eval_many`.

Task: Fitting linear models
---------------------------

For this example we will use the `ToothGrowth` dataset. This dataset provides 
time series of chick weights for 25 individual chicks on 4 diferent diets.
We will investigate whether chick weight is best explained by diet, time 
since birth, or both.


```r
data(ToothGrowth)
formulas = c(
    len ~ dose - 1,
    len ~ dose,
    len ~ I(dose^2)
)
```

We want to extract the coefficients of the linear model and the adjusted $R^2$ 
values. We could use a for loop to test these different models:


```r
results = vector("list", length=length(formulas))
for(i in seq(length(formulas))){
  out = lm(formulas[[i]], data=ToothGrowth)
  rsq = summary(out)$adj.r.squared
  results[[i]] = rsq
}
names(results) = formulas
```

Which gives us a named list of coefficients and $R^2$ values. We can use 
`reval` to do the same analysis with only one call to `eval_many`:


```r
rfun = function(x) data.frame(adj.r.squared = summary(x)$adj.r.squared)
kable(eval_many(lm, formula = formulas, default.args = list(data = ToothGrowth),
  method="set", reshape.fun = rfun))
```



| adj.r.squared|set                       |
|-------------:|:-------------------------|
|     0.9191635|formula = len ~ dose - 1  |
|     0.6381807|formula = len ~ dose      |
|     0.5701483|formula = len ~ I(dose^2) |

We use the `reshape.fun` argument to extract the information we need. `rfun` is 
a function to extract the data we want, in the format that we want it. Note 
that when reshape.fun returns a vector or single value, transposing it with 
`t()` usually leads to cleaner outputs. 

What if we wanted to do this same analysis, but for each individual chick in 
`ChickWeight`? To do it manually we would need to write a second loop to 
iterate through the individual chick data as well as the formulas. With 
`eval_many`, the process is much simpler.


```r
# prepare the data
datasets = lapply(sort(unique(ToothGrowth$supp)), 
  function(x) ToothGrowth[ToothGrowth$supp == x,])
names(datasets) = paste("supp ~", sort(unique(ToothGrowth$supp)))
kable(eval_many(lm, formula = formulas, data = datasets, method="permute", 
  reshape.fun = rfun, id.type="multi"))
```



| adj.r.squared|formula         |data      |
|-------------:|:---------------|:---------|
|     0.4566532|len ~ I(dose^2) |supp ~ OJ |
|     0.9558955|len ~ dose - 1  |supp ~ VC |
|     0.7436894|len ~ I(dose^2) |supp ~ VC |
|     0.8939512|len ~ dose - 1  |supp ~ OJ |
|     0.8012993|len ~ dose      |supp ~ VC |
|     0.5469659|len ~ dose      |supp ~ OJ |
