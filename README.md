# The codependent package :bee: :blossom:
An R package for estimating affiliate species richness based on power law scaling with host diversity, using rarefaction on bipartite species association networks. Because [some things](https://www.manrepeller.com/2018/05/gilmore-girls-and-codependency.html) just go together.

Use the function `copredict` to extrapolate power law curves out to a higher value. Use `copredict.ci` to fit a series of models to only half of the total curve, and see what happens (for an overestimated confidence bound).

Author
----------

Colin J. Carlson (ccarlson@sesync.org)

Installing the package
----------------------

Install directly from Github:

``` {r, setup, echo = FALSE, message = FALSE}
knitr::opts_chunk$set(echo = FALSE,
  comment = "#",
  tidy = FALSE,
  error = FALSE,
  fig.width = 7,
  fig.height = 4.5,
  fig.path = 'vignettes/readme-',
  cache=FALSE)

# If you don't have devtools:
# install.packages("devtools")

devtools::install_github("cjcarlson/codependent")
```

``` {r dataset, message=FALSE}
# Load the package

library(codependent)

data(rob1929)
head(rob1929)
```

```{r chunk_name, echo=FALSE}
x <- rnorm(100)
y <- 2*x + rnorm(100)
cor(x, y)
```
