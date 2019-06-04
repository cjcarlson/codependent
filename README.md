# :bee: codependent :blossom:
## An R package for network-based estimation of affiliate species richness
An R package for estimating affiliate species richness based on power law scaling with host diversity, using rarefaction on bipartite species association networks. Because [some things](https://www.manrepeller.com/2018/05/gilmore-girls-and-codependency.html) just go together.

Use the function `copredict` to extrapolate power law curves out to a higher value. Use `copredict.ci` to fit a series of models to only half of the total curve, and see what happens (for an overestimated confidence bound).

Author
----------

Colin J. Carlson (cjc322@georgetown.edu)

Contributions by Tad Dallas (LSU) and Laura Ward Alexander (UC Berkeley)

Installing the package
----------------------

Install directly from Github:

``` {r, setup, echo = FALSE, message = FALSE}
# If you don't have devtools:
# install.packages("devtools")
devtools::install_github("cjcarlson/codependent")
```

What can you do with codependent?
----------------------

Load the package

``` {r dataset, message=FALSE}
library(codependent)
```

You can build and fit nested curves using `binera`, for bipartite network rarefaction:

``` {r dataset, message=FALSE}
data(rob1929)
head(rob1929)
binera(rob1929, iter=10, plots=TRUE)
```

You can EXTRAPOLATE to a higher number of hosts using `copredict`:

``` {r dataset, message=FALSE}
copredict(rob1929, n.indep=500, iter=10, plot=TRUE)
```

*Multigroup* extrapolation is coming soon!

Coextinction curves
----------------------

Coextinction curves in the style of Koh _et al._, 2004 are done with `coextinct`:

``` {r dataset, message=FALSE}
coextinct(rob1929, iter=10, plots=TRUE)
```

I'll be adding new functionality for that part of the package soon.
