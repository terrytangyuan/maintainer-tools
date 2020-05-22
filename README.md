# maintainertools


**maintainertools** package provides a collection of useful functions for project maintainers, such as writing summaries on issues, pull requests, and preparing for releases for projects on GitHub.

## Installation

You can install the development version of **maintainertools** package from GitHub with:

```r
install.packages("remotes")
remotes::install_github("terrytangyuan/maintainer-tools")
```


## Example

This is a basic example which shows you how to generate a summary that can be included in release notes:

``` r
library(maintainertools)

repo <- "kubeflow/common"
from <- "2020-05-10"
until <- "2020-05-19"
labels_mapping <- list(
  "size/S" = "Small Changes",
  "size/L" = "Large Changes",
  "size/M" = "Medium Changes")

get_release_summary(repo, from, until, labels_mapping)
```

You can find an example release summmary [here](./tests/testthat/resources/example-release-notes.md).
