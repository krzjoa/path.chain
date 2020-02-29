
<!-- README.md is generated from README.Rmd. Please edit that file -->

# path.chain <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Documentation](https://img.shields.io/badge/documentation-path.chain-orange.svg?colorB=E91E63)](http://krzjoa.github.io/path.chain/)
[![Travis build
status](https://travis-ci.org/krzjoa/path.chain.svg?branch=master)](https://travis-ci.org/krzjoa/path.chain)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/krzjoa/path.chain?branch=master&svg=true)](https://ci.appveyor.com/project/krzjoa/path.chain)
[![Buy hex
stciker](https://img.shields.io/badge/buy%20hex-path.chain-green)](http://www.redbubble.com/people/krzjoa/works/45140988-path-chain-r-package-hex-sticker?p=sticker&asc=u)
<!-- badges: end -->

> Concise structure for chainable paths

## Installation

``` r
# install.packages("devtools")
devtools::install_github("krzjoa/path.chain")
```

## Example

If you are using RStudio, you know that among many excellent features of
this IDE the **path autocompletion**. However, you can also meet
situations, when that may be not enough. Most of all, I mean bigger
projects, where you are storing some complex file structure in
**config**.

``` r
library(path.chain)
```
