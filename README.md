






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Quarterly Labour Force Survey data cleaning <img src="man/figures/README-lfsclean-open.png" align="right" style="padding-left:10px;background-color:white;" width="100" height="100" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## Motivation

The motivation for `lfsclean` is to develop a set of standard functions
for processing raw data from the quarterly Labour Force Survey (LFS),
which is an ongoing survey of households representative of the UK
population primarily collecting demographic and labour market
information such as employment status, earnings, and hours of work.

The `lfsclean` package contains functions which read in the raw data
files, processes them into clean output variables, and combines all data
files into a single output data table. The functions also create
real-terms values for nominally valued monetary variables (earnings and
wages), allowing the user to select either the CPIH or RPI.

## Installation

You can install the latest version of `lfsclean` from GitHub with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/STAPM/lfsclean.git", 
  build_vignettes = FALSE
)
```

## Citation

Cite this package as:

Morris, D (2023). lfsclean: An R package for cleaning UK Quarterly
Labour Force Survey data. version \[x.x.x\]. University of Sheffield.
<https://doi.org/10.17605/OSF.IO/AHDNY>

## Projects

Some examples of projects making use of the `lfsclean` package are:

1.  [Input-Output
    modelling](https://gitlab.com/SPECTRUM_Sheffield/projects/input-output-modelling).
    Here the package is used to create full-time equivalent employment
    by sector and year to estimate the impacts of changing demand in
    different sectors on total employment.
