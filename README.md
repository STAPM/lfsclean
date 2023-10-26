






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

### The Data

The Labour Force Survey (LFS) is a representative survey of the UK with
the aim of collecting detailed labour market information. It is used to
produce official statistics on unemployment. The survey has been
conducted on a quarterly basis since 1992 with data collected in waves.
Each sampled wave of participants remain in the survey for five
consecutive quarters, with five waves participating in the survey at any
one time and staggered such that each quarter one wave which has
participated for the previous five quarters is replaced with a new wave
of participants.

Note that there has been a long-running trend in the LFS data towards
increasing non-response to the survey, particularly after wave 1.
Between 2014 and 2020 the overall response rate in a given quarter
declined from around 50% to nearer 40%. During 2020 quarter 2, the first
full quarter during which Covid-19 restrictions were in force in the UK,
the response fell dramatically to below 30%. While some attempt to
improve response was made by increasing the wave 1 sample, the overall
response rate has continued to decline since, to almost 15% in 2023
quarter 2. Analysis of data from the LFS, particularly from 2020
onwards, should consider the potential impact of these very low response
rates, especially when conducting subgroup analysis.

<div class="figure">

<img src="man/figures/README-unnamed-chunk-2-1.png" alt="Labour Force Survey response rate 2014-2023" width="80%" />
<p class="caption">
Labour Force Survey response rate 2014-2023
</p>

</div>

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
    modelling](https://gitlab.com/stapm/projects/input-output-modelling/input-output-modelling).
    Here the package is used to create full-time equivalent employment
    by sector and year to estimate the impacts of changing demand in
    different sectors on total employment.
