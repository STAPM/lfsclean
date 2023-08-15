






<!-- README.md is generated from README.Rmd. Please edit that file -->

# Quarterly Labour Force Survey - Data Cleaning ![Labour Force Survey Data Wrangling](man/figures/lfsclean-open.png)

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

`lfsclean` is currently available only to members of the project team.
To access you need to [sign-up for a GitLab
account](https://gitlab.com/). You will then need to be added to the
STAPM project team to gain access. If you are on a Windows machine you
will also need to [install
Rtools](https://www.rdocumentation.org/packages/installr/versions/0.22.0/topics/install.Rtools).  
Once that is sorted, you can install the latest version or a specified
version from GitLab with:

``` r
#install.packages("devtools")
#install.packages("getPass")
#install.packages("git2r")

devtools::install_git(
  "https://github.com/STAPM/lfsclean.git", 
  build_vignettes = TRUE
)

# Where uname is your Gitlab user name.
# ref is the version you want to install - remove for the latest version
# this should make a box pop up where you enter your GitLab password
```

## Usage

The **inputs** are the raw LFS data files obtained from the [UK Data
Service](https://ukdataservice.ac.uk/). **These must be the tab
delimited versions of the data, not Stata or SPSS**, and all individual
tab delimited files needed must be placed together in a single
directory.

A typical workflow for using the package looks as follows, with the read
data function for each individual year contained within a global
cleaning function which applies each cleaning function in turn. The
collection of global cleaning functions for each year are then wrapped
in a function to combine years of data.

The `lfsclean()` wrapper function is used to process multiple quarterly
cross-sectional LFS data files into a single dataset of cleaned
variables.

``` r

#### read in and clean raw data using the package

####################
## input arguments

root  <- "C:/"
file  <- "" # path to the folder where the tab files are
year  <- 1993:2023
ages  <- 16:64
keep_vars <- NULL
complete_vars <- NULL
deflator <- "cpih"

##############################
### LFS data 1993 - 2022

lfs_data <- lfsclean(root = root,
                     file = file,
                     year = year,
                     ages = ages,
                     keep_vars = keep_vars,
                     complete_vars = complete_vars,
                     deflator = deflator)
```

The `lfsclean_5q()` wrapper function is used to process the raw
five-quarter longitudinal data.

### Projects

Some examples of projects making use of the `lfsclean` package are:

1.  [Input-Output
    modelling](https://gitlab.com/SPECTRUM_Sheffield/projects/input-output-modelling).
    Here the package is used to create full-time equivalent employment
    by sector and year to estimate the impacts of changing demand in
    different sectors on total employment.
