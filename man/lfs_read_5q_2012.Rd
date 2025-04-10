% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfs_read_5q_2012.R
\name{lfs_read_5q_2012}
\alias{lfs_read_5q_2012}
\title{Read LFS 2012 longitudinal}
\usage{
lfs_read_5q_2012(
  root = c("X:/"),
  file = "HAR_PR/PR/LFS/Data/longitudinal/tab/"
)
}
\arguments{
\item{root}{Character - the root directory}

\item{file}{Character - the file path and name}
}
\value{
Returns a data table
}
\description{
Reads and performs basic cleaning on the Labour Force Survey five-quarter longitudinal
data with a 1st wave that began in calendar year 2012.
}
