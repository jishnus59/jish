% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Visualization.r
\name{Visualization}
\alias{Visualization}
\title{Visualization function}
\usage{
Visualization(df, target, var)
}
\arguments{
\item{x}{varibale need to look the target distribution}
}
\value{
It will give back you the deciled data with target disribution
}
\description{
Takes the data and divid in to 10 parts and looks the target distribution.
}
\note{
set the working directory of folder to dump the output and set the variable name as col_list
}
\examples{
data(CTDF)
View(CTDF)
col_list="SCR"
for (i in 1 : length(col_list)) {
Visualization(df = CTDF,target = "TARGET",var = col_list[i])
}
}
\author{
Rajesh Jakhotia
}
