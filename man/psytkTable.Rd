\name{psytkTable}
\alias{psytkTable}
\title{psytkTable}
\description{Present R data frame or R matrix in a nice table for R terminal output
}
\usage{
psytkTable(x, header = NULL , align = "L" , padding=2  , wrap = 20)
}
\arguments{
  \item{x}{A data frame or matrix with some data.}
  \item{header}{A header above the content of the table.}
  \item{align}{L, C, or R for left, center, or right alignment for all or vector of each column}
  \item{padding}{Spacing between columns, default is 2}
  \item{wrap}{Wrap cells if they have more than 20 characters}
}
\value{
This function simply outputs to terminal screen.
}
\examples{
\dontrun{
x = data.frame( items=1:3,names=c("John","Claudia","Mel") )
psytkTable( x , header=TRUE )
}
}
