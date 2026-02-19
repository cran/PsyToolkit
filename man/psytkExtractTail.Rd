\name{psytkExtractTail}
\alias{psytkExtractTail}
\title{psytkExtractTail}
\description{
Extracts a value from each participants experiment data
}
\usage{
psytkExtractTail(data , dependent)
}
\arguments{
  \item{data}{The experiment data (not the survey object)}
  \item{dependent}{The column of the table you are interested in.}
}
\value{
A vector of values as long as there are files/participants/sessions.
}
\examples{
\dontrun{
## assume there is an experiment Stroop and the 5th column of this file
## is the reaction time

d = psytkReadData("data")
myExpData = d$expData.Stroop
speed = psytkExtractTail( myExpData , 5 )
}
}
