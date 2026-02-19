\name{psytkReadData}
\alias{psytkReadData}
\title{psytkReadData}
\description{
Load PsyToolkit raw data.
}
\usage{
psytkReadData(surveyOrDir , psytkAllowRepeatExp = TRUE , verbose = TRUE)
}
\arguments{
  \item{surveyOrDir}{The name of the folder where you unzipped your PsyToolkit downloaded data file}
  \item{psytkAllowRepeatExp}{If more than one version of the same experiment available, the last will be used}
  \item{verbose}{With TRUE you get detailed information about each datafile.}
}
\value{
Returns on object holding all the data of the survey, including experiment data if available.
}
\details{
This is the start of any workflow for working with PsyToolkit data.
}
\examples{
\dontrun{
d = psytkReadData("myfolder")
}
}
