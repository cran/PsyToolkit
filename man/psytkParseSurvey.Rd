\name{psytkParseSurvey}
\alias{psytkParseSurvey}
\title{psytkParseSurvey}
\description{
Parses the survey.txt file in your data file. Rarely needed on its
own. Is used by psytkReadData
}
\usage{
psytkParseSurvey(datadir = NA , surveyfilename = NA)
}
\arguments{
  \item{datadir}{The folder where you extracted your PsyToolkit data zip file}
  \item{surveyfilename}{The name of the survey if not survey.txt}
}
\value{
Returns a survey objects giving data about questions and items in it.
}
\examples{
\dontrun{
mySurvey = psytkParseSurvey( "data" )
}
}
