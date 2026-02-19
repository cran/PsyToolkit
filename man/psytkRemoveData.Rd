\name{psytkRemoveData}
\alias{psytkRemoveData}
\title{psytkRemoveData}
\description{
Allows you to remove participant datafiles from a PsyToolkit survey data object.
}
\usage{
psytkRemoveData(psytkSurveyData , selection)
}
\arguments{
  \item{psytkSurveyData}{The object holding the PsyToolkit survey data}
  \item{selection}{A logical vector as long as the number of participants/files used}
}
\value{
Returns a new object without the removed data. This includes removal from the participant data
}
\examples{
\dontrun{
## example of removing participant with ID number lower than 10 from all data

dataset1 = psytkReadData("data")
selection = rep( T , dataset$n )
selection[ d$IDcount < 10 ] = F
dataset2 = psytkRemoveData( d , selection )
}
}
