\name{psytkExtractFromExperiment}
\alias{psytkExtractFromExperiment}
\title{psytkExtractFromExperiment}
\description{
Extract the data of one participant from one experiment in the
PsyToolkit data object
}
\usage{
psytkExtractFromExperiment(dataset , expname , id = 1 , removeID = TRUE)
}
\arguments{
  \item{dataset}{The name of the PsyToolkit survey data}
  \item{expname}{The name of the experiment}
  \item{id}{The id, can either be numerical or UUID}
  \item{removeID}{Returned data does not contain participant IDs}
}
\value{
A data frame with the participant's data.
}
\examples{
\dontrun{
## imagine your data contains an experiment named "Stroop"
## imagine the Stroop data have as 5th column the reaction time
## this is a way to calculate the mean RT of each participant

d = psytkReadData("data")
meanRTs = numeric( d$n )
for ( i in d$IDcount ){
  tmpdata = psytkExtractFromExperiment( d , "Stroop" , id = i )
  meanRTs = mean( tmpdata[,5] )
}
}
}
