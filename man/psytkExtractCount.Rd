\name{psytkExtractCount}
\alias{psytkExtractCount}
\title{psytkExtractCount}
\description{
Find out how for each participant how many cases of condition exist
}
\usage{
psytkExtractCount(data , names , conditions)
}
\arguments{
  \item{data}{data object}
  \item{names}{names of conditions}
  \item{conditions}{The actual conditions as logical vectors}
}
\value{
A dataframe with for each condition a vector with data for each participant/file.
}
\examples{
\dontrun{
d = psytkReadData("data")

stroopCorrect = d$expData.stroopForR[,"V7"] == 1 ## correct key pressed
stroopWrong   = d$expData.stroopForR[,"V7"] == 2 ## wrong key pressed
stroopError   = d$expData.stroopForR[,"V7"] != 1 ## wrong key pressed
                                                 ## or no key at all (timeout)
stroopSlow    = d$expData.stroopForR[,"V7"] == 3 ## timeout
con           = d$expData.stroopForR[,"V4"] == 1
inc           = d$expData.stroopForR[,"V4"] == 0
realdata      = d$expData.stroopForR[,"V1"] == "real"

conditionCongruent   = con
conditionIncongruent = inc

basicSelection = stroopCorrect & realdata

subsel = realdata

stroopTrials = psytkExtractCount(
                   data = d$expData.stroopForR ,
                   names = c("congruent","incongruent","all"),
                   conditions = cbind( conditionCongruent & subsel ,
                   conditionIncongruent & subsel  , realdata & subsel ))
}
}

