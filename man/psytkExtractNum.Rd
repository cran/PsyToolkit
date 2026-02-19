\name{psytkExtractNum}
\alias{psytkExtractNum}
\title{psytkExtractNum}
\description{
Apply function to data each participants' experiment data.
}
\usage{
psytkExtractNum(data , dependent , names , conditions , f = mean)
}
\arguments{
  \item{data}{The experiment data frame}
  \item{dependent}{The column in the experiment data of interest}
  \item{names}{The names of the conditions}
  \item{conditions}{For each name of a condition a vector of logical selecting data}
  \item{f}{A function such as mean (default), median, min, max}
}
\value{
Returns a dataframe with one column for each condition.
}
\examples{
\dontrun{
d = psytkReadData("data")
stroopCorrect = d$expData.stroopForR[,"V7"] == 1 ## correct key pressed
stroopWrong   = d$expData.stroopForR[,"V7"] == 2 ## wrong key pressed
stroopError   = d$expData.stroopForR[,"V7"] != 1 ## wrong key pressed or
                                                 ## no key at all (timeout)
stroopSlow    = d$expData.stroopForR[,"V7"] == 3 ## timeout
con           = d$expData.stroopForR[,"V4"] == 1
inc           = d$expData.stroopForR[,"V4"] == 0
realdata      = d$expData.stroopForR[,"V1"] == "real"

conditionCongruent   = con
conditionIncongruent = inc

basicSelection = stroopCorrect & realdata

## the psytkExtractNum function is part of the PsyToolkit library. It
## extracts from the experiment data for each participant one row. So
## you end with a row of RT for the congruent and incongruent
## condition; you can later use this for the t test.

stroopRT = psytkExtractNum(
                data = d$expData.stroopForR ,
                dependent = 10 ,
                names = c("congruent","incongruent"),
                conditions = cbind( conditionCongruent   & basicSelection ,
                                    conditionIncongruent & basicSelection ))
}
}
