\name{psytkExtractPerc}
\alias{psytkExtractPerc}
\title{psytkExtractPerc}
\description{
For each participant, calculate a percentage based on conditions.
}
\usage{
psytkExtractPerc(data , dependent , names , conditions , f = mean)
}
\arguments{
  \item{data}{The experiment data frame}
  \item{dependent}{The column in the experiment data of interest}
  \item{names}{The names of the conditions}
  \item{conditions}{For each name of a condition a vector of logical selecting data}
  \item{f}{A function such as mean (default), median, min, max}
}
\value{
A vector the length of survey IDs in your dataset.
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

## the psytkExtractNum function is part of the PsyToolkit library. It
## extracts from the experiment data for each participant one row. So
## you end with a row of RT for the congruent and incongruent
## condition; you can later use this for the t test.

stroopRT = psytkExtractNum( data = d$expData.stroopForR ,
              dependent = 10 ,
              names = c("congruent","incongruent"),
              conditions = cbind( conditionCongruent   & basicSelection ,
              conditionIncongruent & basicSelection ) )

## the psytkExtractPerc function is part of the PsyToolkit library.

## It extracts from the experiment data for each participant one
## row. It calculates the Percentage of Errors (PE) for each
## participant. This is useful for two reasons: 1) You can use it to
## calculate the Stroop effect in error rates. 2) You can use it to
## find out if some participants performed extremely poorly (for
## example at chance level)

stroopPE = psytkExtractPerc(
              data = d$expData.stroopForR ,
              dependent = !stroopCorrect ,
              names = c("congruent","incongruent"),
              conditions = cbind( conditionCongruent & realdata ,
              conditionIncongruent & realdata ) )
} 
}
