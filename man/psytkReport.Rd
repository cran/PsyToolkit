\name{psytkReport}
\alias{psytkReport}
\title{Create a Summary Report for PsyToolkit Data object}
\description{
Creates a textual summary report for PsyToolkit questionnaire data.
}
\usage{
psytkReport(psytkSurveyData, textbox = FALSE, question = TRUE , print =
TRUE )
}
\arguments{
  \item{psytkSurveyData}{A PsyToolkit survey data object (typically returned by \code{psytkReadData(foldername)}).}
  \item{textbox}{Logical; whether to include summaries for textbox
    items. Optional, because these can sometimes be long.}
  \item{question}{Logical; whether to include question text from q: lines when available.}
  \item{print}{Logical; whether or not the report is shown in terminal.}
}
\value{
Returns a report object, which can be shown with function print.
}

\examples{
## Real workflow (requires your own PsyToolkit data files in myfolder)
\dontrun{
d = psytkReadData( "myfolder" )
}
## runnable example for CRAN checks:
data(psytkDemo) # or for runnable example

myReport = psytkReport( psytkDemo ) # this will show the report in the terminal 
print( myReport ) # you can print the report later as well
}
