\name{psytkReport}
\alias{psytkReport}
\title{Create a Summary Report for PsyToolkit Data object}
\description{
Creates a textual summary report for PsyToolkit questionnaire data.
}
\usage{
psytkReport(psytkSurveyData, textbox = FALSE, question = TRUE)
}
\arguments{
  \item{psytkSurveyData}{A PsyToolkit survey data object (typically returned by \code{psytkReadData(foldername)}).}
  \item{textbox}{Logical; whether to include summaries for textbox
    items. Optional, because these can sometimes be long.}
  \item{question}{Logical; whether to include question text from q: lines when available.}
}
\value{
Typically called for its printed output. Returns the report invisibly.
}

\examples{
## Real workflow (requires your own PsyToolkit data files in myfolder)
\dontrun{
d = psytkReadData( "myfolder" )
}
## runnable example for CRAN checks:
data(d) # or for runnable example

psytkReport( d )
}
