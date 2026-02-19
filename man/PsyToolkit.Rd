\name{PsyToolkit-package}
\alias{PsyToolkit-package}
\alias{PsyToolkit}
\docType{package}
\title{PsyToolkit: Analysis Tools for PsyToolkit Data}
\description{
Functions to read, summarise, and export PsyToolkit questionnaire and experiment data.
}
\details{

\strong{Typical workflow}

\enumerate{
  \item Download PsyToolkit data from your PsyToolkit account (a zip file).
  \item Create your R working directory, and within that, a folder \code{data}.
  \item Extract the downloaded PsyToolkit zip file into your new folder \code{data}.
  \item Read the data from the folder with \code{\link{psytkReadData}}.
  \item Summarise the data with \code{\link{psytkReport}}.
  \item Process data with other functions, such as \code{\link{psytkExtractFromExperiment}}.
  \item Export data with \code{\link{psytkExport}}.
}

\strong{Main functions}

\itemize{
  \item \code{\link{psytkReadData}}: Read PsyToolkit data into an R object.
  \item \code{\link{psytkReport}}: Print a summary report.
  \item \code{\link{psytkExport}}: Export data to an Excel workbook.
  \item \code{\link{psytkTable}}: Print formatted tables.
  \item \code{\link{psytkExtractFromExperiment}}: Extract data for detailed processing.
  \item \code{\link{psytkExtractTail}}: Extract last line from experiment files.
}

\strong{Other functions}

\itemize{
  \item \code{\link{psytkParseSurvey}}
  \item \code{\link{psytkReadTable}}
  \item \code{\link{psytkRemoveData}}
  \item \code{\link{psytkQ}}
  \item \code{\link{psytkExtractCount}}
  \item \code{\link{psytkExtractNum}}
  \item \code{\link{psytkExtractPerc}}
  \item \code{\link{psytkExtractTail}}
  \item \code{\link{psytkExtractFromExperiment}}
}

\strong{Example}

A small example object is included for documentation examples: \code{data(d)}.
This is not required for normal usage.
}

