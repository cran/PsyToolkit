\name{psytkExport}
\alias{psytkExport}
\title{psytkExport}
\description{
Exports most of the loaded PsyToolkit data to Excel file.
}
\usage{
psytkExport(surveydata , file = "exported_data.xlsx" , labels = F , orderByType = F)
}
\arguments{
  \item{surveydata}{The PsyToolkit survey data object}
  \item{file}{If no filename provided, default exported_data.xlsx will be used}
  \item{labels}{Not yet implemented}
  \item{orderByType}{Order output by question type instead of order of survey}
}
\value{
No returned value.
}
\examples{
## Real workflow (requires your own PsyToolkit data files in myfolder)
\dontrun{
d = psytkReadData( "myfolder" )
}
## runnable example for CRAN checks:
data(d) # or for runnable example

tmp_out_file = tempfile(fileext=".xlsx")

psytkExport( d , tmp_out_file )

unlink( tmp_out_file)
}
