\name{psytkReadTable}
\alias{psytkReadTable}
\title{psytkReadTable}
\description{
Read any textfile containing data. Idea for textfiles with unequal row lengths.
}
\usage{
psytkReadTable(filename)
}
\arguments{
  \item{filename}{The name of the text file to be rad}
}
\value{
A data frame
}
\details{
The benefit of this specific function over read.table() is that it can
read datafiles with really different numbers of items per row
easily. That can be helpful for reading in experiment data.\cr
This function is rarely needed for end users.
}
\examples{
\dontrun{
z = psytkReadTable( "mydata.txt" )
}
}
