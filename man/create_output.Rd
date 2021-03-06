% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_output.R
\name{create_output}
\alias{create_output}
\title{Create Output}
\usage{
create_output(target_directories)
}
\arguments{
\item{target_directories}{A list of PDF file locations to be scraped and converted.}
}
\value{
A data frame representing structured data for the list of input files.
}
\description{
Create Output
}
\examples{
\dontrun{
course_ratings <- create_output('~/courses/')
}
}
