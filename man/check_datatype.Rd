% Please edit documentation in R/check_fields.R
\name{check_datatype}
\alias{check_datatype}
\title{Check if the required and recommended datatype-specific SHARK fields are present.}
\usage{
check_datatype(data, level = "error")
}
\arguments{
\item{data}{The data frame.}

\item{level}{The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".}
}
\value{
Any warnings or errors.
}
\description{
Missing or empty required fields are reported as errors,
missing or empty recommended fields are reported as warnings.
}
