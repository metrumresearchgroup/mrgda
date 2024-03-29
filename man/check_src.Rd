% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-src.R
\name{check_src}
\alias{check_src}
\title{Run all source data checks}
\usage{
check_src(.src_list, .subject_col = "USUBJID", .return_details = FALSE)
}
\arguments{
\item{.src_list}{List of all soure domain data.frames}

\item{.subject_col}{Column name of subject identifier (character)}

\item{.return_details}{Return list of all summary checks be returned (TRUE/FALSE)}
}
\description{
This will run all source data checks available in the \code{mrgda} package. When
given a list of source data.frames it will perform a series of checks on each.
A high level summary of the passing/failing checks will be provided in the console
and more in depth results can be accessed by setting the \code{.return_details} argument to
\code{TRUE}.
}
