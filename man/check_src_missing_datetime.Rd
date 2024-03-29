% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-src-missing-datetime.R
\name{check_src_missing_datetime}
\alias{check_src_missing_datetime}
\title{Check if missing datetimes in SDTM domain}
\usage{
check_src_missing_datetime(
  .domain_df,
  .domain_name,
  .time_col = NULL,
  .subject_col = "USUBJID"
)
}
\arguments{
\item{.domain_df}{source data (data.frame)}

\item{.domain_name}{Name of source domain (character)}

\item{.time_col}{Column name of datetime variable (character)}

\item{.subject_col}{Column name of subject identifier (character)}
}
\description{
Checks if there are any missing datetime records in a given data.frame. Some commonly used
domains have default datetime columns to use for this check, however the .time_col
argument can be used to indicate which variable should be checked.
}
\examples{
\dontrun{
check_src_missing_datetime(.domain_df = src_list$pc, .domain_name = "pc")
check_src_missing_datetime(.domain_df = src_list$ex, .domain_name = "ex")
}
}
