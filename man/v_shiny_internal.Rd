% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v.R
\name{v_shiny_internal}
\alias{v_shiny_internal}
\title{Basic shiny app for running \code{mrgda::create_v_datatable()} on the server}
\usage{
v_shiny_internal(
  .df_list,
  .subject_col = NULL,
  .freeze_cols = NULL,
  .digits = 3,
  host = NULL,
  port = NULL
)
}
\arguments{
\item{.df_list}{A dataframe \emph{or} list of dataframes that you want to process and view.}

\item{.subject_col}{A character string specifying the subject column. If this column is
present in the dataframe, it will be used to group rows in the datatable.}

\item{.freeze_cols}{A character vector specifying columns to freeze when scrolling horizontally.
\code{.subject_col} will automatically be appended to this list (i.e. \code{.subject_col} is always frozen).}

\item{.digits}{number of digits to round numeric columns to. Set to \code{NULL} to prevent rounding.}

\item{host, port}{Host and port to serve the Shiny app on. If \code{port} isn't
specified, it is randomly selected from the valid range of dynamic ports.}
}
\description{
Basic shiny app for running \code{mrgda::create_v_datatable()} on the server
}
\keyword{internal}