% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/v-helpers.R
\name{make_v_caption}
\alias{make_v_caption}
\title{Make a caption displaying the dataset name and number of subjects}
\usage{
make_v_caption(.name, .df, .subject_col = NULL, .font_size = 10)
}
\arguments{
\item{.name}{name of the dataset}

\item{.df}{A dataframe that you want to process and view.}

\item{.subject_col}{A character string specifying the subject column. If this column is
present in the dataframe, it will be used to group rows in the datatable.}

\item{.font_size}{font size of the caption}
}
\value{
a \code{shiny.tag} object
}
\description{
Make a caption for for each rendering of \code{create_v_datatable()}, displaying the
number of subjects under the dataset name (center aligned).
}
\keyword{internal}
