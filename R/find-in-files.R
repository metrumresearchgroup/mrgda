#' @keywords internal
find_in_files <- function(.paths, .string, .file_types = c("R", "Rmd", "ctl")){

  .out <- list()

  for (path.i in .paths) {

    files_search.i <- list.files(path = path.i, full.names = TRUE, recursive = TRUE)

    files_search.i <- files_search.i[tools::file_ext(files_search.i) %in% .file_types]

    found_in.i <- c()

    for (file.i in files_search.i) {

      if (any(grepl(pattern = .string, x = readLines(file.i, warn = FALSE), fixed = TRUE))) {

        found_in.i <- c(found_in.i, gsub(path.i, "", file.i, fixed = TRUE))
      }

    }

    .out[[gsub(here::here(), "", path.i, fixed = TRUE)]] <- found_in.i

  }

  return(.out)
}
