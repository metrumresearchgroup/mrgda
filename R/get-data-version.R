#' Get data version
#'
#' @description
#' This function reads a YAML file containing version information about data and
#' returns the previous or current version along with the corresponding file name.
#'
#' @param .endpoint A character string denoting the endpoint to be used for file naming.
#' @param .lookup_path The path to the YAML file containing version information.
#' @param .version Specify if you want current or previous data version (defaults to current)
#'
#' @return A list containing the information of the current or previous data version
#' and the corresponding file name.
#'
#' @examples
#' # Make sure the yaml file exists at the specified path before running
#' get_data_version(
#'   .endpoint = "pk",
#'   .lookup_path = system.file("derived/data-version-lookup.yaml", package = "mrgda"),
#'   .version = "current"
#'   )
#'
#' @export
get_data_version <- function(.endpoint, .lookup_path, .version = "current"){

  stopifnot(.version %in% c("current", "previous"))

  .vn <- dplyr::if_else(.version == "current", 1, 2)

  out <- list()

  out$info <-
    yaml::read_yaml(.lookup_path) %>%
    dplyr::bind_rows() %>%
    suppressMessages() %>%
    dplyr::arrange(-Major, -Minor) %>%
    dplyr::slice(.vn)

  out$file <-
    out$info %>%
    dplyr::mutate(File = glue::glue("{.endpoint}-{Major}-{Minor}")) %>%
    dplyr::pull(File) %>%
    as.character()

  return(out)
}
