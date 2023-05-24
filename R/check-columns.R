#' Check Column Names in a DataFrame
#'
#' This function checks if the columns of a data frame (`df`) are correctly named based on
#' a lookup data frame (`lookup`). The `lookup` should specify what columns are 'required'
#' or 'permitted' and also provide common alternative names for the columns. If any alternative
#' names are found in `df`, the function will print a consolidated `dplyr` rename call to correct
#' all alternative names at once.
#'
#' @param df A data frame whose column names are being checked.
#' @param lookup A data frame that specifies the correct column names, whether each column is 'required'
#' or 'permitted', and common alternative names for each column. This data frame should have three columns:
#' 'column_name' (the correct names for the columns), 'type' (whether each column is 'required' or 'permitted'),
#' and 'alt_name' (common alternative names for each column). The 'alt_name' column should not have any duplicates.
#'
#' @return A named logical vector with two elements. The first element, 'All required columns present', is `TRUE`
#' if all required columns are present in `df` and `FALSE` otherwise. The second element,
#' 'No unpermitted columns present', is `TRUE` if there are no unpermitted columns in `df` and `FALSE` otherwise.
#' If any alternative names are found in `df`, the function will print a consolidated `dplyr` rename call
#' to correct all alternative names at once.
#'
#' @examples
#' # create a data frame to check
#' df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
#'
#' # create a lookup data frame
#' lookup <- data.frame(column_name = c("A", "B", "C"),
#'                      type = c("required", "required", "permitted"),
#'                      alt_name = c("AA", "BB", "CC"))
#'
#' # check the columns
#' check_columns(df, lookup)
#'
#' @export
check_columns <- function(df, lookup) {
  # get the names of the columns in the df and lookup data frames
  df_names <- names(df)
  lookup_names <- names(lookup)

  # check if lookup dataframe has required structure
  if(!all(c("column_name", "type", "alt_name") %in% lookup_names)) {
    stop("Lookup dataframe must have 'column_name', 'type' and 'alt_name' columns")
  }

  # separate required and permitted columns from the lookup dataframe
  required_cols <- lookup$column_name[lookup$type == "required"]
  permitted_cols <- lookup$column_name[lookup$type == "permitted"]

  # check if all required columns are in the data frame
  required_check <- all(required_cols %in% df_names)

  # check if there are any unpermitted columns in the data frame
  permitted_check <- all(df_names %in% c(required_cols, permitted_cols))

  # Check for alternative column names
  alt_names <- lookup$alt_name[lookup$alt_name %in% df_names]
  rename_list <- stats::setNames(lookup$column_name[lookup$alt_name %in% df_names], alt_names)

  if (length(alt_names) > 0) {
    cat(cli::code_highlight(paste0(
      "rename(",
      paste(
        rename_list,
        names(rename_list),
        sep = " = ",
        collapse = ", "
      ),
      ")"
    )))
  }

  # return the results
  return(c("All required columns present" = required_check,
           "No unpermitted columns present" = permitted_check))
}
