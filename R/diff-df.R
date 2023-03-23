#' @keywords internal
diff_df <- function(.base, .compare, .unique_row_identifier = NULL, .unique_subject_identifier = NULL, .file){

  # Drop .unique_row_identifier
  if (!is.null(.unique_row_identifier)) {
    .base[[.unique_row_identifier]] <- NULL
    .compare[[.unique_row_identifier]] <- NULL
  }

  # out <- list()

  diffdf::diffdf(
    base = .base,
    compare = .compare,
    file = .file,
    suppress_warnings = TRUE
  )

  # if(!is.null(.unique_subject_identifier)){
  #   .out$by_id
  # }


}


# This blows up in certain cases, e.g.:

# x=  mtcars %>% arrange(cyl,mpg,hp)

# y = bind_rows(mtcars, slice(mtcars,1) %>% arrange(cyl,mpg,hp))


#
# added_rows <-  inner_join(x,y) == x
# dropped_rows

# added rows
# dropped rows
# added column
# dropped columns
# changed columns (check every column)

# old <- readr::read_csv(system.file("derived", "pk.csv", package = "mrgda"), na = ".")
# new <= old
# always
# nm_spec <- yspec::ys_load(system.file("derived", "pk.yml", package = "mrgda"))
# flags <- mrgda::view_mrgda_flags(.data = old, .spec = nm_spec, .view = FALSE)
# flags %>% filter(Flag == "num") %>% pull(`Assigned Columns`)
