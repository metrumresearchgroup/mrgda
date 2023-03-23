#' @keywords internal
diff_df <- function(.base, .compare, .file){
  diffdf::diffdf(
    base = .base,
    compare = .compare,
    file = .file,
    suppress_warnings = TRUE
  )
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
