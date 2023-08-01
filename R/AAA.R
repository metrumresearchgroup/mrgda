# needed for gsub piping
utils::globalVariables(c("."))

# tidyselect::where isnt exported in earlier versions
# this will add support for earlier cases
utils::globalVariables("where")
