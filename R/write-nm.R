write_nm <- function(.data, .spec, .file) {

  # Prepare Meta Data Folder ------------------------------------------------
  .data_location <- dirname(.file)
  .data_name <- tools::file_path_sans_ext(basename(.file))
  .meta_data_folder <- file.path(.data_location, .data_name)

  # Create directory anew if it exists
  if (dir.exists(.meta_data_folder)) {
    unlink(.meta_data_folder, recursive = TRUE)
  }

  dir.create(.meta_data_folder)

  # Write Out Meta Data -----------------------------------------------------
  haven::write_xpt(
    data = yspec::ys_add_labels(.data, spec),
    path = file.path(.meta_data_folder, paste0(.data_name, ".xpt")),
    version = 5, # Use version 5
    name = .data_name # Max of 8 chars
  )

  yspec::ys_document(
    x = .spec,
    output_dir = .meta_data_directory
  )

  if (file.exists(.file)) {
    diffdf::diffdf(
      base = readr::read_csv(.file, na = "."),
      compare = .data,
      file = file.path(.meta_data_folder, "data-diff.txt"),
      suppress_warnings = TRUE
    )
  }

  # Write Out Main Data Set -------------------------------------------------
  data.table::fwrite(
    x = .data,
    file = .file,
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    na = "."
  )

}
