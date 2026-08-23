#' Validate a dataset with yspec, NM-TRAN, and NONMEM
#'
#' @description
#' Performs a disposable validation of a NONMEM-ready dataset. The function
#' checks the dataset against a yspec specification, generates a matching
#' `$INPUT` record, runs NM-TRAN to obtain direct preprocessing diagnostics,
#' and submits a fast NONMEM `CHECKOUT` run. All generated model files and
#' NONMEM output are created in a temporary directory and removed when the
#' function exits.
#'
#' @param data_path Character scalar giving the path to a CSV dataset.
#'   Missing values may be encoded as an empty field, `.`, or `NA`.
#' @param spec_path Character scalar giving the path to the yspec YAML
#'   specification corresponding to `data_path`.
#'
#' @details
#' The dataset columns must have exactly the same names and order as the yspec
#' specification. Numeric and integer columns are parsed as numeric values.
#' Character columns are checked by yspec, then replaced with zero in the
#' temporary NONMEM input file and explicitly marked `DROP` in `$INPUT`.
#'
#' The function first looks upward from `data_path`, `spec_path`, and the current
#' working directory for an existing `bbi.yaml`. When none is found, it creates
#' a temporary configuration using `NONMEM_DIR`, `NM_DIR`, `/opt/NONMEM`, or
#' `/opt/nonmem`. `NONMEM_VERSION` may be set to select an installed version;
#' otherwise the highest available version is selected.
#'
#' A working `bbi` executable is located from `bbr.bbi_exe_path` or `PATH`. If
#' none is available, [bbr::use_bbi()] is called noninteractively. The resolved
#' executable is assigned to the session option `bbr.bbi_exe_path`.
#'
#' The generated control stream uses `$DATA ... CHECKOUT` with a minimal `$PRED`
#' block. This verifies that NM-TRAN and NONMEM can process the data but does not
#' produce meaningful predictions or residuals because no dataset-independent
#' scientific model exists.
#'
#' yspec range and value failures do not prevent the NONMEM checkout from being
#' attempted when the dataset columns and numeric values are otherwise usable.
#' This allows the resulting error to report that NM-TRAN and NONMEM passed
#' while the yspec checks failed.
#'
#' @return
#' Invisibly returns an object of class `nm_data_validation`, implemented as a
#' list containing:
#' \describe{
#'   \item{ok}{Always `TRUE` when the function returns normally.}
#'   \item{data_path}{Absolute source dataset path.}
#'   \item{spec_path}{Absolute specification path.}
#'   \item{bbi_path}{Resolved `bbi` executable path.}
#'   \item{bbi_version}{Resolved `bbi` version.}
#'   \item{nonmem_version}{NONMEM version used by NM-TRAN.}
#'   \item{yspec_ok}{Logical yspec validation status.}
#'   \item{yspec_output}{Captured yspec validation report.}
#'   \item{nmtran_output}{Captured NM-TRAN output.}
#'   \item{nonmem_listing}{Captured NONMEM listing file.}
#' }
#'
#' Errors include the relevant yspec report, NM-TRAN output, bbi output, or the
#' end of the NONMEM listing whenever available.
#'
#' @examples
#' \dontrun{
#' result <- validate_nm_data(
#'   "data/derived/pk.csv",
#'   "data/derived/pk.yml"
#' )
#'
#' result$nonmem_version
#' }
#'
#' @export
validate_nm_data <- function(data_path, spec_path) {
  tail_text <- function(x, n = 150L) {
    if (is.null(x) || !length(x)) {
      return("<no diagnostic output was produced>")
    }

    x <- unlist(
      strsplit(as.character(x), "\\r?\\n"),
      use.names = FALSE
    )
    x <- x[nzchar(trimws(x))]

    if (!length(x)) {
      return("<no diagnostic output was produced>")
    }

    paste(utils::tail(x, n), collapse = "\n")
  }

  stop_with_output <- function(message, output = NULL) {
    suffix <- if (is.null(output) || !length(output)) {
      ""
    } else {
      paste0("\n\n", tail_text(output))
    }

    stop(paste0(message, suffix), call. = FALSE)
  }

  find_up <- function(start, filename = "bbi.yaml") {
    current <- normalizePath(start, mustWork = FALSE)

    if (!dir.exists(current)) {
      current <- dirname(current)
    }

    repeat {
      candidate <- file.path(current, filename)

      if (file.exists(candidate)) {
        return(normalizePath(candidate, mustWork = TRUE))
      }

      parent <- dirname(current)

      if (identical(parent, current)) {
        return(NULL)
      }

      current <- parent
    }
  }

  resolve_bbi <- function(candidate) {
    if (
      is.null(candidate) ||
      length(candidate) != 1L ||
      !nzchar(candidate)
    ) {
      return("")
    }

    resolved <- unname(Sys.which(candidate))

    if (!nzchar(resolved) && file.exists(candidate)) {
      resolved <- normalizePath(candidate, mustWork = TRUE)
    }

    if (!nzchar(resolved) || !file.exists(resolved)) {
      return("")
    }

    version <- tryCatch(
      bbr::bbi_version(.bbi_exe_path = resolved),
      error = function(e) ""
    )

    if (!nzchar(version)) {
      return("")
    }

    normalizePath(resolved, mustWork = TRUE)
  }

  ensure_bbi <- function() {
    configured <- getOption("bbr.bbi_exe_path", "bbi")
    resolved <- resolve_bbi(configured)

    if (!nzchar(resolved)) {
      resolved <- resolve_bbi("bbi")
    }

    if (!nzchar(resolved)) {
      old_suppress <- getOption("bbr.suppress_interactivity")

      on.exit(
        options(bbr.suppress_interactivity = old_suppress),
        add = TRUE
      )

      options(
        bbr.bbi_exe_path = "bbi",
        bbr.suppress_interactivity = TRUE
      )

      installation <- tryCatch(
        {
          bbr::use_bbi()
          NULL
        },
        error = identity
      )

      if (inherits(installation, "error")) {
        stop_with_output(
          paste(
            "No working bbi executable was found, and automatic",
            "installation failed:"
          ),
          conditionMessage(installation)
        )
      }

      xdg_home <- Sys.getenv("XDG_DATA_HOME", unset = "")
      home <- Sys.getenv("HOME", unset = "")

      candidates <- c(
        "bbi",
        if (nzchar(xdg_home)) {
          file.path(xdg_home, "bbi", "bbi")
        } else {
          ""
        },
        if (nzchar(home)) {
          file.path(home, ".local", "share", "bbi", "bbi")
        } else {
          ""
        },
        "/usr/local/bin/bbi"
      )

      for (candidate in candidates[nzchar(candidates)]) {
        resolved <- resolve_bbi(candidate)

        if (nzchar(resolved)) {
          break
        }
      }
    }

    if (!nzchar(resolved)) {
      stop(
        paste(
          "bbr::use_bbi() completed, but a working bbi executable",
          "could not be located in the current R session."
        ),
        call. = FALSE
      )
    }

    options(bbr.bbi_exe_path = resolved)

    list(
      path = resolved,
      version = bbr::bbi_version(.bbi_exe_path = resolved)
    )
  }

  find_nonmem <- function() {
    requested_version <- Sys.getenv(
      "NONMEM_VERSION",
      unset = ""
    )

    roots <- unique(c(
      Sys.getenv("NONMEM_DIR", unset = ""),
      Sys.getenv("NM_DIR", unset = ""),
      "/opt/NONMEM",
      "/opt/nonmem"
    ))
    roots <- roots[nzchar(roots)]

    for (candidate in roots) {
      candidate <- normalizePath(
        candidate,
        mustWork = FALSE
      )

      if (!dir.exists(candidate)) {
        next
      }

      direct_nmtran <- file.path(
        candidate,
        "tr",
        "NMTRAN.exe"
      )

      # NONMEM_DIR may point directly to an installation.
      if (file.exists(direct_nmtran)) {
        version <- basename(candidate)

        if (
          nzchar(requested_version) &&
          !identical(requested_version, version)
        ) {
          next
        }

        return(list(
          root = dirname(candidate),
          version = version,
          home = candidate
        ))
      }

      # Alternatively, NONMEM_DIR may contain multiple installations.
      installations <- list.files(
        candidate,
        full.names = TRUE,
        recursive = FALSE,
        all.files = FALSE
      )
      installations <- installations[
        dir.exists(installations)
      ]
      installations <- installations[
        file.exists(
          file.path(
            installations,
            "tr",
            "NMTRAN.exe"
          )
        )
      ]

      if (!length(installations)) {
        next
      }

      versions <- basename(installations)

      if (nzchar(requested_version)) {
        selected <- match(
          requested_version,
          versions
        )

        if (is.na(selected)) {
          next
        }
      } else {
        numeric_version <- suppressWarnings(
          as.numeric(
            sub(
              "^[^0-9]*([0-9]+).*$",
              "\\1",
              versions
            )
          )
        )

        selected <- order(
          numeric_version,
          versions,
          decreasing = TRUE,
          na.last = TRUE
        )[1L]
      }

      return(list(
        root = candidate,
        version = versions[[selected]],
        home = installations[[selected]]
      ))
    }

    requested_note <- if (nzchar(requested_version)) {
      paste0(
        " Requested NONMEM_VERSION was '",
        requested_version,
        "'."
      )
    } else {
      ""
    }

    stop(
      paste0(
        "No usable NONMEM installation was found. Set NONMEM_DIR ",
        "to the directory containing the NONMEM version directories, ",
        "or set it directly to a NONMEM installation directory.",
        requested_note
      ),
      call. = FALSE
    )
  }

  data_path <- normalizePath(
    data_path,
    mustWork = FALSE
  )
  spec_path <- normalizePath(
    spec_path,
    mustWork = FALSE
  )

  if (!file.exists(data_path)) {
    stop(
      "Data file does not exist: ",
      data_path,
      call. = FALSE
    )
  }

  if (!file.exists(spec_path)) {
    stop(
      "Specification file does not exist: ",
      spec_path,
      call. = FALSE
    )
  }

  data_path <- normalizePath(
    data_path,
    mustWork = TRUE
  )
  spec_path <- normalizePath(
    spec_path,
    mustWork = TRUE
  )

  work_dir <- tempfile("nm-data-validation-")
  fs::dir_create(work_dir)

  on.exit(
    {
      if (fs::dir_exists(work_dir)) {
        fs::dir_delete(work_dir)
      }
    },
    add = TRUE
  )

  bbi <- ensure_bbi()

  spec <- tryCatch(
    yspec::ys_load(spec_path),
    error = function(e) {
      stop(
        "Could not load the yspec specification:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  data <- tryCatch(
    readr::read_csv(
      data_path,
      na = c("", ".", "NA"),
      show_col_types = FALSE,
      name_repair = "minimal"
    ),
    error = function(e) {
      stop(
        "Could not read the dataset:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (!nrow(data)) {
    stop(
      "The dataset contains no data rows.",
      call. = FALSE
    )
  }

  expected_names <- names(spec)
  actual_names <- names(data)

  if (!identical(actual_names, expected_names)) {
    missing_columns <- setdiff(
      expected_names,
      actual_names
    )
    extra_columns <- setdiff(
      actual_names,
      expected_names
    )

    details <- c(
      if (length(missing_columns)) {
        paste0(
          "Missing columns: ",
          paste(missing_columns, collapse = ", ")
        )
      },
      if (length(extra_columns)) {
        paste0(
          "Unexpected columns: ",
          paste(extra_columns, collapse = ", ")
        )
      },
      if (
        !length(missing_columns) &&
        !length(extra_columns)
      ) {
        "The columns are present but are not in specification order."
      },
      paste0(
        "Expected: ",
        paste(expected_names, collapse = ", ")
      ),
      paste0(
        "Observed: ",
        paste(actual_names, collapse = ", ")
      )
    )

    stop(
      paste(
        c(
          "Dataset columns do not match the yspec specification.",
          details
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  spec_types <- vapply(
    spec,
    function(column) {
      type <- column[["type"]]

      if (is.null(type) || !length(type)) {
        "numeric"
      } else {
        as.character(type[[1L]])
      }
    },
    character(1)
  )

  numeric_columns <- names(spec_types)[
    spec_types %in% c("numeric", "integer")
  ]
  character_columns <- names(spec_types)[
    spec_types == "character"
  ]

  if (!length(numeric_columns)) {
    stop(
      paste(
        "The specification contains no numeric or integer",
        "columns for NONMEM."
      ),
      call. = FALSE
    )
  }

  # Parse all columns that NONMEM will read as numeric.
  check_data <- data

  for (column in numeric_columns) {
    source <- check_data[[column]]

    if (is.numeric(source) || is.integer(source)) {
      parsed <- as.numeric(source)
    } else if (is.logical(source)) {
      parsed <- as.numeric(source)
    } else {
      parsed <- suppressWarnings(
        readr::parse_double(
          as.character(source),
          na = c("", ".", "NA")
        )
      )
    }

    bad <- !is.na(source) & is.na(parsed)

    if (any(bad)) {
      bad_rows <- which(bad)
      examples <- unique(
        as.character(source[bad])
      )

      stop(
        paste0(
          "Column '",
          column,
          "' is declared ",
          spec_types[[column]],
          " but contains non-numeric data.\n",
          "Example rows: ",
          paste(
            utils::head(bad_rows, 10L),
            collapse = ", "
          ),
          "\nExample values: ",
          paste(
            utils::head(examples, 10L),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    check_data[[column]] <- parsed
  }

  # Run yspec against the parsed data and retain its detailed report.
  yspec_log_path <- file.path(
    work_dir,
    "yspec-check.txt"
  )

  yspec_result <- tryCatch(
    suppressMessages(
      yspec::ys_check(
        check_data,
        spec,
        output = yspec_log_path,
        error_on_fail = FALSE
      )
    ),
    error = identity
  )

  yspec_output <- if (file.exists(yspec_log_path)) {
    readLines(
      yspec_log_path,
      warn = FALSE
    )
  } else {
    character()
  }

  if (inherits(yspec_result, "error")) {
    stop_with_output(
      paste0(
        "yspec validation could not be completed:\n",
        conditionMessage(yspec_result)
      ),
      yspec_output
    )
  }

  yspec_ok <- isTRUE(yspec_result)

  # Make a purely numeric temporary copy for NONMEM. Character columns have
  # already been checked by yspec and will be marked DROP in $INPUT.
  nm_data <- check_data

  for (column in character_columns) {
    nm_data[[column]] <- rep(
      0,
      nrow(nm_data)
    )
  }

  staged_data_path <- file.path(
    work_dir,
    "data.csv"
  )

  readr::write_csv(
    nm_data,
    staged_data_path,
    na = ".",
    col_names = FALSE
  )

  input_block <- yspec::nm_input(
    spec,
    .cat = FALSE,
    .long = FALSE,
    .drop = character_columns
  )

  # The table is only used to confirm that NONMEM completed the checkout.
  # Limit it to 20 fields to avoid creating an unnecessarily wide output.
  table_columns <- utils::head(
    numeric_columns,
    20L
  )

  table_block <- c(
    "$TABLE",
    strwrap(
      paste(table_columns, collapse = " "),
      width = 72L,
      prefix = "  ",
      exdent = 2L
    ),
    "  NOPRINT NOAPPEND ONEHEADER FILE=check.tab"
  )

  model_name <- "data-check"
  model_path <- file.path(
    work_dir,
    model_name
  )
  control_path <- paste0(
    model_path,
    ".ctl"
  )

  # ../data.csv is resolved from the bbr output directory, which is one
  # directory below the control stream.
  control_stream <- c(
    "$PROBLEM Generic disposable NONMEM data validation",
    "",
    input_block,
    "",
    "$DATA ../data.csv CHECKOUT WIDE",
    "",
    "$PRED",
    "Y = 0",
    "",
    table_block,
    ""
  )

  writeLines(
    control_stream,
    control_path,
    useBytes = TRUE
  )

  # Reuse the nearest project configuration when possible.
  config_candidates <- unique(
    unlist(
      lapply(
        c(
          dirname(data_path),
          dirname(spec_path),
          getwd()
        ),
        find_up
      ),
      use.names = FALSE
    )
  )
  config_candidates <- config_candidates[
    nzchar(config_candidates)
  ]

  if (length(config_candidates)) {
    config_path <- config_candidates[[1L]]
  } else {
    nonmem <- find_nonmem()

    initialization <- tryCatch(
      {
        invisible(
          utils::capture.output(
            bbr::bbi_init(
              .dir = work_dir,
              .nonmem_dir = nonmem$root,
              .nonmem_version = nonmem$version
            )
          )
        )

        NULL
      },
      error = identity
    )

    if (inherits(initialization, "error")) {
      stop(
        "Could not create a temporary bbi configuration:\n",
        conditionMessage(initialization),
        call. = FALSE
      )
    }

    config_path <- file.path(
      work_dir,
      "bbi.yaml"
    )
  }

  model <- tryCatch(
    bbr::new_model(
      .path = model_path,
      .description = paste(
        "Temporary data validation for",
        basename(data_path)
      ),
      .tags = c(
        "temporary",
        "data-check",
        "yspec"
      ),
      .overwrite = TRUE
    ),
    error = function(e) {
      stop(
        "Could not create the temporary bbr model:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Run NM-TRAN separately so its exit status and direct diagnostic output
  # can be reported before attempting the complete NONMEM checkout.
  nmtran_result <- tryCatch(
    bbr::run_nmtran(
      model,
      .config_path = config_path,
      clean = TRUE
    ),
    error = identity
  )

  if (inherits(nmtran_result, "error")) {
    stop(
      "NM-TRAN could not be started:\n",
      conditionMessage(nmtran_result),
      call. = FALSE
    )
  }

  if (!identical(
    as.integer(nmtran_result$status_val),
    0L
  )) {
    stop_with_output(
      paste0(
        "NM-TRAN validation failed with status ",
        nmtran_result$status_val,
        ":"
      ),
      nmtran_result$output
    )
  }

  # NM-TRAN passed. Run the actual NONMEM checkout locally.
  submission_result <- tryCatch(
    bbr::submit_model(
      model,
      .mode = "local",
      .config_path = config_path,
      .overwrite = TRUE,
      .wait = TRUE
    ),
    error = identity
  )

  listing_path <- bbr::build_path_from_model(
    model,
    ".lst"
  )

  listing_output <- if (file.exists(listing_path)) {
    readLines(
      listing_path,
      warn = FALSE
    )
  } else {
    character()
  }

  if (inherits(submission_result, "error")) {
    diagnostic <- c(
      conditionMessage(submission_result),
      if (length(listing_output)) {
        c(
          "",
          "End of NONMEM listing:",
          utils::tail(
            listing_output,
            150L
          )
        )
      }
    )

    stop_with_output(
      "NM-TRAN passed, but the NONMEM checkout failed:",
      diagnostic
    )
  }

  if (!file.exists(listing_path)) {
    bbi_output <- if (
      is.list(submission_result) &&
      !is.null(submission_result[["stdout"]])
    ) {
      submission_result[["stdout"]]
    } else {
      NULL
    }

    stop_with_output(
      "NONMEM returned without producing a listing file.",
      bbi_output
    )
  }

  checkout_confirmed <- any(
    grepl(
      "DATA CHECKOUT RUN:[[:space:]]+YES",
      listing_output,
      ignore.case = TRUE
    )
  )

  output_dir <- bbr::get_output_dir(model)
  table_path <- file.path(
    output_dir,
    "check.tab"
  )

  if (
    !checkout_confirmed ||
    !file.exists(table_path)
  ) {
    reason <- c(
      if (!checkout_confirmed) {
        "The listing did not confirm 'DATA CHECKOUT RUN: YES'."
      },
      if (!file.exists(table_path)) {
        "The expected checkout table was not created."
      },
      "",
      "End of NONMEM listing:",
      utils::tail(
        listing_output,
        150L
      )
    )

    stop_with_output(
      paste(
        "NONMEM ran but the disposable data checkout",
        "was not confirmed:"
      ),
      reason
    )
  }

  if (!yspec_ok) {
    stop_with_output(
      paste(
        "NM-TRAN and NONMEM checkout completed, but the dataset",
        "failed the yspec value or range checks:"
      ),
      yspec_output
    )
  }

  message(
    "Validation passed: yspec, NM-TRAN, and NONMEM checkout completed."
  )

  invisible(
    structure(
      list(
        ok = TRUE,
        data_path = data_path,
        spec_path = spec_path,
        bbi_path = bbi$path,
        bbi_version = bbi$version,
        nonmem_version = nmtran_result$nonmem_version,
        yspec_ok = TRUE,
        yspec_output = yspec_output,
        nmtran_output = nmtran_result$output,
        nonmem_listing = listing_output
      ),
      class = "nm_data_validation"
    )
  )
}
