#' Generate LLM-friendly package context
#'
#' Extracts roxygen documentation and function signatures for all exported
#' functions in the sec.gov package. Designed to be injected into an LLM
#' context window so it understands the full API without needing the
#' source code.
#'
#' @param compact If `TRUE` (default), shows roxygen + signature only.
#'   If `FALSE`, includes full function bodies.
#'
#' @return A single character string (invisibly). Also prints to console
#'   via `cat()`.
#'
#' @examples
#' \dontrun{
#' sec_context()
#' ctx <- sec_context()
#' sec_context(compact = FALSE)
#' }
#'
#' @export
sec_context <- function(compact = TRUE) {
  pkg <- "sec.gov"
  ns <- getNamespaceExports(pkg)
  ns <- sort(ns[!ns %in% c("sec_context", "print.sec_filing")])

  src_file <- system.file("source", "sec.R", package = pkg)
  if (src_file != "") {
    src_lines <- readLines(src_file, warn = FALSE)
  } else {
    src_lines <- NULL
  }

  chunks <- lapply(ns, function(fn_name) {
    if (is.null(src_lines)) {
      obj <- get(fn_name, envir = asNamespace(pkg))
      args <- paste(names(formals(obj)), collapse = ", ")
      return(paste0(fn_name, "(", args, ")\n"))
    }

    fn_pattern <- paste0("^", fn_name, " <- function")
    fn_line <- grep(fn_pattern, src_lines)
    if (length(fn_line) == 0) {
      obj <- get(fn_name, envir = asNamespace(pkg))
      args <- paste(names(formals(obj)), collapse = ", ")
      return(paste0(fn_name, "(", args, ")\n"))
    }

    fn_line <- fn_line[1]
    rox_start <- fn_line
    while (rox_start > 1 && grepl("^#'", src_lines[rox_start - 1]))
      rox_start <- rox_start - 1

    if (compact) {
      brace_line <- fn_line
      while (brace_line <= length(src_lines) && !grepl("\\{\\s*$", src_lines[brace_line]))
        brace_line <- brace_line + 1
      header <- src_lines[rox_start:brace_line]
      stub <- paste0("  # ... run `", fn_name,
                     "` to view source or `?", fn_name, "` for help")
      c(header, stub, "}", "")
    } else {
      depth <- 0
      end_line <- fn_line
      for (i in fn_line:length(src_lines)) {
        depth <- depth + nchar(gsub("[^{]", "", src_lines[i])) -
                         nchar(gsub("[^}]", "", src_lines[i]))
        if (depth == 0) { end_line <- i; break }
      }
      c(src_lines[rox_start:end_line], "")
    }
  })

  header <- paste0(
    "# sec.gov - SEC EDGAR Client for R\n",
    "# Version: ", utils::packageVersion(pkg), "\n",
    "# Dependencies: httr2, xml2, jsonlite (core); readxl (optional)\n",
    "# All functions return data.frames.\n",
    "#\n",
    "# ", length(ns), " exported functions:\n",
    "# ", paste(ns, collapse = ", "), "\n",
    "#\n\n"
  )

  body <- paste(unlist(chunks), collapse = "\n")
  out <- paste0(header, body)
  cat(out)
  invisible(out)
}
