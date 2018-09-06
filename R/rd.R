#' Creates R help entry
#'
#' @description
#'
#' It uses the YAML spec file to create a single help entry.  Its output is a text vector.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left empty.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' create_rd(my_spec)
#' @export
create_rd <- function(spec_path) {

  spec <- get_spec(spec_path = spec_path)

  header <- list(paste0("\\docType{", spec$type,"}"))
  if (!is.null(spec$help$name)) header <- c(header, paste0("\\name{", spec$help$name, "}"))
  if (!is.null(spec$help$alias)) header <- c(header, paste0("\\alias{", spec$help$alias, "}"))
  if (!is.null(spec$help$title)) header <- c(header, paste0("\\title{", spec$help$title, "}"))
  if (!is.null(spec$help$format)) header <- c(header, paste0("\\format{", spec$help$format, ""))
  header <- c(header, "\\describe{")

  items <- NULL
  items <- lapply(
    spec$variables, function(x){
      variable <- x["trans"]
      if (variable == "TRUE") variable <- "y"
      paste0("\\item{", variable, "}{", x["desc"], "}")
    }
  )
  items <- as.character(items)
  names(items) <- NULL

  footer <- list("}}")
  if (!is.null(spec$help$usage)) {
    footer <- c(footer, paste0("\\usage{", spec$help$usage, "}"))
  }
  if (!is.null(spec$help$description)) {
    footer <- c(footer, paste0("\\description{", spec$help$description, "}"))
  }
  if (!is.null(spec$help$source)) {
    footer <- c(footer, paste0("\\source{", spec$help$source, "}"))
  }
  footer <- c(footer, "\\keyword{datasets}")


  rd <- c(header, items, footer)
  as.character(rd)
}

#' Creates and saved R help file
#'
#' @description
#'
#' Saves a the help output, from 'create_rd()', into an 'rd' file with the same name as the translated dataset.
#'
#' This function is meant for packages that will ship with a copy of the translated data set.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left NULL.
#' @param rd_folder The target folder location where the 'rd' file will be save to. Defaults to 'man'.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' save_rd(my_spec, tempdir())
#' @export
save_rd <- function(spec_path = NULL, rd_folder = "man") {

  is.readable(rd_folder)

  spec <- get_spec(spec_path = spec_path)

  rd_name <- spec$name

  rd <- create_rd(spec_path)

  writeLines(
    rd,
    con = file.path(rd_folder, paste0(rd_name, ".rd")),
    useBytes = TRUE
  )
}

#' Creates rd files for multiple data sets
#'
#' @description
#'
#' Cycles through all of the spec files inside a folder specified in the 'spec_folder' argument.
#' The function translates and saves them to a specified folder.
#'
#' Saves the files to the folder specified in the 'rd_folder' argument.
#'
#' This function is meant for packages that will ship with a copy of the translated data set.
#'
#' @param spec_folder The path to the folder where the YAML spec files are located. Defaults to 'inst/specs'.
#' @param rd_folder The target folder location where the 'rd' file will be save to. Defaults to 'man'.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("specs", package = "datalang")
#' folder_rd(my_spec_folder, tempdir())
#' @export
folder_rd <- function(spec_folder = "inst/specs", rd_folder = "man") {

  is.readable(rd_folder)

  specs <- get_specs_folder(spec_folder = spec_folder)

  paths <- as.character(
    lapply(
      specs,
      function(x)x$path
    )
  )

  invisible(
    lapply(paths, function(x) save_rd(x, rd_folder = rd_folder))
  )

}
