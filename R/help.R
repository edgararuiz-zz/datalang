#' Creates html help
#'
#' @description
#'
#' It uses the YAML spec file to create a single help entry.  Its output is a text vector, with 'HTML' content.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left empty.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' create_html_help(my_spec)
#' @export
create_html_help <- function(spec_path) {

  is.readable(spec_path)

  spec <- read_yaml(spec_path)

  h <- paste0("<body style='font-family:arial;'>")
  h <- c(h, paste0("<h1>", spec$help$title ,"</h1>"))
  h <- c(h, paste0("<h3>", spec$help$description ,"</h3>"))

  h <- c(h, paste0("<table>"))
  items <- NULL
  items <- lapply(
    spec$variables, function(x){
      variable <- x["trans"]
      if (variable == "TRUE") variable <- "y"
      paste0("<tr><td>", variable, "</td><td>", x["desc"], "</td></td>")
    }
  )
  items <- as.character(items)
  h <- c(h, items)
  h <- c(h, paste0("</table>"))
  h <- c(h, paste0("</body>"))


  rd <- c(h)
  as.character(rd)
}

datalang_context <- new.env(parent = emptyenv())

datalang_context$help <- NULL

datalang_help_current <- function() datalang_context$help

#' Adds help entry to the current R session
#'
#' It is a mechanism to track which data sets, or functions, to display via the Help pane,
#' and which to build and display on demand.
#'
#' @param obj The object's name to be tracked
#' @param spec_path The location of the YAML spec file
#' @param package Name of the "host" package. Optional.
#'
#' @export
datalang_help_add <- function(obj, spec_path, package = NULL){

  is.readable(spec_path)

  old <- datalang_context$help
  item <- list(
    object = obj,
    spec_path = spec_path,
    package = package
  )
  if(is.null(datalang_context$help)){
    datalang_context$help <- list(item )
  } else {
    datalang_context$help <- list(datalang_context$help, item)
  }
  invisible(old)
}

#' Wrapper for 'help()' function
#'
#' @description
#'
#' Prevents unnecessary rd files to be shipped with the "host" package.  It builds and displays the data set
#' help, but it does it in the RStudio Viewer pane, as oppossed to the Help pane.  If the requested topic does
#' not match to thel help entries available via 'datalang', then 'view_help()' will forward the topic to
#' the regular 'help()' function.
#'
#' @param topic A quoted or unquoted name of the data set or function
#'
#' @export
view_help <- function(topic){
  expr_topic <- enexpr(topic)

  dh <- datalang_help_current()
  found <- lapply(dh, function(x)x$object == expr_topic)
  found <- as.logical(found)

  if(sum(found) > 0) {
    dh_topic <- dh[found][[1]]
    ht <- datalang::create_html_help(dh_topic$spec_path)
    hs <- file.path(tempdir(), "help.html")
    writeLines(ht, hs)
    rstudioapi::viewer(hs)
  } else {
    if(class(expr_topic) != "character"){
      expr_topic <- as.character(expr_topic)
    }
    utils::help(topic = expr_topic)
  }
}


