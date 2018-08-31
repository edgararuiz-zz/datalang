#' @export
create_html_help <- function(spec_path = NULL) {
  if (is.null(spec_path)) stop("Please provide the path of a spec_path")

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

#' @export
datalang_help_add <- function(obj, spec_path, package = NULL){
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


