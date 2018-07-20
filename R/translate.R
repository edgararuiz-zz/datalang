
translate_column <- function(column, from, to) {
  cl <- column[[1]]
  if ("factor" %in% class(cl)) {
    lv <- levels(cl)
    lv[lv == from] <- to
    levels(cl) <- lv
  }
  cl[cl == from] <- to
  data.frame(cl)
}

#' @export
#' @importFrom magrittr %>%
translate_data <- function(df = NULL, spec = NULL) {
  if (is.null(df)) stop("Please provide a data.frame")
  if (is.null(spec)) stop("Please provide the path of a spec")

  spec <- yaml::read_yaml(spec)
  purrr::imap(spec$variables, ~{
    col <- df[, .y]
    if (!is.null(.x$values)) {
      for (i in 1:length(.x$values)) {
        val <- names(.x$values[i])
        col <- translate_column(col, val, .x$values[[i]])
      }
    }
    names(col) <- .x["trans"]
    col
  }) %>%
    dplyr::bind_cols()

}

#' @export
create_rd <- function(df = NULL, spec = NULL) {
  if (is.null(df)) stop("Please provide a data.frame")
  if (is.null(spec)) stop("Please provide the path of a spec")

  spec <- yaml::read_yaml(spec)

  h <- list(
    "\\docType{data}",
    if(!is.null(spec$help$name)) paste0("\\name{", spec$help$name,"}"),
    if(!is.null(spec$help$alias)) paste0("\\alias{", spec$help$alias,"}"),
    if(!is.null(spec$help$title)) paste0("\\title{", spec$help$title,"}"),
    if(!is.null(spec$help$format)) paste0("\\format{", spec$help$format),
    "\\describe{"
  )

  i <- NULL
  i <- purrr::map_chr(spec$variables, ~{
    paste0("\\item{", .x["trans"], "}{", .x["desc"], "}")
  })
  names(i) <- NULL

  b <- list(
    "}}",
    if(!is.null(spec$help$usage)) paste0("\\usage{", spec$help$usage,"}"),
    if(!is.null(spec$help$description)) paste0("\\description{", spec$help$description,"}"),
    "\\keyword{datasets}"
  )

  h <- c(h, i, b)

  #writeLines(as.character(h), paste0("man/", spec$help$name, ".rd"))
  as.character(h)
}

