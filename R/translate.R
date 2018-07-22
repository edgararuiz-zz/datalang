
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
translate_data <- function(df = NULL, spec_path = NULL) {

  if (is.null(df)) stop("Please provide a data.frame")
  if (is.null(spec_path)) stop("Please provide the path of a spec_path")

  spec <- yaml::read_yaml(spec_path)
  vars <- imap(spec$variables, ~{
    col <- df[, .y]
    vals <- .x$values
    if (!is.null(vals)) {
      for (i in 1:length(vals)) {
        val <- names(vals[i])
        col <- translate_column(col, val, vals[[i]])
      }
    }
    names(col) <- .x["trans"]
    col
  })
  bind_cols(vars)
}

#' @export
create_rd <- function(df = NULL, spec_path = NULL) {

  if (is.null(df)) stop("Please provide a data.frame")
  if (is.null(spec_path)) stop("Please provide the path of a spec_path")

  spec <- yaml::read_yaml(spec_path)

  header <- list(
    "\\docType{data}",
    if(!is.null(spec$help$name)) paste0("\\name{", spec$help$name,"}"),
    if(!is.null(spec$help$alias)) paste0("\\alias{", spec$help$alias,"}"),
    if(!is.null(spec$help$title)) paste0("\\title{", spec$help$title,"}"),
    if(!is.null(spec$help$format)) paste0("\\format{", spec$help$format),
    "\\describe{"
  )

  items <- NULL
  items <- map_chr(
    spec$variables,
    ~ paste0("\\item{", .x["trans"], "}{", .x["desc"], "}")
    )
  names(items) <- NULL

  footer <- list(
    "}}",
    if(!is.null(spec$help$usage)) paste0("\\usage{", spec$help$usage,"}"),
    if(!is.null(spec$help$description)) paste0("\\description{", spec$help$description,"}"),
    "\\keyword{datasets}"
  )
  rd <- c(header, items, footer)
  as.character(rd)
}

save_translation <- function(df, spec, name, data_folder) {
  name <- enexpr(name)
  name_char <- as.character(name)
  e <- global_env()
  e[[name_char]] <- translate_data(df, spec)
  save(list = name_char, file = paste0(data_folder, "/", name_char, ".rda"))
  rm(list = name_char, envir = e)
}

#' @export
translate_folder <- function(spec_folder = "inst/specs", data_folder = "data", rd_folder = "man"){

  specs <- list.files(spec_folder, pattern = "yml")

  walk(specs, ~{

    spec_path <- file.path(spec_folder, .x)

    spec <- yaml::read_yaml(spec_path)

    df <- parse_expr(spec$df$source)
    df <- eval(df)

    name <- parse_expr(spec$df$name)

    if(!is.null(data_folder)){
      save_translation(
        df =   df,
        spec = spec_path,
        name = !!name,
        data_folder = data_folder
      )
    }

    if(!is.null(rd_folder)){
      if(!is.null(spec$help)){
        rd <- create_rd(df, spec_path)
        writeLines(rd, con = file.path(rd_folder, paste0(spec$df$name, ".rd")))

      }
    }
    }
  )
}


