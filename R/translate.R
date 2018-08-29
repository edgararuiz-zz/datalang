translate_column <- function(column, from, to) {
  cl <- column[[1]]
  if ("factor" %in% class(cl)) {
    lv <- levels(cl)
    lv[lv == from] <- to
    levels(cl) <- lv
  }
  cl[cl == from] <- to
  names(cl) <- names(column)
  data.frame(cl)
}

#' @export
translate_data <- function(spec_path = NULL, df = NULL) {
  if (is.null(spec_path)) stop("Please provide the path of a spec_path")

  spec <- read_yaml(spec_path)

  if (is.null(df)) {
    df <- parse_expr(spec$df$source)
    df <- eval(df)
  }

  vars <- imap(spec$variables, ~{
    field <- .y
    if (field == "TRUE") field <- "y"
    col <- df[, field]
    vals <- .x$values
    if (!is.null(vals)) {
      for (i in 1:length(vals)) {
        val <- names(vals[i])
        col <- translate_column(col, val, vals[[i]])
      }
    }
    variable <- .x["trans"]
    if (variable == "TRUE") variable <- "y"
    col <- as.data.frame(col)
    names(col) <- variable
    col
  })
  vars <- bind_cols(vars)
  if (is_tibble(df)) vars <- as_tibble(vars)
  vars
}

#' @export
save_translation <- function(spec_path, data_folder = "data") {
  spec <- read_yaml(spec_path)

  df_name <- spec$df$name

  df <- translate_data(spec_path)

  assign(df_name, df)
  save(
    list = df_name,
    file = paste0(data_folder, "/", df_name, ".rda")
  )
}

#' @export
load_translation <- function(spec_path, ...) {
  spec <- read_yaml(spec_path)
  df <- translate_data(spec_path)
  assign(
    x = spec$df$name,
    value = df,
    envir = baseenv()
  )
}

#' @export
load_folder_data <- function(spec_folder = "inst/specs", verbose = FALSE, ...) {
  specs <- file.path(spec_folder, list.files(spec_folder))

  invisible({
    lapply(specs, function(x){
      load_translation(x)
      if(verbose){
        spec <- read_yaml(x)
        cat(spec$df$source, " - New dataset:", spec$df$name, "\n")
      }
    })
  })
}

#' @export
folder_data <- function(spec_folder = "inst/specs", data_folder = "data") {
  specs <- list.files(spec_folder)
  invisible({
    lapply(file.path(spec_folder, specs), function(x) {
      save_translation(x, data_folder = data_folder)
    })
  })
}

#' @export
translate_folder <- function(spec_folder = "inst/specs",
                             data_folder = "data", rd_folder = "man") {
  folder_data(spec_folder, data_folder)
  folder_rd(spec_folder, rd_folder)
}
