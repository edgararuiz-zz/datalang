#' Translates a data set
#'
#' @description
#'
#' This is the main translation function in 'datalang'.  It uses a YAML file as the spec to translate a data set.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left empty.
#' @param df A tibble or data.frame object that overrides the one specified in the spec file. Defaults to NULL.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/diamonds-es.yml", package = "datalang")
#' translate_data(my_spec)
#' @export
translate_data <- function(spec_path, df = NULL) {
  is.readable(spec_path)

  spec <- read_yaml(spec_path)

  if (is.null(df)) {
    df <- parse_expr(spec$df$source)
    df <- eval(df)
  }

  vars <- spec$variables
  var_names <- names(vars)
  vars_TRUE <- var_names == "TRUE"
  if(sum(vars_TRUE) > 0){
    if(vars[vars_TRUE][[1]]$trans == "TRUE"){
      vars[vars_TRUE][[1]]$trans <- "y"
    }
    var_names[vars_TRUE]  <- "y"
  }

  dfl <- lapply(
    seq_along(vars),
    function(x) {
      cl <- df[, var_names[x]][[1]]
      from <- names(vars[[x]]$values)
      to <- as.character(vars[[x]]$values[from])
      if (!is.null(from)) {
        if ("factor" %in% class(cl)) {
          lv <- levels(cl)
          for (i in seq_along(from)) {
            lv[lv == from[i]] <- to[i]
          }
          levels(cl) <- lv
        } else {
          for (i in seq_along(from)) {
            cl[cl == from[i]] <- to[i]
          }
        }
      }

      cl <- as.data.frame(cl)
      colnames(cl) <- var_names[x]
      cl
    }
  )
  dfl <- as.data.frame(dfl)
  if (is_tibble(df)) dfl <- as_tibble(dfl)
  dfl
}

#' Translates and saves a data set
#'
#' @description
#'
#' Saves a translated version of the data set in 'rda' format.  It wraps the translate_data()
#' function, and saves the output.
#'
#' This function is meant for packages that will ship with a copy of the translated data set.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left NULL.
#' @param data_folder The target folder location where the 'rda' file will be save to.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/diamonds-es.yml", package = "datalang")
#' save_translation(my_spec, tempdir())
#' @export
save_translation <- function(spec_path, data_folder = "data") {
  is.readable(spec_path)

  spec <- read_yaml(spec_path)

  df_name <- spec$df$name

  df <- translate_data(spec_path)

  assign(df_name, df)
  save(
    list = df_name,
    file = paste0(data_folder, "/", df_name, ".rda")
  )
}

#' Translates and loads a data set into an R environment
#'
#' @description
#'
#' Translates and loads the translation to a specified R environment.
#'
#' This function is meant for packages that will do not wish to ship a copy of the data set, and
#' wish to execute the translation on the fly, preferably when the "host" package is attached.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left empty.
#' @param envir The target environment where the translated data set will be loaded to. Defaults to the base R environment.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/diamonds-es.yml", package = "datalang")
#' load_translation(my_spec)
#' @export
load_translation <- function(spec_path, envir = baseenv(), ...) {
  is.readable(spec_path)
  spec <- read_yaml(spec_path)
  df <- translate_data(spec_path)
  assign(
    x = spec$df$name,
    value = df,
    envir = envir
  )

  datalang_help_add(
    obj = spec$df$name,
    spec_path = spec_path
  )
}

#' Translates and loads multiple a data sets into an R environment
#'
#' @description
#'
#' Cycles through all of the spec files inside a folder specified in the 'spec_folder' argument.
#' The function translates and loads each new data set to a specified R environment.
#'
#' This function is meant for packages that will do not wish to ship a copy of the data set, and
#' wish to execute the translation on the fly, preferably when the "host" package is attached.
#'
#' @param spec_folder The path to the folder where the YAML spec files are located. Defaults to 'inst/specs'.
#' @param verbose Prints to the console the name of the original data set, and the name of the new translated data set.
#' @param envir The target environment where the translated data set will be loaded to. Defaults to the base R environment.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("specs", package = "datalang")
#' load_folder_data(my_spec_folder)
#' @export
load_folder_data <- function(spec_folder = "inst/specs",
                             verbose = FALSE, envir = baseenv(), ...) {
  is.readable(spec_folder)

  specs <- file.path(spec_folder, list.files(spec_folder))

  invisible({
    lapply(specs, function(x) {
      load_translation(x, envir = envir)
      if (verbose) {
        spec <- read_yaml(x)
        cat("    ", spec$df$source, " >-> ", spec$df$name, "\n")
      }
    })
  })
}

#' Translates and loads multiple a data sets into an R environment
#'
#' @description
#'
#' Cycles through all of the spec files inside a folder.  The default behaivor is if a language
#' setting is detected ('Sys.getenv("LANGUAGE")'), it will look for a sub-folder that matches the
#' setting inside the 'spec_folder' path.
#'
#' The function translates and loads each new data set to a specified R environment.
#'
#' This function is meant for packages that will do not wish to ship a copy of the data set, and
#' wish to execute the translation on the fly, preferably when the "host" package is attached.
#'
#' @param spec_folder The path to the folder where the YAML spec files are located. Defaults to 'inst/specs'.
#' @param verbose Prints to the console the name of the original data set, and the name of the new translated data set.
#' @param envir The target environment where the translated data set will be loaded to. Defaults to the base R environment.
#' @param language Optional argument that allows the target language to be specified.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("translations", package = "datalang")
#' load_package_translations(my_spec_folder, language = "es")
#' @export
load_package_translations <- function(spec_folder = "translations",
                                      verbose = TRUE,
                                      envir = baseenv(),
                                      language = NULL) {
  is.readable(spec_folder)


  if (is.null(language)) language <- Sys.getenv("LANGUAGE")

  if (language != "") {
    lang_folder <- file.path(spec_folder, language)
    if (file.exists(lang_folder)) {
      cat("Language setting detected:", language, "\n")
      cat("  Loading available translated datasets \n")
      load_folder_data(
        lang_folder,
        verbose = verbose,
        envir = envir
      )
    }
  }
}

#' @export
folder_data <- function(spec_folder = "inst/specs", data_folder = "data") {
  is.readable(spec_folder)
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
