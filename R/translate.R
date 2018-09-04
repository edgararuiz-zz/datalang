#' Translates a data set
#'
#' @description
#'
#' This is the main translation function in 'datalang'.  It uses a YAML file as the spec to translate a data set.
#'
#' @param spec_path The file location of the YAML spec translation file.  It is a required argument, cannot be left empty.
#' @param .data A tibble or data.frame object that overrides the one specified in the spec file. Defaults to NULL.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' translate_data(my_spec)
#' @export
translate_data <- function(spec_path, .data = NULL) {
  is.readable(spec_path)

  spec <- read_yaml(spec_path)

  if (is.null(.data)) {
    df <- parse_expr(spec$df$source)
    df <- eval(df)
    if("function" %in% class(df)) return()
  } else {
    df <- .data
  }

  if(is_tibble(df)){
    was_tibble <- TRUE
  } else {
    was_tibble <- FALSE
    df <-as_tibble(df)
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

  new_names <- as.character(lapply(vars, function(x)x$trans))

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
      colnames(cl) <- new_names[x]
      cl
    }
  )
  dfl <- as.data.frame(dfl)
  if (was_tibble) dfl <- as_tibble(dfl)
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
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
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
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' load_translation(my_spec)
#' @export
load_translation <- function(spec_path, envir = baseenv(), package = NULL) {
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
    spec_path = spec_path,
    package = package
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
load_folder_data <- function(spec_folder = "inst/specs", verbose = FALSE,
                             envir = baseenv(), package = NULL
                             ) {
  is.readable(spec_folder)

  specs <- file.path(spec_folder, list.files(spec_folder))

  invisible({
    lapply(specs, function(x) {
      tr <- load_translation(x,
                             envir = envir,
                             package = package
                             )
      if (verbose) {
        spec <- read_yaml(x)
        if(is.null(tr)){
          cat("    ", spec$df$source, " >-> ", spec$df$name, "\n")
        } else {
          cat("    ", spec$df$source, "\n")
        }

      }
      tr
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
                                      language = NULL,
                                      package = NULL) {
  is.readable(spec_folder)

  if (is.null(language)) language <- Sys.getenv("LANGUAGE")

  if (language != "") {
    lang_folder <- file.path(spec_folder, language)
    if (file.exists(lang_folder)) {

      first_file <- list.files(lang_folder)[[1]]
      first_file <- file.path(lang_folder, first_file)
      first_file <- yaml::read_yaml(first_file)
      first_file <- first_file$df$name


      msgs <- get_messages(language)
      cat(msgs$startup$detected, " \n")
      cat(
        " ",
        msgs$startup$datasets,
        ": \n"
        )
      load_folder_data(
        lang_folder,
        verbose = verbose,
        envir = envir,
        package = package
      )

      create_help_function(
        name = msgs$help$name,
        message = msgs$help$message,
        usage = msgs$help$use,
        example = first_file
        )
    }
  }
}

#' @export
on_attach <- function(package = NULL,
                      spec_folder = system.file("translations", package = package),
                      envir = as.environment(paste0("package:", package)),
                      language = NULL,
                      verbose = TRUE
                      ) {
  load_package_translations(
    spec_folder =  spec_folder,
    envir = envir,
    package = package,
    language = language
  )
}

#' Translates and saves multiple a data sets
#'
#' @description
#'
#' Cycles through all of the spec files inside a folder specified in the 'spec_folder' argument.
#' The function translates and saves them to a specified folder.
#'
#' Saves a translated versions of the data set in 'rda' format.  It wraps the translate_data()
#' function, and saves the output.
#'
#' This function is meant for packages that will ship with a copy of the translated data set.
#'
#' @param spec_folder The path to the folder where the YAML spec files are located. Defaults to 'inst/specs'.
#' @param data_folder The target folder location where the 'rda' file will be save to.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("specs", package = "datalang")
#' folder_data(my_spec_folder, tempdir())
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

#' Translates and saves multiple a data sets
#'
#' @description
#'
#' Cycles through all of the spec files inside a folder specified in the 'spec_folder' argument.
#' The function translates and saves them to a specified folder. It also creates and saves the
#' help files.
#'
#' Saves a translated versions of the data set in 'rda' format.
#'
#' This function is meant for packages that will ship with a copy of the translated data set.
#'
#' @param spec_folder The path to the folder where the YAML spec files are located. Defaults to 'inst/specs'.
#' @param data_folder The target folder location where the 'rda' file will be saved to.
#' @param rd_folder The target folder location where the help files will be saved to.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("specs", package = "datalang")
#' folder_data(my_spec_folder, tempdir())
#' @export
translate_folder <- function(spec_folder = "inst/specs",
                             data_folder = "data", rd_folder = "man") {
  folder_data(spec_folder, data_folder)
  folder_rd(spec_folder, rd_folder)
}
