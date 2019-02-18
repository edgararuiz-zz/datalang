#' Translates a data set
#'
#' @description
#'
#' It uses a YAML file as the spec to translate a data set.
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
  spec <- get_spec(spec_path)

  if (spec$type != "data") stop("Not a valid data set spec")

  if (is.null(.data)) {
    df <- parse_expr(spec$source)
    df <- eval(df)
  } else {
    df <- .data
  }

  was_tibble <- is_tibble(df)
  df <- as_tibble(df)

  vars <- spec$variables

  dfl <- lapply(
    seq_along(vars),
    function(x) {
      curr <- vars[[x]]
      cl <- df[, curr$name][[1]]
      from <- names(curr$values)
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
      colnames(cl) <- curr$trans
      cl
    }
  )

  dfl <- as.data.frame(dfl)

  colnames(dfl) <- sapply(
    seq_along(vars),
    function(x) { vars[[x]]$trans }
  )

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
  spec <- get_spec(spec_path = spec_path)

  df_name <- spec$name

  df <- translate_data(spec_path)

  assign(df_name, df)
  save(
    list = df_name,
    file = paste0(data_folder, "/", df_name, ".rda"),
    compress = "xz"
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
#' @param package Name of the package as a character variable. It is used in the help tracker.
#'
#' @examples
#' library(datalang)
#' my_spec <- system.file("specs/thisweek.yml", package = "datalang")
#' load_translation(my_spec)
#' @export
load_translation <- function(spec_path, envir = baseenv(), package = NULL) {
  spec <- get_spec(spec_path = spec_path)

  df <- translate_data(spec_path = spec_path)

  assign(
    x = spec$name,
    value = df,
    envir = envir
  )

  datalang_help_add(
    obj = spec$name,
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
#' @param verbose If TRUE, it returns a list object with specs that where processed
#' @param envir The target environment where the translated data set will be loaded to. Defaults to the base R environment.
#' @param package Name of the package as a character variable. It is used in the help tracker.
#'
#' @examples
#' library(datalang)
#' my_spec_folder <- system.file("specs", package = "datalang")
#' load_folder_data(my_spec_folder)
#' @export
load_folder_data <- function(spec_folder = "inst/specs", verbose = FALSE,
                             envir = baseenv(), package = NULL) {
  specs <- get_specs_folder(
    spec_folder = spec_folder,
    filter_type = "data"
  )

  paths <- as.character(
    lapply(
      specs,
      function(x) x$path
    )
  )

  invisible(
    lapply(
      paths,
      function(x) {
        load_translation(
          spec_path = x,
          envir = envir,
          package = package
        )
      }
    )
  )

  if (verbose) specs
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
#' @param package Name of the package as a character variable. It is used in the help tracker.
#'
#' @export
load_package_translations <- function(spec_folder = "translations",
                                      verbose = TRUE,
                                      envir = baseenv(),
                                      language = NULL,
                                      package = NULL) {
  if (is.null(language)) language <- Sys.getenv("LANGUAGE")

  if (language != "") {
    lang_folder <- file.path(spec_folder, language)
    if (file.exists(lang_folder)) {
      specs <- get_specs_folder(
        spec_folder = lang_folder
      )

      max_source <- max(as.integer(lapply(specs, function(x) nchar(x$source))))
      max_source <- max_source + 2

      types <- as.character(lapply(specs, function(x) x$type))

      msgs <- get_messages(language)
      startup <- NULL
      startup <- c(startup, paste0(msgs$startup$detected))
      startup <- c(startup, paste0(msgs$startup$datasets, ":"))

      if (any(types == "data")) {
        load_folder_data(
          lang_folder,
          verbose = verbose,
          envir = envir,
          package = package
        )
        items <- specs[types == "data"]
        m <- as.character(
          lapply(
            items,
            function(x) {
              ifelse(x$source == x$name, x$name, paste0(pad_str(x$source, max_source, "-"), "> ", x$name))
            }
          )
        )
        startup <- c(startup, paste0(" ", msgs$startup$data))
        startup <- c(startup, paste0("  ", m))
      }

      if (any(types == "function")) {
        load_folder_functions(
          lang_folder,
          envir = envir,
          verbose = verbose
        )
        items <- specs[types == "function"]
        m <- as.character(
          lapply(
            items,
            function(x) {
              ifelse(x$source == x$name, x$name, paste0(pad_str(x$source, max_source, "-"), "> ", x$name))
            }
          )
        )
        startup <- c(startup, paste0(" ", msgs$startup$functions))
        startup <- c(startup, paste0("  ", m))
      }

      create_help_function(name = msgs$help$name)
      startup <- c(startup, paste0(msgs$help$message, ": ", msgs$help$name, "()"))
      startup <- c(startup, paste0(msgs$help$use, ": ", msgs$help$name, "(", specs[[1]]$name, ")"))


      startup <- paste0(startup, collapse = "\n")
      if (verbose) cat(startup)
    }
  }
}

#' @inherit load_package_translations
#' @export
on_attach <- function(package = NULL,
                      spec_folder = system.file("translations", package = package),
                      envir = as.environment(paste0("package:", package)),
                      language = NULL,
                      verbose = TRUE) {
  load_package_translations(
    spec_folder = spec_folder,
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
  specs <- get_specs_folder(
    spec_folder = spec_folder,
    filter_type = "data"
  )

  paths <- as.character(
    lapply(
      specs,
      function(x) x$path
    )
  )

  lapply(
    paths,
    function(x) {
      save_translation(spec_path = x, data_folder = data_folder)
    }
  )
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
