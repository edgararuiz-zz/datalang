#' Translates and loads a function into an R environment
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
#' @export
load_function <- function(spec_path, envir = globalenv(), package = NULL) {
  spec <- get_spec(spec_path = spec_path)

  if (spec$type != "function") stop("Not a valid function spec")

  if (is.null(package)) package <- spec$package

  fun_name <- spec$source
  if (!is.null(package)) fun_name <- paste0(package, "::", fun_name)

  a <- formals(eval(parse_expr(fun_name)))

  a_names <- get_item(spec$variables, "name")
  a_trans <- get_item(spec$variables, "trans")

  trans <- as.character(lapply(names(a), function(x) a_trans[x == a_names]))

  names(a) <- trans

  b <- lapply(
    spec$variables,
    function(x) {
      paste0(x$name, " = ", x$trans)
    }
  )
  b <- as.character(b)
  b <- paste0(b, collapse = ", ")
  b <- paste0(fun_name, "(", b, ")")
  b <- parse_expr(b)

  f <- new_function(
    eval(a),
    expr(!!b),
    envir
  )

  f <- list(f)
  names(f) <- spec$name
  env_bind(
    envir,
    !!!f
  )

  datalang_help_add(
    obj = spec$name,
    spec_path = spec_path,
    package = package
  )
}

#' Translates and loads multiple a funciton into an R environment
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
load_folder_functions <- function(spec_folder = "inst/specs", verbose = FALSE,
                                  envir = globalenv(), package = NULL) {
  specs <- get_specs_folder(
    spec_folder = spec_folder,
    filter_type = "function"
  )

  paths <- get_item(specs, "path")

  lapply(
    paths,
    function(x) {
      load_function(
        spec_path = x,
        envir = envir,
        package = package
      )
    }
  )

  if (verbose) specs
}
