#' @export
load_function <- function(spec_path, envir = baseenv(), package = NULL){

  spec <- get_spec(spec_path = spec_path)

  if(spec$type != "function") stop("Not a valid function spec")

  if(is.null(package)) package <- spec$package

  fun_name <- spec$source
  if(!is.null(package)) fun_name <- paste0(package, "::", fun_name)

  a <- formals(eval(parse_expr(fun_name)))

  a_names <- as.character(lapply(spec$variables, function(x) x$name))
  a_trans <- as.character(lapply(spec$variables, function(x) x$trans))

  trans <- as.character(lapply(names(a), function(x) a_trans[x == a_names]))

  names(a) <- trans

  b <- lapply(
    spec$variables,
    function(x){
      paste0(x$name, " = ", x$trans)
    }
  )
  b <- as.character(b)
  b <- paste0(b, collapse = ", ")
  b <- paste0(fun_name, "(", b, ")")
  b <- parse_expr(b)

  f <- new_function(
    eval(a),
    expr(!! b),
    envir
  )

  f <- list(f)
  names(f) <- spec$name
  env_bind(
    envir,
    !!! f
  )

}

#' @export
load_folder_functions <- function(spec_folder = "inst/specs", verbose = FALSE,
                                  envir = baseenv(), package = NULL
) {

  specs <- get_specs_folder(
    spec_folder = spec_folder,
    filter_type = "function"
  )

  paths <- as.character(
    lapply(
      specs,
      function(x)x$path
    )
  )

  lapply(
    paths,
    function(x){
      load_function(
        spec_path = x,
        envir = envir,
        package = package
      )
    })

  if(verbose) specs

}
