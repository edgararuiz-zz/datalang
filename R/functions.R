#' @export
load_function <- function(spec_path, envir = baseenv()){

  is.readable(spec_path)

  spec <- read_yaml(spec_path)

  if(is.null(spec$df$type)){
    return()
  } else {
    if(spec$df$type != "function") return()
    }

  a <- formals(
    eval(
      parse_expr(paste0(spec$df$package, "::", spec$df$source))
      #parse_expr(spec$df$source)
    )
  )

  trans <- lapply(names(a), function(x){
    sp <-spec$variables[x]
    t <- sp[[1]]["trans"]
    if(as.character(t) == "NULL"){
      x
    } else {
      t[[1]]
    }
  })

  trans <- as.character(trans)
  names(a) <- trans

  b <- lapply(seq_along(spec$variables), function(x){
    a <- names(spec$variables[x])
    t <- spec$variables[x][[1]]$trans
    if(is.null(t)) t <- a
    paste0(a, " = ", t)
  })
  b <- as.character(b)
  b <- paste0(b, collapse = ", ")
  b <- paste0(spec$df$package, "::", spec$df$source, "(", b, ")")
  #b <- paste0(spec$df$source, "(", b, ")")
  b <- parse_expr(b)

  f <- new_function(
    eval(a),
    expr(!! b),
    envir
  )

  f <- list(f)
  names(f) <- spec$df$name
  env_bind(
    envir,
    !!! f
  )

}

#' @export
load_folder_functions <- function(spec_folder = "inst/specs", verbose = FALSE,
                                  envir = baseenv(), package = NULL
) {
  is.readable(spec_folder)

  specs <- file.path(spec_folder, list.files(spec_folder))

  invisible({
    lapply(specs, function(x) {
      load_function(x, envir = envir)
      if (verbose) {
        spec <- read_yaml(x)
        cat("    f -> ", spec$df$source, " >-> ", spec$df$name, "\n")
      }
    })
  })
}
