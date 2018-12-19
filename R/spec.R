get_spec <- function(spec_path) {
  is.readable(spec_path)

  yaml_file <- yaml::read_yaml(spec_path)

  vars <- yaml_file$variables

  variables <- lapply(
    seq_along(vars),
    function(x) {
      curr <- vars[x][[1]]
      name_curr <- names(vars[x])
      v <- list(
        name = name_curr,
        trans = ifelse(is.null(curr$trans), name_curr, curr$trans),
        desc = ifelse(is.null(curr$desc), "", curr$desc),
        has_values = !is.null(curr$values)
      )
      if (v$has_values) v$values <- curr$values
      v
    }
  )

  base <- yaml_file$df
  if (is.null(base)) base <- yaml_file

  spec <- list(
    source = base$source,
    name = base$name,
    package = ifelse(is.null(base$package), "", base$package),
    type = ifelse(is.null(base$type), "data", base$type),
    path = spec_path,
    variables = variables,
    has_help = !is.null(yaml_file$help)
  )

  if (spec$has_help) spec$help <- yaml_file$help

  spec
}

get_specs_folder <- function(spec_folder, filter_type = "") {
  is.readable(spec_folder)

  files <- list.files(spec_folder, pattern = "yml")
  files <- file.path(spec_folder, files)

  specs <- lapply(files, get_spec)

  if (filter_type != "") {
    specs_type <- lapply(specs, function(x) x$type)
    specs <- specs[specs_type == filter_type]
  }
  specs
}
