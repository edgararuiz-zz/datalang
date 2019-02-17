create_this_week <- function(path = "data") {
  thisweek <- data.frame(
    day = c("friday", "saturday", "sunday"),
    morning = c(76, 71, 70),
    afternoon = c(88, 85, 83)
  )
  save(thisweek, file = file.path(path, "thisweek.rda"))
}

get_messages <- function(language = "es") {
  path <- system.file("messages", package = "datalang")
  msgs <- list.files(path)
  msgs <- msgs[msgs == paste0(language, ".yml")]
  spec <- file.path(path, msgs)
  if (length(msgs) == 0) spec <- file.path(path, "en.yml")
  yaml::read_yaml(spec)
}

create_help_function <- function(name) {
  f <- new_function(
    alist(... = ),
    quote(datalang::datalang_help(...))
  )
  f <- list(f)
  names(f) <- name
  env_bind(base_env(), !!!f)
}

pad_str <- function(x, len, fill = " ") {
  pad <- len - nchar(x)
  pad <- paste0(rep(fill, pad), collapse = "")
  paste0(x, pad)
}

get_item <- function(list_object, item, type = c("character", "integer")) {
  it <- lapply(
    list_object,
    function(x) x[[item]]
  )
  type <- type[1]
  if (type == "character") it <- as.character(it)
  if (type == "integer") it <- as.integer(it)
  it
}
