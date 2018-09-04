create_this_week <- function(path = "data"){
  thisweek <- data.frame(
    day = c("friday", "saturday", "sunday"),
    morning = c(76, 71, 70),
    afternoon = c(88, 85, 83)
  )
  save(thisweek, file = file.path(path, "thisweek.rda"))

}

get_messages <- function(language = "es"){
  path <- system.file("messages", package = "datalang")
  msgs <- list.files(path)
  msgs <- msgs[msgs == paste0(language, ".yml")]
  spec <- file.path(path, msgs)
  if(length(msgs) == 0) spec <- file.path(path, "en.yml")
  yaml::read_yaml(spec)
}

create_help_function <- function(name,
                                 message = "New help function added:",
                                 usage = "Usage:",
                                 example = "mtcars"
                                 ){
  f <- new_function(
    alist(... =),
    quote(datalang_help(...))
  )
  f <- list(f)
  names(f) <- name
  env_bind(base_env(), !!! f)
  message <- paste0(message, ":")
  cat("  ", message, paste0(name, "()"), "\n")
  cat("    ", paste0(usage, ": ", name, "(", example,")"), "\n")
}
