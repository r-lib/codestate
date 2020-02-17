side_effects <- function(chunk) {
  td <- chunk$tracking
  state <- chunk$state

  out <- list()

  # Bindings are potentially large, so we want to save in .RDS
  out$bindings <- td$value[td$action == "set" & td$name == "binding"]
  out$bindings <- intersect(out$bindings, env_names(chunk$env))

  if (!identical(state$old$seed, state$new$seed)) {
    out$seed <- state$new$seed
  }

  options_set <- td$value[td$action == "set" & td$name == "option"]
  out$options <- list_diff(state$old$options[options_set], state$new$options[options_set])

  if (!identical(state$old$wd, state$new$wd)) {
    out$wd <- state$new$wd
  }

  if (!identical(state$old$search, state$new$search)) {
    new_packages <- setdiff(state$new$search, state$old$search)
    new_packages <- sub("^package:", "", new_packages)
    out$packages <- new_packages
  }

  out
}
