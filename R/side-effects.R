side_effects <- function(chunk) {
  td <- chunk$tracking
  state <- chunk$state

  # Bindings are potentially large, so we want to save in .RDS
  binding_set <- td$value[td$action == "set" & td$name == "binding"]
  binding_set <- intersect(binding_set, env_names(chunk$env))
  bindings <- env_get_list(chunk$env, binding_set)

  if (!identical(state$old$seed, state$new$seed)) {
    bindings$.Random.seed <- state$new$seed
  }

  # All other side-effects are short values, so we want to record in .yaml
  effects <- list()

  effects$bindings <- binding_set

  options_set <- td$value[td$action == "set" & td$name == "option"]
  effects$options <- list_diff(state$old$options[options_set], state$new$options[options_set])

  if (!identical(state$old$wd, state$new$wd)) {
    effects$wd <- state$new$wd
  }

  if (!identical(state$old$search, state$new$search)) {
    new_packages <- setdiff(state$new$search, state$old$search)
    new_packages <- sub("^package:", "", new_packages)
    effects$packages <- new_packages
  }

  list(bindings = bindings, effects = effects)
}
