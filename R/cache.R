cache <- function(expr) {
  expr <- enquo(expr)
  cache_chunk(quo_get_expr(expr), quo_get_env(expr))
}

cache_chunk <- function(code, env) {
  old <- capture_state()
  tracking <- track_code(code, env)
  new <- capture_state()

  effects <- side_effects(tracking, old, new)
  bindings <- env_get_list(env, effects$bindings, default = zap())
  effects_hash <- digest::digest(list(effects, bindings), algo = "murmur32")

  new_cached_chunk(
    code = code,
    tracking = tracking,
    effects = effects,
    bindings = bindings,
    effects_hash = effects_hash,
    dependencies = NULL
  )
}

# tracking ----------------------------------------------------------------

track_code <- function(expr, env) {
  local_acc()

  local_trace(getOption, (!!acc_add)("get", "option", x))
  # local_trace(Sys.getenv, (!!acc_add)("get", "envvar", x))

  dot_names <- expr(names(substitute(alist(...)))[-1])
  local_trace(options, (!!acc_add)("set", "option", !!dot_names))
  # local_trace(Sys.setenv, (!!acc_add)("set", "envar", !!dot_names))

  old_seed <- random_seed()
  eval_bare(expr, watch_env(env))
  new_seed <- random_seed()

  if (!identical(old_seed, new_seed)) {
    acc_add("set", "seed", NA)
    acc_add("get", "seed", NA)
  }

  acc_data()
}

watch_env <- function(env) {
  created <- character()

  objectable::object_table(
    parent_env = env,
    get = function(name) {
      binding <- find_binding(name, env)

      if (!binding$in_package && !name %in% created) {
        acc_add("get", "binding", name)
      }

      binding$val
    },

    set = function(name, value) {
      created <<- union(created, name)
      acc_add("set", "binding", name)
      env_poke(env, name, value)
    }
  )
}


# side-effects ------------------------------------------------------------

side_effects <- function(tracking, old, new) {
  out <- list()

  out$bindings <- tracking$value[tracking$action == "set" & tracking$name == "binding"]

  out$seed <- if (!identical(old$seed, new$seed)) new$seed

  options_set <- tracking$value[tracking$action == "set" & tracking$name == "option"]
  out$options <- list_diff(old$options[options_set], new$options[options_set])

  out$wd <- if (!identical(old$wd, new$wd)) new$wd

  new_packages <- setdiff(new$search, old$search)
  new_packages <- sub("^package:", "", new_packages)
  out$packages <- new_packages

  out
}

capture_state <- function() {
  list(
    wd = getwd(),
    search = search(),
    options = options(),
    seed = random_seed()
  )
}
