track <- function(expr) {
  expr <- enquo(expr)

  track_code(quo_get_expr(expr), quo_get_env(expr))
}

track_code <- function(expr, env) {
  local_acc()

  local_trace(getOption, (!!acc_add)("get", "option", x))
  # local_trace(Sys.getenv, (!!acc_add)("get", "envvar", x))

  dot_names <- expr(names(substitute(alist(...)))[-1])
  local_trace(options, (!!acc_add)("set", "option", !!dot_names))
  # local_trace(Sys.setenv, (!!acc_add)("set", "envar", !!dot_names))

  old <- capture_state()
  eval_bare(expr, watch_env(env))
  new <- capture_state()

  if (!identical(old$seed, new$seed)) {
    acc_add("set", "seed", NA)
    acc_add("get", "seed", NA)
  }

  list(
    expr = expr,
    env = env,
    tracking = acc_data(),
    state = list(old = old, new = new)
  )
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

capture_state <- function() {
  list(
    wd = getwd(),
    search = search(),
    options = options(),
    seed = random_seed()
  )
}
