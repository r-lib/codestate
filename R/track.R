track <- function(expr) {
  expr <- enquo(expr)

  track_code(quo_get_expr(expr), quo_get_env(expr))
}
track_code <- function(expr, env) {
  local_acc()

  local_trace(getOption, (!!acc_add)("get", "option", x))
  dot_names <- expr(names(substitute(alist(...)))[-1])
  local_trace(options, (!!acc_add)("set", "option", !!dot_names))

  eval_bare(expr, watch_env(env))
  acc_data()
}

watch_env <- function(env) {
  objectable::object_table(
    parent_env = env,
    get = function(name) {
      binding <- find_binding(name, env)

      if (!binding$in_package) {
        acc_add("get", "binding", name)
      }

      binding$val
    },

    set = function(name, value) {
      acc_add("set", "binding", name)
      env_poke(env, name, value)
    }
  )
}

