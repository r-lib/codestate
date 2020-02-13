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
      if (!env_has(env, name, inherit = TRUE)) {
        stop("object '", name, "' not found", call. = FALSE)
      }

      # Track symbols above the global environment
      # This isn't quite right because the symbol might also be found earlier -
      # probably need to implement inheritance manually so I can do this
      # correctly
      pkg_env <- env_parent(global_env())
      if (!env_has(pkg_env, name, inherit = TRUE)) {
        acc_add("get", "binding", name)
      }

      env_get(env, name, inherit = TRUE)
    },

    set = function(name, value) {
      acc_add("set", "binding", name)
      env_poke(env, name, value)
    }
  )
}
