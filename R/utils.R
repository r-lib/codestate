on_exit <- function(code, env = caller_env()) {
  code <- enexpr(code)
  exit <- expr(on.exit(!!code, add = TRUE))

  eval_bare(exit, env)
}

local_trace <- function(fun, tracer, env = caller_env()) {
  tracer <- enexpr(tracer)
  fun <- enexpr(fun)

  suppressMessages(trace(fun, tracer, print = FALSE, where = env))
  on_exit(suppressMessages(untrace(!!fun)), env = env)
}

find_binding <- function(name, env, in_package = FALSE) {
  if (identical(env, empty_env())) {
    stop("object '", name, "' not found", call. = FALSE)
  } else if (env_has(env, name)) {
    list(env = env, val = env_get(env, name), in_package = in_package)
  } else {
    if (identical(env, global_env()) || identical(env, asNamespace("chonky"))) {
      in_package <- TRUE
    }
    find_binding(name, env_parent(env), in_package)
  }
}

recycle <- function(x, to, arg = deparse(substitute(x))) {
  if (length(x) == length(to)) {
    return(x)
  }

  if (length(x) != 1) {
    stop("Can't recycle `", arg, "` to length ", length(to), call. = FALSE)
  }

  rep(x, length(to))
}
