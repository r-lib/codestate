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
    if (identical(env, global_env()) || identical(env, asNamespace("codestate"))) {
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

random_seed <- function() {
  env_get(global_env(), ".Random.seed", default = zap())
}

list_diff <- function(old, new) {
  changed <- rep(FALSE, length(new))

  for (i in seq_along(new)) {
    nm <- names(new)[[i]]
    changed[[i]] <- !identical(old[[nm]], new[[nm]])
  }

  new[changed]
}

compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

map <- function(.x, .f, ...) {
  lapply(X = .x, FUN = .f, ...)
}
map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, SIMPLIFY = FALSE, MoreArgs = list(...), USE.NAMES = FALSE)
}
map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, FUN.VALUE = character(1), ...)
}

remove_source <- function(x) {
  if (is_closure(x)) {
    body(x) <- remove_source(body(x))
    x
  } else if (is_call(x)) {
    attr(x, "srcref") <- NULL
    attr(x, "wholeSrcref") <- NULL
    attr(x, "srcfile") <- NULL

    x[] <- lapply(x, remove_source)
    x
  } else {
    x
  }
}
