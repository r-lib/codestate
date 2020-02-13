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

