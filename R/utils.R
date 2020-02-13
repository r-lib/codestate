on_exit <- function(code, env = caller_env()) {
  code <- enexpr(code)
  exit <- expr(on.exit(!!code, add = TRUE))

  eval_bare(exit, env)
}
