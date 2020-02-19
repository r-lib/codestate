process <- function(..., .env = caller_env()) {
  dots <- enexprs(...)
  chunks <- lapply(dots, cache_chunk, env = .env)
  names(chunks) <- chunk_names(names(dots))

  add_dependencies(chunks)
}

chunk_names <- function(x) {
  ifelse(x == "", paste0("chunk", seq_along(x)), x)
}
