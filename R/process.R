process <- function(..., .env = caller_env()) {
  dots <- enexprs(...)
  chunks <- lapply(dots, cache_chunk, env = .env)
  names(chunks) <- chunk_names(names(dots))

  add_dependencies(chunks)
}

chunk_names <- function(x) {
  ifelse(x == "", paste0("chunk", seq_along(x)), x)
}

chunks_write <- function(chunks, path) {
  paths <- fs::path(path, names(chunks), ext = "qs")
  invisible(map2(chunks, paths, qs::qsave))
}

chunks_read <- function(path) {
  dir <- fs::dir_ls(path, glob = "*.qs")
  names <- fs::path_ext_remove(fs::path_file(dir))

  set_names(map(dir, qs::qread), names)
}
