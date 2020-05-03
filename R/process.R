process <- function(..., .env = caller_env(), .cache = list()) {
  dots <- enexprs(...)
  names(dots) <- chunk_names(names(dots))

  chunks <- rep_named(names(dots), list(NULL))
  cache <- .cache

  for (nm in names(dots)) {
    chunks[[nm]] <- chunk_eval(dots[[nm]], env, cache[[nm]], compact(chunks))
  }

  chunks
}

chunk_eval <- function(code, env, cached, chunks) {
  if (chunk_invalid(code, cached, chunks)) {
    out <- chunk_cache(code, env)
    out$dependencies <- chunk_dependencies(out, chunks)
    out
  } else {
    chunk_replay(cached, env)
  }
}

chunk_replay <- function(chunk, env) {
  env_bind(env, !!!chunk$bindings)

  if (!is.null(chunk$effects$seed)) {
    env_bind(globalenv(), .Random.seed = chunk$effects$seed)
  }

  if (!is.null(chunk$effects$options)) {
    options(chunk$effects$options)
  }

  if (!is.null(chunk$effects$wd)) {
    setwd(chunk$effects$wd)
  }

  if (!is.null(chunk$effects$packages)) {
    lapply(chunk$effects$packages, library, character.only = TRUE)
  }

  chunk
}

chunk_invalid <- function(code, cached, chunks) {
  # Chunk isn't cached
  if (is.null(cached)) {
    return(TRUE)
  }

  # Code inside chunk has changed
  if (!identical(remove_source(code), remove_source(cached$code))) {
    return(TRUE)
  }

  # Dependencies have changed
  dependencies <- chunk_dependencies(cached, chunks)
  if (!setequal(dependencies$chunk, cached$dependencies$chunk)) {
    return(TRUE)
  }

  # Side-effects of a dependency have changed
  old_hash <- cached$dependencies$effects_hash
  new_hash <- map_chr(cached$dependencies$chunk, function(x) chunks[[x]]$effects_hash %||% "NO CACHE")
  any(old_hash != new_hash)
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
