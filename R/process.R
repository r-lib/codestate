process <- function(...) {
  dots <- enquos(...)
  chunks <- lapply(dots, track)

  out <- rep_named(names(dots), list())

  out <- map2(out, dots, add_code)
  out <- map2(out, chunks, add_effects)
  out <- map2(out, chunks, add_bindings)
  out <- map(out, add_hash)
  names(out) <- ifelse(names(dots) == "", paste0("chunk", seq_along(dots)), names(dots))

  add_dependencies(chunks, out)
}

add_code <- function(chunk, quo) {
  chunk$code <- quo_get_expr(quo)
  chunk
}

add_hash <- function(chunk) {
  chunk$hash <- digest::digest(list(chunk$code, chunk$bindings, chunk$effects), algo = "murmur32")
  chunk
}
add_effects <- function(chunk, tracking) {
  chunk$effects <- side_effects(tracking)
  chunk
}
add_bindings <- function(chunk, tracking) {
  if (length(chunk$effects$bindings) == 0) {
    return(chunk)
  }

  chunk$bindings <- env_get_list(tracking$env, chunk$effects$bindings, default = zap())
  chunk
}

