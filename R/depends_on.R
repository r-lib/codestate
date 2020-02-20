chunk_dependencies <- function(chunk, chunks) {
  if (length(chunks) == 0) {
    return(tibble::tibble(chunk = character(), reason = character(), effects_hash = character()))
  }

  sets <- map2(chunks, seq_along(chunks), function(chunk, i) {
    td <- chunk$tracking
    out <- td[td$action == "set", c("name", "value"), drop = FALSE]
    out$chunk <- i
    out
  })
  sets <- vec_rbind(!!!rev(sets))

  get <- chunk$tracking[chunk$tracking$action == "get", c("name", "value")]
  set <- sets[c("name", "value")]
  idx <- vec_match(get, set)
  dep <- sets[idx, , drop = FALSE]

  dep_chunks <- vec_split(dep, dep$chunk)
  dep_chunks <- dep_chunks[!is.na(dep_chunks$key), , drop = FALSE]
  tibble::tibble(
    chunk = names(chunks)[dep_chunks$key],
    reason = map_chr(dep_chunks$val, function(df) paste0(reason(df), collapse = ", ")),
    effects_hash = map_chr(chunks[dep_chunks$key], function(x) x$effects_hash)
  )
}

reason <- function(tracking) {
  ifelse(is.na(tracking$value), tracking$name, paste0(tracking$name, ":", tracking$value))
}
