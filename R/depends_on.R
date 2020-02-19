add_dependencies <- function(chunks) {

  sets <- map2(chunks, seq_along(chunks), function(chunk, i) {
    td <- chunk$tracking
    out <- td[td$action == "set", c("name", "value"), drop = FALSE]
    out$chunk <- i
    out
  })
  sets <- vec_rbind(!!!rev(sets))

  for(i in seq2(2, length(chunks))) {
     dep <- depends_on(
      chunks[[i]]$tracking,
      sets[sets$chunk < i, , drop = FALSE]
    )
    dep_chunks <- vec_split(dep, dep$chunk)
    dep_chunks <- dep_chunks[!is.na(dep_chunks$key), , drop = FALSE]
    chunks[[i]]$dependencies <- tibble(
      chunk = names(chunks)[dep_chunks$key],
      reason = map_chr(dep_chunks$val, function(df) paste0(reason(df), collapse = ", ")),
      hash = vapply(chunks[dep_chunks$key], function(x) x$effects_hash, character(1))
    )

    # Better to have name, reason, hash in tibble?
    # reason = pasted name-value (except where value = NA, and just use name)
    # out[[i]]$dependencies <- map()
  }

  chunks
}

depends_on <- function(tracking, sets) {
  get <- tracking[tracking$action == "get", c("name", "value")]
  set <- sets[c("name", "value")]
  idx <- vec_match(get, set)

  sets[idx, , drop = FALSE]
}

reason <- function(tracking) {
  ifelse(is.na(tracking$value), tracking$name, paste0(tracking$name, ":", tracking$value))
}
