add_dependencies <- function(chunks, out) {

  sets <- map2(chunks, seq_along(chunks), function(chunk, i) {
    td <- chunk$tracking
    out <- td[td$action == "set", c("name", "value"), drop = FALSE]
    out$chunk <- i
    out
  })
  sets <- vec_rbind(!!!rev(sets))

  for(i in seq2(2, length(out))) {
     dep <- depends_on(
      chunks[[i]]$tracking,
      sets[sets$chunk < i, , drop = FALSE]
    )
    out[[i]]$depends_on <- dep
    # Better to have name, reason, hash in tibbe?
    # reason = pasted name-value (except where value = NA, and just use name)
    out[[i]]$dependencies <- map(out[vec_unique(dep$chunk)], function(x) x$hash)
  }

  out
}

depends_on <- function(tracking, sets) {
  get <- tracking[tracking$action == "get", c("name", "value")]
  set <- sets[c("name", "value")]
  idx <- vec_match(get, set)

  sets[idx, , drop = FALSE]
}
