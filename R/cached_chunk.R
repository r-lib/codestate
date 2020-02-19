new_cached_chunk <- function(code,
                             tracking,
                             effects,
                             effects_hash,
                             bindings,
                             dependencies = NULL
                             ) {


  structure(
    list(
      code = code,
      tracking = tracking,
      effects = effects,
      bindings = bindings,
      effects_hash = effects_hash,
      dependencies = dependencies
    ),
    class = "cached_chunk"
  )
}

#' @export
print.cached_chunk <- function(x, ...) {
  code_hash <- digest::digest(x$code, algo = "murmur32")
  cli::cli_text("<cached_chunk>")

  cli::cli_ul()
  cli::cli_li("Code {.val {code_hash}}")
  cli::cli_li("Effects {.val {x$effects_hash}}")
  gets <- x$tracking[x$tracking$action == "get", , drop = FALSE]
  if (nrow(gets) > 0) {
    cli::cli_li("Gets {.val {reason(gets)}}")
  }
  sets <- x$tracking[x$tracking$action == "set", , drop = FALSE]
  if (nrow(sets) > 0) {
    cli::cli_li("Sets {.val {reason(sets)}}")
  }
  cli::cli_end()
  cli::cli_end()

  if (!is.null(x$dependencies) && nrow(x$dependencies) > 0) {
    cli::cli_text("Depends on:")
    cli::cli_ul()
    cli::cli_li(glue::glue("{x$dependencies$chunk} ({x$dependencies$hash}): {{.val {x$dependencies$reason} }}"))
    cli::cli_end()
    cli::cli_end()
  }
}
