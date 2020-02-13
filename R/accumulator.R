Accumulator <- R6::R6Class(public = list(
  action = character(),
  name = character(),
  value = character(),

  add = function(action, name, value) {
    self$action <- c(self$action, action)
    self$name <- c(self$name, name)
    self$value <- c(self$value, value)
  },

  data = function() {
    acc_tibble(self$action, self$name, self$value)
  }
))

acc_tibble <- function(action, name, value) {
  tibble::tibble(action = action, name = name, value = value)
}

env <- env()

check_active <- function() {
  if (env_has(env, "acc")) {
    return()
  }

  abort("No accumulator is active")
}

local_acc <- function(env = caller_env()) {
  acc_activate()
  on_exit(acc_deactivate(), env = env)
}

acc_activate <- function() {
  env$acc <- Accumulator$new()
  NULL
}

acc_deactivate <- function() {
  env_unbind(env, "acc")
  NULL
}

acc_data <- function() {
  check_active()
  env$acc$data()
}

acc_add <- function(action, name, value) {
  check_active()
  env$acc$add(action, name, value)
}

