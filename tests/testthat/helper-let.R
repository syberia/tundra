let <- function(name, expr, envir = parent.frame()) {
  name <- gsub('"', '', deparse(substitute(name)), fixed = TRUE)
  force(envir)
  expr <- substitute(expr)
  makeActiveBinding(name, function() {
    force(envir); force(name); force(expr)
    local <- initial <- sys.frames()[[length(sys.frames()) - 1]]
    obj   <- structure(list(), class = "nonexistent")
    while (!identical(local, emptyenv()) && !identical(local, envir)) {
      if (exists(name, envir = local, inherits = FALSE) &&
          !bindingIsActive(name, env = local)) {
        obj <- get(name, envir = local)
        break
      }
      local <- parent.env(local)
    }
    if (is(obj, "nonexistent")) {
      obj <- eval(expr, envir = envir)
      if (!identical(initial, envir)) {
        assign(name, obj, envir = initial)
      }
    }
    obj
  }, env = envir)
}

