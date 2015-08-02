`%||%` <- function(x, y) if (is.null(x)) y else x

list_to_env <- function(obj, parent = emptyenv()) {
  if (length(obj) == 0) {
    new.env(parent = parent)
  } else {
    list2env(obj, parent = parent)
  }
}

#' Evaluate a function while injecting some locals.
#'
#' Instead of modifying a closure's parent environment directly,
#' sometimes it may be desirable to do a one-time injection that
#' overrides what would normally be accessible through the closure.
#' \code{call_with} allows this by extending the usual \code{do.call}
#' to a third argument that is a list or environment temporarily
#' injected during the course of the call.
#'
#' @param fn function.
#' @param args list. The arguments to call the \code{fn} with.
#' @param with list or environment. Additional locals to make available
#'    during the call.
#' @return The result of calling \code{fn} with the injection provided
#'    by the \code{with} parameter.
#' @examples \dontrun{
#' fn <- local({ x <- 1; function(y) { x + y } })
#' stopifnot(fn(1) == 2)
#' stopifnot(call_with(fn, list(1), list(x = 2)) == 3)
#' }
call_with <- function(fn, args, with) {
  stopifnot(is.list(with) || is.environment(with))
  debugged <- isdebugged(fn)
  copy_fn <- fn
  if (debugged) debug(copy_fn)
  env <- with
  if (!is.environment(with)) {
    with <- list_to_env(with, parent = environment(copy_fn))
  }
  environment(fn) <- with
  do.call(fn, args)
}

#' Merge two lists and overwrite latter entries with former entries
#' if names are the same.
#'
#' For example, \code{list_merge(list(a = 1, b = 2), list(b = 3, c = 4))}
#' will be \code{list(a = 1, b = 3, c = 4)}.
#' @param list1 list
#' @param list2 list
#' @return the merged list.
#' @examples \dontrun{
#' stopifnot(identical(list_merge(list(a = 1, b = 2), list(b = 3, c = 4)),
#'                     list(a = 1, b = 3, c = 4)))
#' stopifnot(identical(list_merge(NULL, list(a = 1)), list(a = 1)))
#' }
list_merge <- function(list1, list2) {
  list1 <- list1 %||% list()
  # Pre-allocate memory to make this slightly faster.
  list1[Filter(function(x) nchar(x) > 0, names(list2) %||% c())] <- NULL
  for (i in seq_along(list2)) {
    name <- names(list2)[i]
    if (!identical(name, NULL) && !identical(name, "")) list1[[name]] <- list2[[i]]
    else list1 <- append(list1, list(list2[[i]]))
  }
  list1
}

