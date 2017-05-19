#' A standard container format for classifiers developed in R.
#'
#' @docType class
#' @name tundraContainer
#' @export
tundraContainer <- R6::R6Class("tundraContainer",
  public = list(
    .keyword          = NULL,  # character
    .train_function   = NULL,  # function
    .predict_function = NULL,  # function
    .munge_procedure  = NULL,  # list of mungepieces
    .default_args     = NULL,  # list
    .trained          = FALSE, # logical
    .input            = NULL,  # environment
    .output           = NULL,  # environment
    .internal         = NULL,  # environment
    .hooks            = NULL,  # list

    initialize = initialize,
    train      = train,
    predict    = predict,
    add_hook   = add_hook,
    munge      = function(dataframe, steps = TRUE) {
      mungebits2::munge(dataframe, munge_procedure[steps])
    },
    show       = function() {
      cat(paste0("A tundraContainer of type ", sQuote(self$.keyword), "\n"))
      invisible(self)
    }
  ),
  private = list(
    run_hooks = run_hooks
  )
)

#' @export
tundra_container <- function(...) { tundraContainer$new(...) }

#' @export
print.tundraContainer <- function(x, ...) { x$show() }

#' @export
summary.tundraContainer <- function(x, ...) { summary(x$.output$model, ...) }
