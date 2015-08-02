#' A standard container format for classifiers developed in R.
#'
#' @docType class
#' @name tundraContainer
#' @export
tundraContainer <- R6::R6Class("tundraContainer",
  public = list(
    .keyword          = NULL, # character
    .train_function   = NULL, # function or NULL
    .predict_function = NULL, # function or NULL
    .munge_procedure  = NULL, # list of mungepieces
    .default_args     = NULL, # list
    .trained          = FALSE, # logical
    .input            = NULL, # environment
    .output           = NULL, # environment
    .internal         = NULL, # environment
    .hooks            = NULL, # list

    initialize = function(...) {
    }
  )
)

