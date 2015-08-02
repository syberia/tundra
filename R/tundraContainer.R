#' A standard container format for classifiers developed in R.
#'
#' @docType class
#' @name tundraContainer
#' @export
tundraContainer <- R6::R6Class("tundraContainer",
  public = list(
    .keyword          = NULL, # character
    .train_function   = NULL, # function
    .predict_function = NULL, # function
    .munge_procedure  = NULL, # list of mungepieces
    .default_args     = NULL, # list
    .trained          = FALSE, # logical
    .input            = NULL, # environment
    .output           = NULL, # environment
    .internal         = NULL, # environment
    .hooks            = NULL, # list

    initialize = initialize,
    train      = train
    
  )
)

