#' Initialize a tundraContainer object.
#'
#' @param keyword character. The name of the classifier; for example,
#'    "lm" or "knn".
#' @param train_function function. The function used to train the model.
#'    Its first argument will be a data.frame, and the second argument
#'    a list of additional parameters used for training the model.
#' @param predict_function function. The function used to predict
#'    on new datasets. Its first argument will be a data.frame,
#'    the dataset to predict on it, and its second (optional)
#'    argument will be additional parameters used for prediction
#'    output (such as whether to return a probabilistic or absolute
#'    value).
#' @param munge_procedure list. A list of trained
#'    \code{\link[mungebits]{mungepiece}}s to apply to data sets
#'    during prediction. 
#' @param default_args list. A list of default arguments to provide to
#'    the second argument to the \code{train_function}. The additional
#'    arguments provided to the \code{tundraContainer}'s \code{train}
#'    method will be merged on top of these defaults.
#' @param internal list. Internal metadata that should accompany the
#'    model. Usually this is domain/organization specific, and can
#'    include things such as a list of primary keys used for training
#'    the model, identifiers or names of data sources used for
#'    training the model, etc. It is a playground entirely under
#'    your control, and can be used by other packages or a production
#'    server hosting the model to achieve additional behavior.
initialize <- function(keyword, train_function = identity,
                       predict_function = identity, munge_procedure = list(),
                       default_args = list(), internal = list()) {
  if (!(is.list(munge_procedure) || is(munge_procedure, "stageRunner"))) {
    stop("The ", sQuote("munge_procedure"), " parameter must be a list or ",
         "stageRunner object.")
  }

  self$.keyword          <<- keyword
  self$.train_function   <<- train_function
  self$.predict_function <<- predict_function
  self$.munge_procedure  <<- munge_procedure
  self$.default_args     <<- default_args
  self$.internal         <<- internal

  self$.input            <<- list_to_env(list())
  lockEnvironment(self$.input)
  self$.output           <<- list_to_env(list())
  self$.internal         <<- list_to_env(list())
}

