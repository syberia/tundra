#' Predict on a dataset using a trained tundraContainer.
#'
#' @param dataframe data.frame. The data set to generate predictions on
#'    with the trained model. The data will be preprocessed with the
#'    \code{tundraContainer}'s trained \code{munge_procedure} and
#'    then passed as the first argument to the \code{tundraContainer}'s
#'    \code{predict_function}.
#' @param predict_args list. A list of arguments to pass to pass to the
#'    \code{tundraContainer}'s \code{predict_function} as its second argument.
#' @param verbose logical. Either \code{TRUE} or \code{FALSE}, by
#'    default the latter. If \code{TRUE}, then output produced by
#'    running the \code{munge_procedure} or the \code{predict_function}
#'    will not be silenced.
#' @param munge logical. Either \code{TRUE} or \code{FALSE}, by
#'    default the former. If \code{FALSE}, the \code{munge_procedure}
#'    provided to the container during initialization will be used to
#'    preprocess the given \code{dataframe}.
#' @return The value returned by the \code{tundraContainer}'s
#'    \code{predict_function}, usually a numeric vector or
#'    \code{data.frame} of predictions.
predict <- function(dataframe, predict_args = list(), verbose = FALSE, munge = TRUE) {
  if (!isTRUE(self$.trained)) {
    stop("Tundra model ", sQuote(self$.keyword), " has not been trained yet.")
  }

  force(verbose)
  force(munge)
  force(predict_args)

  private$run_hooks("predict_pre_munge")
  if (isTRUE(munge) && length(self$.munge_procedure) > 0) {
    initial_nrow <- NROW(datafram)
    dataframe <- munge(dataframe, self$.munge_procedure, verbose)
    if (NROW(dataframe) != initial_nrow) {
      warning("Some rows were removed during data preparation. ",
              "Predictions will not match input dataframe.")
    }
  }
  private$run_hooks("predict_post_munge")

  if (length(formals(self$.predict_function) < 2 || missing(predict_args))) {
    args <- list(dataframe)
  } else {
    args <- list(dataframe, predict_args)
  }

  call_with(
    self$.predict_function,
    args,
    list(input = self$.input, output = self$.output)
  )
}

