#' Train a model encapsulated within a tundraContainer.
#'
#' @param dataframe data.frame. The data set to train the model on. This
#'    will be preprocessed with the \code{tundraContainer}'s 
#'    \code{munge_procedure} and then passed as the first argument to
#'    the \code{tundraContainer}'s \code{train_function}.
#' @param train_args list. A list of arguments to pass to make available
#'    to the \code{tundraContainer}'s \code{train_function} through
#'    use of the \code{input} keyword. See the examples.
#' @param verbose logical. Either \code{TRUE} or \code{FALSE}, by
#'    default the latter. If \code{TRUE}, then output produced by
#'    running the \code{munge_procedure} or the \code{train_function}
#'    will not be silenced.
#' @param munge logical. Either \code{TRUE} or \code{FALSE}, by
#'    default the former. If \code{FALSE}, the \code{munge_procedure}
#'    provided to the container during initialization will be assumed
#'    to have been trained, and the \code{dataframe} provided will not
#'    be run through it.
#' @return The value returned by the \code{tundraContainer}'s
#'    \code{train_function}. Since the \code{train_function} has side effects
#'    on the container, this can usually be \code{invisible(NULL)}.
train <- function(dataframe, train_args = list(), verbose = FALSE, munge = TRUE) {
  if (isTRUE(self$.trained)) {
    stop("The tundra ", sQuote(self$.keyword), " model has already been trained.")
  }

  force(train_args)
  force(verbose)
  force(munge)

  private$run_hooks("train_pre_munge")
  if (isTRUE(munge) && length(self$.munge_procedure) > 0) {
    dataframe <- munge(dataframe, self$.munge_procedure, verbose)
    attr(dataframe, "mungepieces") <- NULL
  }
  private$run_hooks("train_post_munge")

  output <- call_with(
    self$.train_function,
    list(dataframe),
    list(input = list_to_env(train_args, self$.input), output = self$.output)
  )

  private$run_hooks("train_finalize")
  self$.trained <<- TRUE

  output
}

munge <- function(dataframe, munge_procedure, verbose) {
  if (isTRUE(verbose)) {
    capture.output(Recall(dataframe, munge_procedure, FALSE))
  } else {
    mungebits::munge(dataframe, munge_procedure)
  }
}

