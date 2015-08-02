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

}
