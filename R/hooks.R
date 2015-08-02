#' Add a hook to a tundraContainer.
#'
#' Hooks are useful for defining additional checks that should be
#' performed prior to and during training and prediction. For example,
#' one might want to issue a warning if the user is predicting on 
#' rows that were used for training, or a sanity check might be 
#' present prior to training to ensure a dependent variable is present.
#'
#' The following hooks are available.
#'
#' \enumerate{
#'   \item{train_pre_munge}{This hook runs during a call to the
#'     container's \code{train} method, just prior to invoking the
#'     \code{munge_procedure} to clean up the dataset. It could be
#'     useful for defining pre-conditions on the dataset to ensure
#'     it can be munged successfully.}
#'   \item{train_post_munge}{This hook runs during a call to the
#'     container's \code{train} method, just after invoking the
#'     \code{munge_procedure} to clean up the dataset. It could be
#'     useful for defining post-conditions on the dataset to ensure
#'     it was munged successfully.}
#'   \item{train_finalize}{This hook runs just after the \code{train}
#'     method calls the \code{train_function}. It could be used to
#'     verify presence or validate properties of the trained model.}
#'   \item{predict_pre_munge}{This hook runs during a call to the
#'     container's \code{predict} method, just prior to invoking the
#'     \code{munge_procedure} to clean up the dataset. It could be
#'     useful for defining pre-conditions on the dataset to ensure
#'     it can be munged successfully.}
#'   \item{predict_post_munge}{This hook runs during a call to the
#'     container's \code{predict} method, just after invoking the
#'     \code{munge_procedure} to clean up the dataset. It could be
#'     useful for defining post-conditions on the dataset to ensure
#'     it was munged successfully.}
#' }
#'
#' Each hook will be provided the \code{tundraContainer} as input
#' (unless it has no arguments, in which case it will simply be called).
#'
#' @param hook_name character. The hook to run. Must be one of the available
#'    hooks.
run_hooks <- function(hook_name) {
  for (hook in hooks[[hook_name]]) {
    if (length(formals(hook)) > 0) {
      hook(self)
    } else {
      hook()
    }
  }
}

