#' Tundra container class
#'
#' TODO: Formally define parameter spaces for models
#' 
#' @docType class
tundra_container <- setRefClass('tundraContainer',
  fields = list(keyword = 'character',
                train_fn = 'function',
                predict_fn = 'function',
                munge_procedure = 'list',
                trained = 'logical',
                inputs = 'list',
                output = 'ANY'),
  methods = list(
    initialize = function(keyword, train_fn, predict_fn, munge_procedure = list()) {
      keyword <<- keyword
      train_fn <<- train_fn
      predict_fn <<- predict_fn
      munge_procedure <<- munge_procedure
      trained <<- FALSE
    },

    train = function(dataframe, ...) {
      if (trained)
        stop("Tundra model '", keyword, "' has already been trained.")
      on.exit(trained <<- TRUE)

      inputs <<- list(...)
      environment(train_fn) <<- environment()
      train_fn(dataframe, ...)
    },

    predict = function(dataframe, ...) {
      if (!trained)
        stop("Tundra model '", keyword, "' has not been trained yet.")

      environment(predict_fn) <<- environment()
      predict_fn(dataframe, ...)
    }
  )
)

