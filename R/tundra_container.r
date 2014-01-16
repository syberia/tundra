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
                default_args = 'list',
                trained = 'logical',
                input = 'list',
                output = 'ANY'),
  methods = list(
    initialize = function(keyword, train_fn, predict_fn,
                          munge_procedure = list(),
                          default_args = list()) {
      keyword <<- keyword
      train_fn <<- train_fn
      predict_fn <<- predict_fn
      munge_procedure <<- munge_procedure
      default_args <<- default_args
      trained <<- FALSE
    },

    train = function(dataframe, train_args = list(), verbose = FALSE) {
      if (trained)
        stop("Tundra model '", keyword, "' has already been trained.")

      if (length(munge_procedure) > 0) {
        require(mungebits)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure))

        # Store trained munge_procedure
        munge_procedure <<- attr(dataframe, 'mungepieces')
        attr(dataframe, 'mungepieces') <- NULL
      }

      input <<- append(train_args, default_args)
      environment(train_fn) <<- environment() # Allow access to reference class
      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- train_fn(dataframe)
      trained <<- TRUE
      res 
    },

    predict = function(dataframe, predict_args = list(), verbose = FALSE) {
      if (!trained)
        stop("Tundra model '", keyword, "' has not been trained yet.")

      if (length(munge_procedure) > 0) {
        require(mungebits)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure))
      }

      environment(predict_fn) <<- environment() # Allow access to reference class
      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- predict_fn(dataframe, predict_args))
      res
    }
  )
)

