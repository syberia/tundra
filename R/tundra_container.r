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
                inputs = 'list',
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

    train = function(dataframe, ..., verbose = FALSE) {
      if (trained)
        stop("Tundra model '", keyword, "' has already been trained.")
      on.exit(trained <<- TRUE)

      if (length(munge_procedure) > 0) {
        require(mungebits)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure))
        # Store trained munge_procedure
        munge_procedure <<- attr(dataframe, 'mungepieces')
        attr(dataframe, 'mungepieces') <- NULL
      }

      inputs <<- append(list(...), default_args)
      environment(train_fn) <<- environment()
      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- do.call(function(...) train_fn(dataframe, ...), inputs))
      res
    },

    predict = function(dataframe, ..., verbose = FALSE) {
      if (!trained)
        stop("Tundra model '", keyword, "' has not been trained yet.")

      if (length(munge_procedure) > 0) {
        require(mungebits)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure))
      }

      environment(predict_fn) <<- environment()
      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- predict_fn(dataframe, ...))
      res
    }
  )
)

