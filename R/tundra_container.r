#' Tundra container class
#'
#' TODO: Formally define parameter spaces for models
#' 
#' @docType class
tundra_container <- setRefClass('tundraContainer',  #define reference classes to access by reference instead of by value
  fields = list(keyword = 'character',
                train_fn = 'function',
                predict_fn = 'function',
                munge_procedure = 'list',
                default_args = 'list',
                trained = 'logical',
                sandbox = 'environment',
                input = 'list',
                output = 'ANY',
                internal = 'list' # for storing info about the model
                ),
  methods = list(
    initialize = function(keyword, train_fn, predict_fn,
                          munge_procedure = list(),
                          default_args = list(),
                          internal = list()) {
      keyword <<- keyword
      train_fn <<- train_fn
      predict_fn <<- predict_fn
      munge_procedure <<- munge_procedure
      default_args <<- default_args
      internal <<- internal
      trained <<- FALSE
    },

    train = function(dataframe, train_args = list(), verbose = FALSE, munge = TRUE) {
      if (trained)
        stop("Tundra model '", keyword, "' has already been trained.")

      if (length(munge_procedure) > 0 && identical(munge, TRUE)) {
        require(mungebits)
        triggers <- unlist(lapply(munge_procedure,
                              function(x) inherits(x, 'trigger')))
        
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure)) # Apply munge_procedure to dataframe

        # Store trained munge_procedure
        munge_procedure <<- attr(dataframe, 'mungepieces')[!triggers]

        # reset mungepieces to NULL after training
        attr(dataframe, 'mungepieces') <- NULL
      }

      run_env <- new.env(parent = old_env <- environment(train_fn))
      on.exit(environment(train_fn) <<- old_env)
      input <<- append(train_args, default_args)
      run_env$input <- input; run_env$output <- output
      debug_flag <- isdebugged(train_fn)
      environment(train_fn) <<- run_env
      if (debug_flag) debug(train_fn)

      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- train_fn(dataframe))           # Apply train function to dataframe

      input <<- run_env$input; output <<- run_env$output
      rm(run_env)
      trained <<- TRUE
      res 
    },

    predict = function(dataframe, predict_args = list(), verbose = FALSE, munge = TRUE) {
      if (!trained)
        stop("Tundra model '", keyword, "' has not been trained yet.")

      if (length(munge_procedure) > 0 && identical(munge, TRUE)) {
        require(mungebits)
        initial_nrow <- nrow(dataframe)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- munge(dataframe, munge_procedure)) # Apply munge_procedure to dataframe
        if (nrow(dataframe) != initial_nrow)
          warning(paste("Some rows were removed during data preparation.",
                        "Predictions will not match input dataframe."))

      }

      #sandbox <<- new.env(parent = globalenv())
      #sandbox$input <<- input
      #sandbox$output <<- output
      #environment(predict_fn) <<- sandbox
      run_env <- new.env(parent = globalenv())
      run_env$input <- input; run_env$output <- output
      
      debug_flag <- isdebugged(predict_fn)
      environment(predict_fn) <<- run_env
      #environment(predict_fn) <<- environment()
      if (debug_flag) debug(predict_fn)
      #thisenv <- environment()

      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- if (length(formals(predict_fn)) < 2 || missing(predict_args)) predict_fn(dataframe)
               else predict_fn(dataframe, predict_args)  # Apply predict function to dataframe
      )
      input <<- run_env$input; output <<- run_env$output
      rm(run_env)
      res
    }
  )
)


