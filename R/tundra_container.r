#' @name tundra_container
#' @title tundra_container
#' @export
NULL

#' Tundra container class
#'
#' TODO: Formally define parameter spaces for models
#' 
#' @docType class
#' @name tundraContainer
#' @aliases NULL
#' @export
tundra_container <- setRefClass('tundraContainer',  #define reference classes to access by reference instead of by value
  fields = list(keyword = 'character',
                train_fn = 'function',
                predict_fn = 'function',
                munge_procedure = 'ANY',  # tundra contains munge_procedure so that it remembers the data-prep steps
                default_args = 'list',
                trained = 'logical',
                input = 'list',
                output = 'ANY',    # output stores the actual output of the train function (e.g. the model object)
                internal = 'list', # for storing info about the model
                hooks = 'list'),
  methods = list(
    initialize = function(keyword = character(0),
                          train_fn = identity, predict_fn = identity,
                          munge_procedure = list(),
                          default_args = list(),
                          internal = list()) {
      if (!(is.list(munge_procedure) || is(munge_procedure, "stageRunner"))) {
        stop("munge_procedure must be a list or stageRunner", call. = FALSE)
      }
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

      force(train_args); force(verbose); force(munge) 

      .run_hooks('train_pre_munge')

      if (length(munge_procedure) > 0 && identical(munge, TRUE)) {
        require(mungebits)
        triggers <- unlist(lapply(munge_procedure,
                              function(x) inherits(x, 'trigger')))
        
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- mungebits::munge(dataframe, munge_procedure)) # Apply munge_procedure to dataframe

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

      .run_hooks('train_post_munge')

      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <- train_fn(dataframe))           # Apply train function to dataframe

      input <<- run_env$input; output <<- run_env$output
      trained <<- TRUE
      res 
    },

    predict = function(dataframe, predict_args = list(), verbose = FALSE, munge = TRUE) {
      if (!trained)
        stop("Tundra model '", keyword, "' has not been trained yet.")

      force(verbose); force(munge); force(predict_args)

      .run_hooks('predict_pre_munge')

      if (length(munge_procedure) > 0 && identical(munge, TRUE)) {
        require(mungebits)
        initial_nrow <- nrow(dataframe)
        (if (!verbose) capture.output else function(...) eval.parent(...))(
          dataframe <- mungebits::munge(dataframe, munge_procedure)) # Apply munge_procedure to dataframe
        if (nrow(dataframe) != initial_nrow)
          warning(paste("Some rows were removed during data preparation.",
                        "Predictions will not match input dataframe."))

      }

      run_env <- new.env(parent = globalenv())
      run_env$input <- input; run_env$output <- output
      debug_flag <- isdebugged(predict_fn)
      environment(predict_fn) <<- run_env
      if (debug_flag) debug(predict_fn)

      .run_hooks('predict_post_munge')

      (if (!verbose) capture.output else function(...) eval.parent(...))(
        res <-
          if (length(formals(predict_fn)) < 2 || missing(predict_args)) {
            predict_fn(dataframe)
          } else { predict_fn(dataframe, predict_args) }
      )
      input <<- run_env$input; output <<- run_env$output
      res
    },
    
    munge = function(dataframe, steps = TRUE) {
      mungebits::munge(dataframe, munge_procedure[steps]) 
    },

    show = function() {
      cat(paste("A tundraContainer of type", sQuote(keyword)), "\n")
    },

    add_hook = function(type, hook_function) {
      stopifnot(is.character(type) && length(type) == 1)
      stopifnot(is.function(hook_function))
      allowed_types <- paste0(as.character(outer(
        c('train', 'predict'), c('pre', 'post'), paste, sep = '_')), '_munge')
      if (!is.element(type, allowed_types)) {
        stop("Tundra container hooks must be one of: ",
             paste(allowed_types, collapse = ", "))
      }

      hooks[[type]] <<- c(hooks[[type]], hook_function)
    },

    .run_hooks = function(type) {
      if (!exists('hooks')) return() # Backwards compatibility
      for (i in seq_along(hooks[[type]])) {
        eval.parent(bquote({
          `*fn*` <- hooks[[.(type)]][[.(i)]]
          environment(`*fn*`) <- environment()
          `*fn*`()
        }))
      }
    }
  )
)

#' @export
summary.tundraContainer <- function(x, ...) summary(x$output$model, ...)
#' @export
print.tundraContainer <-
  function(x, ...) print(paste("A tundraContainer of type", sQuote(x$keyword)), ...)

