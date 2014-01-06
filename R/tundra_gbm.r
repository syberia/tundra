#' Tundra GBM wrapper
tundra_gbm_train_fn <- function(dataframe, ...) {
  require(gbm)

  options <- list(...)
  gbm_args <- list()
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)

  if (options$cv) {
    gbm_args[[1]] <- as.formula(paste('dep_var ~ `',
                                      paste(indep_vars, collapse = "` + `"),
                                      '`', sep = ''))
    gbm_args$data <- dataframe
    gbm_args$cv.folds <- options$cv_folds
    gbm_args$n.cores  <- options$number_of_cores
  } else {
    gbm_args$x <- dataframe[, indep_vars]
    gbm_args$y <- dataframe[, 'dep_var']
  }

  gbm_args <- append(gbm_args,
    list(distribution      = options$distribution,
         n.trees           = options$number_of_trees,
         shrinkage         = options$shrinkage_factor,
         interaction.depth = options$depth,
         n.minobsinnode    = options$min_observations,
         train.fraction    = options$train_fraction,
         bag.fraction      = options$bag_fraction,
         keep.data         = TRUE
  ))

  output <<- list(model = do.call(gbm, gbm_args), perf = list())
  if (!is.null(options$perf_method)) {
    output$perf[[options$perf_method]] <<-
      gbm.perf(output$model, method = options$perf_method, plot.it = FALSE)
  }
  if (!is.null(options$prediction_type))
    output$prediction_type <<- options$prediction_type

  invisible("gbm")
}

tundra_gbm_predict_fn <- function(dataframe, ...) {
  options <- list(...)
  if (is.null(inputs$perf_method) && is.null(options$perf_method))
    stop("No GBM performance method specified: must be OOB, test, or cv") 

  require(gbm)

  type <- if (is.null(options$prediction_type)) output$prediction_type
          else options$prediction_type

  # Perf method specified, check if cached
  perf_method <- if (is.null(options$perf_method)) inputs$perf_method
                 else options$perf_method

  if (!perf_method %in% names(output$perf))
    output$perf[[perf_method]] <<- 
      gbm.perf(output$model, method = perf_method, plot.it = FALSE)

  predict.gbm(object = output$model, newdata = dataframe,
    output$perf[[perf_method]], type = type)
}

tundra_gbm <- function(munge_procedure = list()) {
  tundra_container$new('gbm',
                       tundra_gbm_train_fn,
                       tundra_gbm_predict_fn,
                       munge_procedure)
}

