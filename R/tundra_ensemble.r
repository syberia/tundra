#' Tundra ensemble wrapper
tundra_ensemble_train_fn <- function(dataframe) {
  stopifnot('submodels' %in% names(train_args) && 'master' %in% names(train_args))

  if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
    stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
  get(model_fn)
  lapply(train_args$submodels


  gbm_args <- list()
  indep_vars <- setdiff(colnames(dataframe), 'dep_var')
  stopifnot(length(indep_vars) > 0)

  if (train_args$cv) {
    gbm_args[[1]] <- as.formula(paste('dep_var ~ `',
                                      paste(indep_vars, collapse = "` + `"),
                                      '`', sep = ''))
    gbm_args$data <- dataframe
    gbm_args$cv.folds <- train_args$cv_folds
    gbm_args$n.cores  <- train_args$number_of_cores
  } else {
    gbm_args$x <- dataframe[, indep_vars]
    gbm_args$y <- dataframe[, 'dep_var']
  }

  gbm_args <- append(gbm_args,
    list(distribution      = train_args$distribution,
         n.trees           = train_args$number_of_trees,
         shrinkage         = train_args$shrinkage_factor,
         interaction.depth = train_args$depth,
         n.minobsinnode    = train_args$min_observations,
         train.fraction    = train_args$train_fraction,
         bag.fraction      = train_args$bag_fraction,
         var.monotone      = train_args$var.monotone,
         keep.data         = TRUE
  ))

  # Hack to prevent a hellbug where the AWS.tools package
  # masks the stopCluster function, causing a problem in gbm training
  assign('stopCluster', parallel::stopCluster, envir = globalenv())
  output <<- list(model = do.call(gbm, gbm_args), perf = list())
  rm('stopCluster', envir = globalenv())

  if (!is.null(train_args$perf_method)) {
    output$perf[[train_args$perf_method]] <<-
      gbm.perf(output$model, method = train_args$perf_method, plot.it = FALSE)
  }
  if (!is.null(train_args$prediction_type))
    output$prediction_type <<- train_args$prediction_type

  invisible("gbm")
}

tundra_ensemble_predict_fn <- function(dataframe, predicts_args) {
  if (is.null(input$perf_method) && is.null(train_args$perf_method))
    stop("No GBM performance method specified: must be OOB, test, or cv") 

  require(gbm)

  type <- if (is.null(predict_args$prediction_type)) output$prediction_type
          else predict_args$prediction_type

  # Perf method specified, check if cached
  perf_method <- if (is.null(predict_args$perf_method)) input$perf_method
                 else predict_args$perf_method

  if (!perf_method %in% names(output$perf))
    output$perf[[perf_method]] <<- 
      gbm.perf(output$model, method = perf_method, plot.it = FALSE)

  predict.gbm(object = output$model, newdata = dataframe,
    output$perf[[perf_method]], type = type)
}

#' @export
tundra_ensemble <- function(munge_procedure = list(), default_args = list()) {
  tundra_container$new('gbm',
                       tundra_gbm_train_fn,
                       tundra_gbm_predict_fn,
                       munge_procedure,
                       default_args)
}

