#' Tundra ensemble wrapper
tundra_ensemble_train_fn <- function(dataframe) {
  stopifnot('submodels' %in% names(inputs) && 'master' %in% names(inputs))

  buckets <- inputs$validation_buckets %||% 10
  if (nrow(dataframe) < buckets) stop('Dataframe too small')
  slice <- function(x, n) split(x, as.integer((seq_along(x) - 1) / n))
  slices <- slice(seq_len(nrow(dataframe)), buckets)

  meta_dataframe <- do.call(rbind, lapply(slices, function(rows) {
    sub_df <- data.frame(lapply(inputs$submodels, function(model_parameters) {
      stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
      if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
        stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
      model <- get(model_fn)(model_parameters$data,
        model_parameters[setdiff(which(names(model_parameters) != 'data'), 1)])
      model$train(dataframe[-rows])
      model$predict(dataframe[rows, which(colnames(dataframe) != 'dep_var')])
    }))
    colnames(sub_df) <- paste0("model", seq_along(sub_df))
    sub_df
  })

  # TODO: Dry this
  model_parameters <- inputs$master
  stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
  if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
    stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
  output$master <- get(model_fn)(model_parameters$data,
    model_parameters[setdiff(which(names(model_parameters) != 'data'), 1)])
  output$master$train(meta_dataframe)

  # Train final submodels
  output$submodels <<- lapply(inputs$submodels, function(model_parameters) {
    stopifnot(length(model_parameters) > 0 && is.character(model_parameters[[1]]))
    if (!exists(model_fn <- paste0('tundra_', model_parameters[[1]])))
      stop("Missing tundra container for keyword '", model_parameters[[1]], "'")
    model <- get(model_fn)(model_parameters$data,
      model_parameters[setdiff(which(names(model_parameters) != 'data'), 1)])
    model$train(dataframe)
    model
  })

  invisible("ensemble")
}

tundra_ensemble_predict_fn <- function(dataframe, predicts_args) {
  # TODO: DRY
  meta_dataframe <- data.frame(lapply(output$submodels, function(model) {
    model$predict(dataframe[, which(colnames(dataframe) != 'dep_var')])
  }))
  colnames(meta_dataframe) <- paste0("model", seq_along(meta_dataframe))

  output$master$predict(meta_dataframe)
}

#' @export
tundra_ensemble <- function(munge_procedure = list(), default_args = list()) {
  tundra_container$new('ensemble',
                       tundra_ensemble_train_fn,
                       tundra_ensemble_predict_fn,
                       munge_procedure,
                       default_args)
}

