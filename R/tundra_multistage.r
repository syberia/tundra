# Instead of a complicated tundra container, we allow the user to specify the complexity in the syberia model
# In this case, we wrap everything in the input of the model stage with the stagerunner
tundra_multistage_train_fun <- function(dataframe) {
  output$modelenv <- new.env()
  stagerunner <- stageRunner$new(output$modelenv, input[-length(input)]) # last stage is reserved for prediction
  stagerunner$run()
}

tundra_multistage_predict_fn <- function(dataframe, predict_args = list()) {
  predict_stagerunner <- stageRunner$new(output$modelenv, input[length(input)]) 
  # stagerunner is a sequential list of functions acting an environment 
  # and you can execute any function you want on the same environment
  predict_stagerunner$run()
  out <- output$modelenv$output
  output$modelenv$output <- NULL
  out
}


#' @export
tundra_multistage <- function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new('multistage',
                                tundra_multistage_train_fn,
                                tundra_multistage_predict_fn,
                                munge_procedure,
                                default_args,
                                internal)
}

