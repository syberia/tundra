# Instead of a complicated tundra container, we allow the user to specify the complexity in the syberia model
# In this case, we wrap everything in the input of the model stage with the stagerunner
tundra_multistage_train_fn <- function(dataframe) {
  output <<- list(modelenv = new.env())    # Need to change the actual Reference class object with <<- in the parent environment
  output$modelenv$data <<- dataframe       # assign dataframe to output$modelenv so it can be accessed from model stage
  on.exit(output$modelenv$data <<- NULL)   #remove data from output$modelenv in case of error
  stagerunner <- stageRunner$new(output$modelenv, input[-length(input)]) # input[-length(input)] : input carries all the model stage and 
                                                                         # last stage is reserved for prediction
  stagerunner$run()
  output <<- output                        # Need to change the actual Reference class object with <<- in the parent environment
}

tundra_multistage_predict_fn <- function(dataframe, predict_args = list()) {
  output$modelenv$data <<- dataframe
  on.exit(output$modelenv$data <<- NULL)
  predict_stagerunner <- stageRunner$new(output$modelenv, input[length(input)]) 
  # stagerunner is a sequential list of functions acting an environment 
  # and you can execute any function you want on the same environment
  predict_stagerunner$run()
  out <- output$modelenv$prediction
  output$modelenv$prediction <- NULL
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

