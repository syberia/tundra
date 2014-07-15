#' Tundra ensemble wrapper

# return a tundra container for the ensemble submodels
fetch_submodel_container <- function(model_parameters) {
  is.character(model_parameters[[1]])
  type <- model_parameters[[1]]
  model_parameters[[1]] <- NULL
  
  # additional munge steps for submodel (empty list means use the post-munged data without additional work)
  munge_procedure <- model_parameters$data %||% list() 

  # remaining model parameters (default_args)
  default_args <- model_parameters[which(names(model_parameters) != 'data')] %||% list()

  # internal argument
  internal <- list()

  # look for associated tundra container
  if (exists(model_fn <- paste0('tundra_', type))) {
    return(get(model_fn)(munge_procedure, default_args))
  } 

  # look for associated classifier
  prefilename <- file.path(syberia_root(), 'lib', 'classifiers', type)
  if ((file.exists(filename <- paste0(prefilename, '.r')) ||
       file.exists(filename <- paste0(prefilename, '.R')))) {
  
    provided_env <- new.env()
    source(filename, local = provided_env)
    provided_functions <- syberiaStages:::parse_custom_classifier(provided_env, type)

    # create a new tundra container on the fly
    my_tundra_container <-
      tundra:::tundra_container$new(type, 
                                    provided_functions$train, 
                                    provided_functions$predict,
                                    munge_procedure, 
                                    default_args, 
                                    internal)
    return(my_tundra_container)

  }
  
  stop("Missing tundra container for keyword '", type, "'. ",
       "It must exist in the tundra package or be present in ",
       paste0("lib/classifiers/", type, ".R"), call. = FALSE)

}

# train sub models and master model
tundra_ensemble_train_fn <- function(dataframe) {

  stopifnot('submodels' %in% names(input) && 'master' %in% names(input))

  # Set defaults for other parameters
  resample <- input$resample %||% FALSE         # whether or not to use bootstrapped replicates of the data
  replicates <- input$replicates %||% 3         # how many bootstrapped replicates to use if resample = T
  buckets <- input$validation_buckets %||% 10   # number of cross validation folds
  seed <- input$seed %||% 0                     # seed controls the sampling for cross validation
  checkcorr <- input$checkcorr  %||% FALSE      # check correlations of submodel predictions
  input$path <- input$path %||% NULL           # where to save the correlation plot of model predictions

  # extract pre-trained models
  pretrained_models <- Filter( function(x) x[[1]] == 'tundra', input$submodels )
  input$submodels <- Filter( function(x) x[[1]] != 'tundra', input$submodels )
  
  # make a list to store submodels
  output <<- list()
  output$submodels <<- list()
  output$pretrained_models <<- pretrained_models
  
  # Remove munge procedure from data.frame
  # so that it does not get attached to the model tundraContainer.
  # Otherwise the data_stage munge_procedure will be repeated at every train phase
  attr(dataframe, 'mungepieces') <- NULL
  
  # Output
  cat("Training ensemble composed of ", length(input$submodels), " submodels",sep='')
  if (resample) cat(" (", replicates, " bootstrap replications per submodel)",sep='')
  if (length(pretrained_models) > 0) cat(" and ", length(pretrained_models), " pre-trained models",sep='')
  cat("...\n")
  
  if (length(pretrained_models) > 0) {
    cat("Pre-trained models should only be used if they have been trained on different data!\n")
  }
    
  # set seed
  if (seed != 0) set.seed(seed)

   use_cache <- 'cache_dir' %in% names(input)
   if (use_cache) {
     stopifnot(is.character(input$cache_dir))
     input$cache_dir <- normalizePath(input$cache_dir)
   }

  # Get predictions for pre-trained models
  if (length(pretrained_models) > 0) {
    get_preds <- function(model) {
      my_model <- readRDS(paste0(model$path,'.rds'))
      my_model$predict(dataframe)
    }
    pretrained_preds <- Reduce(cbind, lapply(pretrained_models, get_preds))
  }
  
  # Get predictions for remaining models
  if (resample) {
    
    # cross-validation buckets (can be sequential since data are randomly ordered anyway)
    if (nrow(dataframe) < buckets) stop('Dataframe too small')
    which_slice <- floor(buckets*(1:nrow(dataframe) - 1)/nrow(dataframe))
    slices <- split(1:nrow(dataframe), which_slice) 
    
    # parallelize if possible
    packages('parallel')
    apply_method_name <- if (suppressWarnings(require(pbapply))) 'pblapply' else 'lapply'
    apply_method <- get(apply_method_name)
    
    # get predictions on the bootstrapped data
    # use CV predictions if for rows in the bootstrap sample
    # use predictions from a model built on entire bootstrap sample for not-in-sample rows
    which_submodel <- 0
    get_bootstrap_preds <- function(model_parameters) {
      
      which_submodel <<- which_submodel + 1

      if (use_cache) {
       cache_path <- paste0(input$cache_dir, '/', which_submodel)
       if (file.exists(tmp <- paste0(cache_path, 'p')))
         return(as.numeric(as.character(readRDS(tmp)[seq_len(nrow(dataframe))])))
      }
      
      # Sneak the munge procedure out of the submodel, because we do not
      # want to re-run it many times during n-fold cross-validation.
      munge_procedure <- model_parameters$data %||% list()
      model_parameters$data <- NULL
      # After the line below, attr(sub_df, 'selected_rows') will have the resampled
      # row numbers relative to the original dataframe.
      
      sub_df <- munge(dataframe, munge_procedure)
      # TODO: To prevent canonical names like "selected_rows", this could be determined
      # heuristically, like looking for an attribute with "rows" in its name or 
      # one that is an atomic integer vector (except the usual attributes, of course).
      
      
      # Fetch the tundra container for this submodel
      output$submodels[[which_submodel]] <<- fetch_submodel_container(model_parameters)
      
      # Generate predictions for the resampled dataframe using n-fold
      # cross-validation (and keeping in mind the above comment, note we
      # are not re-sampling multiple times, which would be erroneous).
      predicts <- do.call(c, mclapply(slices, function(rows) {
        # Train submodel on all but the current validation slice.
        output$submodels[[which_submodel]]$train(sub_df[-rows, ], verbose = TRUE)
        on.exit(output$submodels[[which_submodel]]$trained <<- FALSE)
        # Mark untrained so tundra container allows us
        # to train again next iteration.
        output$submodels[[which_submodel]]$predict(sub_df[rows, which(colnames(sub_df) != 'dep_var')])
      }))
      
      # Most of the work is done. We now have to generate predictions by
      # training the model on the whole resampled dataframe, and predicting
      # on the rows that were left out due to resampling to train our meta learner later.
      output$submodels[[which_submodel]]$train(sub_df, verbose = TRUE)
      if (use_cache) saveRDS(output$submodels[[which_submodel]], cache_path)
      
      # Record what row indices were left out due to resampling.
      remaining_rows <- setdiff(seq_len(nrow(dataframe)), attr(sub_df, 'selected_rows'))
      if (length(remaining_rows) == 0) return(predicts)  
      
      # Trick: since we have already done all the hard work of predicting,
      # we can now just append the remaining rows to the sampled rows (with duplicates)
      # and find a sequence parameterizing 1, 2, ..., nrow(dataframe) within
      # this list. For example, if 
      #   selected_rows <- c(2, 2, 4, 5, 4)
      # and 
      #   remaining_rows <- c(1, 3)
      # then 
      #   combined_rows <- c(6, 1, 7, 3, 4)
      # which are indices corresponding to a sequence c(1, 2, 3, 4, 5)
      # inside of c(selected_rows, remaining_rows) = c(2, 2, 4, 5, 4, 1, 3)
      rows_drawer <- append(attr(sub_df, 'selected_rows'), remaining_rows)
      combined_rows <- vapply(seq_len(nrow(dataframe)), function(x)
        which(x == rows_drawer)[1], integer(1))
      
      # Now that we have computed a sequence of indices parametrizing 1 .. nrow(dataframe)
      # through c(selected_rows, remaining_rows), take the respective predicted
      # scores and grab the relative indices in that order.
      predicts <- append(predicts,
                         output$submodels[[which_submodel]]$predict(sub_df[remaining_rows,
                                                                           which(colnames(sub_df) != 'dep_var')]))
      if (use_cache) saveRDS(predicts[combined_rows], paste0(cache_path, 'p'))
      predicts[combined_rows]                             
    })
  })) # End construction of meta_dataframe
 } else {
    
    # cross-validation buckets
    if (nrow(dataframe) < buckets) stop('Dataframe too small')
    slices <- split(1:nrow(dataframe), sample.int(buckets, nrow(dataframe), replace=T)) 
    metalearner_dataframe <- do.call(rbind, lapply(slices, function(rows) {
      sub_df <- data.frame(lapply(input$submodels, function(model_parameters) {
        model <- fetch_submodel_container(model_parameters)
        model$train(dataframe[-rows, ], verbose = TRUE)
        res <- model$predict(dataframe[rows, which(colnames(dataframe) != 'dep_var')])
      }))
      colnames(sub_df) <- paste0("model", seq_along(sub_df))
      sub_df
    }))
  }
    
    rownames(metalearner_dataframe) <- NULL
    metalearner_dataframe <- data.frame(metalearner_dataframe, stringsAsFactors = FALSE)
    colnames(metalearner_dataframe) <- paste0("model", seq_along(metalearner_dataframe))
    metalearner_dataframe$dep_var <- dataframe$dep_var
    if (use_cache)
      saveRDS(metalearner_dataframe, paste0(input$cache_dir, '/metalearner_dataframe'))
    
    output$master <<- fetch_submodel_container(input$master)
    output$master$train(metalearner_dataframe, verbose = TRUE)
    
    # Train final submodels
    if (!input$resample) { # If resampling was used, submodels are already trained
      output$submodels <<- lapply(input$submodels, function(model_parameters) {
        model <- fetch_submodel_container(model_parameters)
        model$train(dataframe, verbose = TRUE)
        model
      })
    }

  # add predictions from pre-existing models
  if (length(pretrained_models) > 0) {
    metalearner_dataframe <- cbind(pretrained_preds, metalearner_dataframe)
  }
  
  rownames(metalearner_dataframe) <- NULL
  metalearner_dataframe <- data.frame(metalearner_dataframe, stringsAsFactors = FALSE)
  colnames(metalearner_dataframe) <- paste0("model", seq_along(metalearner_dataframe))
  metalearner_dataframe$dep_var <- dataframe$dep_var
  if (use_cache)
    saveRDS(metalearner_dataframe, paste0(input$cache_dir, '/metalearner_dataframe'))
  
  output$master <<- fetch_submodel_container(input$master)
  output$master$train(metalearner_dataframe, verbose = TRUE)
  
  # Train final submodels
  if (!input$resample) { # If resampling was used, submodels are already trained
    output$submodels <<- lapply(input$submodels, function(model_parameters) {
      model <- fetch_submodel_container(model_parameters)
      model$train(dataframe, verbose = TRUE)
      model
    })
  }

  invisible("ensemble")
}

tundra_ensemble_predict_fn <- function(dataframe, predicts_args = list()) {
  
  # Get predictions for pre-trained models
  if (length(output$pretrained_models)>0) {
    get_preds <- function(model) {
      my_model <- readRDS(paste0(model$path,'.rds'))
      my_model$predict(dataframe)
    }
    pretrained_preds <- Reduce(cbind, lapply(output$pretrained_models,get_preds))
  }
  
  # get submodel scores
  meta_dataframe <- data.frame(lapply(output$submodels, function(model) model$predict(dataframe)))
  
  # construct the predictor matrix
  if (length(output$pretrained_models)>0) {
    meta_dataframe <- cbind(pretrained_preds, meta_dataframe)
  }
  colnames(meta_dataframe) <- paste0("model", seq_along(meta_dataframe))
  
  # score the metamodel
  output$master$predict(meta_dataframe)
}

#' @export
tundra_ensemble <- function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new('ensemble',
                       tundra_ensemble_train_fn,
                       tundra_ensemble_predict_fn,
                       munge_procedure,
                       default_args,
                       internal)
}
