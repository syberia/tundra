# A very simple model mock for testing tundra containers
function(mp = list(), defaults = list()) {
  tundra_container$new('simple',
    function(dataframe) output$val <<- dataframe[1, 1] + input$twiddle,
    function(dataframe)
      if (input$master) output$val + apply(dataframe, 1, sum)
      else rep(output$val, nrow(dataframe)),
    mp, append(defaults, list(master = FALSE, twiddle = 0)))
}

