# gtrendExample

This shiny app visualizes Google search trends. Users input the observable search time period. 

Copy file into a local working directory. When you execute `runApp()` at the bottom of the `server.R` file you will be prompted to log-in to Google. This is done so you can access the Google API via the `gconnect()` function in the `gtrendsR` package in `R`. Once you have inserted your Google log-in information, the application window should open.

The user can then determine the time period of the search query. The left panel shows a scatterplot of the search term's relative frequency over the observed time period, along with a loess smoother. The right panel shows the relative frequency of the search term by U.S. state.
