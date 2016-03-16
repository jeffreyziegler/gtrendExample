# gtrendExample

This shiny app returns search trends pertaining to the search term "ISIS". User input alters the observable search time period. 

Copy files into a local working directory. When you execute `runApp()` at the bottom of the `server.R` file you will be prompted to log-in to Google. This is done so you can access the via the `gconnect()` function in the `gtrendsR` package. Upon successfu

Once you have inserted your Google log-in information, the application window should open. The user then can determine the time period of the search query. The left panel shows a trend scatterplot over the observed time period, along with a loess smoother. The right panel shows the relative frequency of the search term by U.S. state.
