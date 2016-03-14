# load library
library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

# begin shiny app
shinyUI(
  # start page
  fluidPage(
    # add title to plot
    titlePanel("Web Search interest: 'ISIS'"),
    sidebarLayout(position = "left",
      sidebarPanel(
        wellPanel(
          selectInput(
            # label the selection box
            "time", "Select time frame of search",
            # choose timePeriod
            c("1h", "4h", "1d", "7d"))
        ),
        # trend output
        ggvisOutput("plot1")
      ),
        mainPanel("Relative frequency of ISIS searches by state",
          fluidRow(
                # map output
                ggvisOutput("plot2"))
            )
        )
    )
  )
