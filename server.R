# load libraries
library(shiny)
library(plyr)
library(ggvis)
library(ggplot2)
library(gtrendsR)
library(R.utils)
library(RColorBrewer)


# shiny app
shinyServer(
  function(input, output, session){
    # login to google
    gconnect(usr = "<Google Username>", psw = "<Google Password?>", verbose = FALSE)
    # Filter the isis data, returning a data frame with data to construct map
    map <- reactive({
      # create variable to change resolution of trending data
      # load data create input variables 
      isis <- gtrends("ISIS", res=input$time, geo="US")
      
      # create stateMap (list form)
      stateMap <- map_data('state')
      stateMap <- rename(stateMap, replace=c("region"="state"))
      # create function to capitalize both words in string
      cap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
      }
      
      # apply function to statestateMap$state
      stateMap$state <- sapply(stateMap$state, cap)
      isisData <- isis$Top.subregions.for.United.States
      # rename Subregion to state to combine with stateMap
      isisData <- rename(isis$Top.subregions.for.United.States, replace = c("Subregion" = "state"))
      # subset out AK and HI (only continental U.S.)
      isisData <- subset(isisData,  ! state %in% c("Hawaii", "Alaska"))

      # combine data with stateMap
      stateMap <- join(stateMap, isisData, by = "state")
      
      # add colors for map
      ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")
      stateMap$fillColor <- as.character(cut(stateMap$United.States, seq(0, 100, 10), include.lowest=TRUE, labels=ramp(10)))

      # output data frames
      stateMap
      }
    )
    
    # Filter the isis data, returning a data frame with data to construct scatterplot
    scatter <- reactive({
      # Due to dplyr issue #318, we need temp variables for input values
      gconnect(usr = "<Google Username>", psw = "<Google Password?>", verbose = FALSE)
      # create variable to change resolution of trending data
      # load data create input variables 
      isis <- gtrends("ISIS", res=input$time, geo="US")
      
      # create dataframe for trend line plot
      isisTrend <- isis$trend
      
      # output data frames
      isisTrend
      }
    )
    
    # Function for generating tooltip text
    isis_tooltip <- function(x) {
      # deal with missing values
      if (is.null(x)) return(NULL)
      if (is.null(x$group)) return(NULL)
      # Pick out the state with this ID
      statesHover <- isolate(map())
      mapHover <- statesHover[statesHover$group == x$group, ]
      # remove duplicates
      mapHover <- mapHover[!duplicated(mapHover$state), ]
      
      # select info to be shown in hover
      paste0("<b>", mapHover$state, "</b><br>",
      mapHover$United.States, "<br>")
    }
    
    # render map output
    visualLine <- reactive({
      
      # open plot
      scatter %>%
        ggvis(~start, y = ~ISIS.US) %>%
        layer_points(stroke := "white", fill := "YlOrRd") %>%
        layer_model_predictions(model = "loess", se = TRUE) %>%
        add_axis("x", title = "Time") %>%
        add_axis("y", title = "Relative frequency of ISIS search") %>%
        set_options(width=500, height=350, keep_aspect=TRUE)
    })
    
    # render map output
    visualMap <- reactive({
      
      # open plot
      map %>%
        group_by(group) %>%
        ggvis(~long, ~lat) %>%
        layer_paths(fill:=~fillColor, strokeOpacity := 0.5, strokeWidth := 0.1) %>%
        add_tooltip(isis_tooltip, "hover") %>%
        hide_axis("x") %>%
        hide_axis("y") %>%
        set_options(width=850, height=550, keep_aspect=TRUE)
    })
    
    # label visualization output
    visualLine %>% bind_shiny("plot1")
    visualMap %>% bind_shiny("plot2")
  })