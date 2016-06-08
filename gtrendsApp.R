# load libraries
library(shiny)
library(plyr)
library(ggvis)
library(ggplot2)
library(gtrendsR)
library(R.utils)
library(RColorBrewer)

# set default logged in status to FALSE
Logged = FALSE

# create sign-in/initial page
ui1 <- function(){
  tagList(
    # label div "login"
    div(id = "login",
        # set header of page
        h3("Google Log-In"),
        # create log-in panel w/ username and password
        wellPanel(textInput("userName", "Username"),
            passwordInput("passwd", "Password"),
            # create log-in button
            br(),actionButton("Login", "Log in")),
                  # add comment below button
                  p("Please remain patient while Google connects and loads search trend data.")),
        # stylize page
        tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

# create secondary page: scatter plot and map of search trend
ui2 <- function(){
  tagList(
  # start page
  fluidPage(
    # add side bar and main panel
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
                  # title of page
                  mainPanel("Relative frequency of ISIS searches by state",
                            fluidRow(
                              # map output
                              ggvisOutput("plot2"))
                  )
    )
  ) 
)}

# call page with htmlOutput
ui = (htmlOutput("page"))

# begin server function of shiny app
server = function(input, output, session){
    # logged in status determined by log-in from ui1()
    USER <- reactiveValues(Logged = Logged)
    # since the default is set to FALSE
    observe({ 
      if (USER$Logged == FALSE) {
        # determine if valid value is present to log-in
        if (!is.null(input$Login)) {
          if (input$Login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            ## attempt to log-in to google
            # rename variables for gconnect()
            gconnect(usr = Username, psw = Password, verbose = FALSE)
            # if log-in is successful change logged status to TRUE
            if (!is.null(gconnect(usr=Username, psw=Password))) {
                USER$Logged <- TRUE
            }
          } 
        }
        # once user logs in execute ui1 function
        output$page <- renderUI({
          div(class="outer",do.call(bootstrapPage,c("",ui1())))
        })
    }})
    # once log-in is successful
    observe({
      if (USER$Logged == TRUE) 
      {
        # filter isis data, returning a data frame with data to construct map
        map <- reactive({
          # create variable to change resolution of trending data
          # load data create input variables 
          isis <- gtrends(query="ISIS", res=input$time, geo="US")
          
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
        
        # render scatterplot output
        visualLine <- reactive({
          
          # open plot
          scatter %>%
            ggvis(~start, y = ~ISIS.US) %>%
            # point outline
            layer_points(stroke := "white") %>%
            # add smoother
            layer_model_predictions(model = "loess", se = TRUE, fill := "YlOrRd") %>%
            # label axes
            add_axis("x", title = "Time") %>%
            add_axis("y", title = "Relative frequency of ISIS search") %>%
            # set plot dimensions
            set_options(width=500, height=350, keep_aspect=TRUE)
        })
        
        # render map output
        visualMap <- reactive({
          
          # open plot
          map %>%
            group_by(group) %>%
            ggvis(~long, ~lat) %>%
            # determine fill colors
            layer_paths(fill:=~fillColor, strokeOpacity := 0.5, strokeWidth := 0.1) %>%
            # add hover tool
            add_tooltip(isis_tooltip, "hover") %>%
            # hide axes
            hide_axis("x") %>%
            hide_axis("y") %>%
            # set plot dimensions
            set_options(width=850, height=550, keep_aspect=TRUE)
        })
        
        # label visualization output
        visualLine %>% bind_shiny("plot1")
        visualMap %>% bind_shiny("plot2")
        # execute ui2 function
        output$page <- renderUI({
          div(class="outer",do.call(navbarPage,c(inverse=T, title = "Web Search interest: 'ISIS'",ui2())))
        })
        # print ui functions jointly
        print(ui)
      }
    })
}

# run app
runApp(list(ui = ui, server = server))
