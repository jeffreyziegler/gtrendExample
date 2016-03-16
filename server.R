# load libraries
library(shiny)
library(plyr)
library(ggvis)
library(ggplot2)
library(gtrendsR)
library(R.utils)
library(RColorBrewer)

Logged = FALSE

ui1 <- function(){
  tagList(
    div(id = "login",
        h3("Google Log-In"),
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in")),
                  p("Please remain patient while Google connects and loads search trend data.")),
        tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

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
                  mainPanel("Relative frequency of ISIS searches by state",
                            fluidRow(
                              # map output
                              ggvisOutput("plot2"))
                  )
    )
  ) 
)}

# begin shiny app
ui = (htmlOutput("page"))

# shiny app
server = function(input, output, session){
    
    USER <- reactiveValues(Logged = Logged)
    
    observe({ 
      if (USER$Logged == FALSE) {
        if (!is.null(input$Login)) {
          if (input$Login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            # login to google
            gconnect(usr = Username, psw = Password, verbose = FALSE)
            if (length(Username) > 0 & length(Password) > 0) {
              if (!is.null(gconnect(usr=Username, psw=Password))) {
                USER$Logged <- TRUE
              } 
            }
          } 
        }
      }    
    })
    observe({
      if (USER$Logged == FALSE) {
        
        output$page <- renderUI({
          div(class="outer",do.call(bootstrapPage,c("",ui1())))
        })
      }
      if (USER$Logged == TRUE) 
      {
        # Filter the isis data, returning a data frame with data to construct map
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
        
        # render map output
        visualLine <- reactive({
          
          # open plot
          scatter %>%
            ggvis(~start, y = ~ISIS.US) %>%
            layer_points(stroke := "white") %>%
            layer_model_predictions(model = "loess", se = TRUE, fill := "YlOrRd") %>%
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
        
        output$page <- renderUI({
          div(class="outer",do.call(navbarPage,c(inverse=T, title = "Web Search interest: 'ISIS'",ui2())))
        })
        print(ui)
      }
    })
}

runApp(list(ui = ui, server = server))
