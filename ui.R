source('functions.R')
library(plotly)
library(shiny)
library(shinythemes)
library(igraph)
library(plotly)

shinyUI(
  fluidPage(tags$head(
    ### Customizing the shiny theme used by redefining some festuress of the flatly theme css used ###
    tags$style(
      ### HTML function to write HTML Markup for custom positioning calculator logo ###
      HTML(
        "
        .navbar .navbar-brand {
        padding:10px 750px 10px 0px;
        text-align:left;
        height:80px;
        width:100%
        display:inline;
        }
        .navbar .navbar-header {
        float:right;
        }
        .navbar .navbar-nav{
        float:right;
        }
        "
      ) ### end HTML markup ###
      ) ### end style tag ###
      ),
  navbarPage(
    HTML(
      "<a><img src='http://kidoz.net/wp/wp-content/uploads/2015/09/distribution_icon.png'/>Study Of Diffusion in Graphs</a>"
    ), ### end HTML markup ###
    
  theme = shinytheme("flatly"),
  ### position attribute specifies position of navbar and visibility while scrolling ###
  position ="static-top", fluid = TRUE,

  tabPanel(
    "Erdos Renyi",
    column(
      3,
      wellPanel(
        numericInput("no_nodes_er", "Number of nodes", value = "100"),
        hr(),
        textInput("probability", "Gnp Probability", value = "0.02"),
        hr(),
        uiOutput("threshold_er"),
        hr(),
        sliderInput(
          "no_seeds_er","Number of seeds for ER Graph",min = 1,max = 1,value =
            1,step = 1
        ),
        numericInput("no_runs_er","Number of runs",value = 0,min=0)
      )
    ),
    column(9,
         tabsetPanel(
           tabPanel(
             "View ER Graph",
             column(6,h4("Erdos Renyi Graph"),
                    plotOutput("output_plot1")),
             
             column(
               6,h4("Degree distribution for Erdos Renyi Graph"),
               plotOutput("output_plot13")
             )
           ),
           
           tabPanel(
             "Random seed infection",
               plotOutput("output_plot11"),
               h5("Infected nodes:",textOutput("infected_random_er")),
               h5("Number of infected nodes:",textOutput("no_infected_random_er"))
           ),
           tabPanel(
             "Maximum degree node infection",
               plotOutput("output_plot12"),
               h5("Infected nodes:",textOutput("infected_max_er")),
               h5("Number of infected nodes:",textOutput("no_infected_max_er"))
             )
           
         )
         
    )
  ),
  
  tabPanel(
    "Power Law Graphs",
    column(
      3,
      wellPanel(
        numericInput("no_nodes_pl", "Number of nodes", value = "100"),
        hr(),
        numericInput(
          "power_exponent",
          "Power Exponent",
          value = "1",
          min = 0
        ),
        hr(),
        uiOutput("threshold_pl"),
        
        hr(),
        uiOutput("random_seeds_pl"),
        numericInput("no_runs_pl","Number of runs",value = 1,min=1)
      )  
    ),
    column(
      9,
      tabsetPanel(
        tabPanel(
          'View Power Law Graph',
          column(6,h4("Power Law Graph"),
                 plotOutput("output_plot2")),
          
          column(
            6,h4("Degree distribution for Power Law Graph"),
            plotOutput("output_plot23")
          )
        ),
        tabPanel(
          'Random seed infection',
          plotOutput("output_plot21"),
          h5("Infected nodes:",textOutput("infected_random_pl")),
          h5("Number of infected nodes:",textOutput("no_infected_random_pl"))
        ),
        tabPanel(
          'Maximum degree node infection',
          plotOutput("output_plot22"),
          h5("Infected nodes:",textOutput("infected_max_pl")),
          h5("Number of infected nodes:",textOutput("no_infected_max_pl"))
        )
      )
    )
  )
  )
  ))
