source('functions.R')
library(plotly)
library(shiny)
shinyUI(fluidPage(
  titlePanel("Study of Diffusion in Networks"),br(),br(),
  fluidRow(
    column(
      2,numericInput("no_nodes","Number of nodes",value = "100"),
      hr(),
      textInput("probability","Gnp Probability",value = "0.02"),
      numericInput("power_exponent","Power Exponent",value = "1",min=0),
      hr(),
      uiOutput("threshold_er"),
      uiOutput("threshold_pl")
    ),
    column(
      2,
      uiOutput("random_seeds_er"),
      uiOutput("random_seeds_pl"),
      
      numericInput("no_runs","Number of runs",value = 0,min=0),
      submitButton("submit")
      
    ),
    column(
      8,tabsetPanel(
        tabPanel(
          "View ER Model graph and degree distribution",
          column(6,h4("Erdos Renyi Graph"),
                 plotOutput("output_plot1")),
          
          column(
            6,h4("Degree distribution for Erdos Renyi Graph"),
            plotOutput("output_plot13")
          )
        ),
        tabPanel(
          "View Power law Model graph and degree distribution",
          column(6,h4("Power Law Graph"),
                 plotOutput("output_plot2")),
          column(
            6,h4("Degree distribution for Power Law Graph"),
            plotOutput("output_plot23")
          )
          
        ),
        tabPanel(
          "Results - ER Model graph",
          column(
            6,h4("Random seed infection"),
            plotOutput("output_plot11"),
            h5("Infected nodes:",textOutput("infected_random_er")),
            h5("Number of infected nodes:",textOutput("no_infected_random_er"))
            
          ),
          column(
            6,h4("Maximum degree node infection"),
            plotOutput("output_plot12"),
            h5("Infected nodes:",textOutput("infected_max_er")),
            h5("Number of infected nodes:",textOutput("no_infected_max_er"))
          )
        ),
      tabPanel(
        "Results - Power law graph",
        column(
          6,h4("Random seed infection"),
          plotOutput("output_plot21"),
          h5("Infected nodes:",textOutput("infected_random_pl")),
          h5("Number of infected nodes:",textOutput("no_infected_random_pl"))
        ),
        column(
          6,h4("Maximum degree node infection"),
          plotOutput("output_plot22"),
          h5("Infected nodes:",textOutput("infected_max_pl")),
          h5("Number of infected nodes:",textOutput("no_infected_max_pl"))
          
          
        )
      ),
      tabPanel(
        "Graph Information",
        column(
          6,
          h4("ER Model information"),
          h5("Nodes:"),textOutput("text11"),
          h5("No of Edges:"),textOutput("text12"),
          h5("Giant component size:"),textOutput("text13"),
          h5("Random seed nodes:",textOutput("seeds_er")),
          h5("Maximum degree seeds for ER:",textOutput("hs_degree_er"))
          
        ),
        
        column(
          6,
          h4("Power Law Model information"),
          h5("Nodes:"),textOutput("text21"),
          h5("No of Edges:"),textOutput("text22"),
          h5("Giant component size"),tableOutput("text23"),
          h5("Random seed nodes:",textOutput("seeds_pl")),
          h5(
            "Maximum degree seeds for Powerlaw:",textOutput("hs_degree_pl")
          ),hr()
          
        )
      )
    )
  )
)))
