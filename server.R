source('functions.R')
library(plotly)

shinyServer(function(input, output, session) {
  graph_er <-
    reactive({
      sample_gnp(as.numeric(input$no_nodes_er),as.numeric(input$probability))%>%set_vertex_attr("color",value="white")
    })

  graph_pl <-
    reactive({
      barabasi.game(as.numeric(input$no_nodes_pl),power=input$power_exponent,directed=FALSE)%>%set_vertex_attr("color",value="white")
    })

  giant_er <- reactive({
    giant_component(graph_er())
  })
  
  giant_pl <- reactive({
    giant_component(graph_pl())
  })
  
  random_seeds_er <-
    reactive({
      c(sample(giant_er(),ifelse(is.null(input$no_seeds_er),0,input$no_seeds_er)))
    })
  
  random_seeds_pl <-
    reactive({
      c(sample(giant_pl(),ifelse(is.null(input$no_seeds_pl),0,input$no_seeds_pl)))
    })
  
  max_seeds_er <-
    reactive({
      max_degree(graph_er(),input$no_seeds_er)
    })
  
  max_seeds_pl <-
    reactive({
      max_degree(graph_pl(),as.numeric(input$no_seeds_pl))
    })
  
  random_seed_er_graph <- reactive({
    infect(
      graph_er(),as.numeric(input$no_runs_er),input$threshold_er,as.character(random_seeds_er())
    )
  })
  
  
  random_seed_pl_graph <- reactive({
    infect(graph_pl(),as.numeric(input$no_runs_pl),input$threshold_pl,as.character(random_seeds_pl()))
  })
  
  max_seed_er_graph <- reactive({
    infect(
      graph_er(),input$no_runs_er,input$threshold_er,as.character(max_seeds_er())
      )
  })
  
  max_seed_pl_graph <- reactive({
    infect(
      graph_pl(),as.numeric(input$no_runs_pl),input$threshold_pl,as.character(max_seeds_pl())
    )
  })
  
  
  output$threshold_er <- renderUI({
    numericInput(
      "threshold_er","Enter a threshold (of infected neighbors) for a node in the ER Graph to be infected",value = "0",min = 0,max = max_degree_value(graph_er(),c(1))
    )
  })
  
  output$threshold_pl <- renderUI({
    numericInput(
      "threshold_pl","Enter a threshold (of infected neighbors) for a node in the power law Graph to be infected ",value = "0",min = 0,max = max_degree_value(graph_pl(),c(1))
    )
  })
  
  observe({
    input$probability 
    updateSliderInput(
          session ,"no_seeds_er","Number of seeds for ER Graph",min = 0,max = length(giant_er()),value =
            1,step = 1
        )
  })
  output$random_seeds_pl <-
    renderUI({
      sliderInput(
        "no_seeds_pl","Number of seeds for power law graph",min = 1,max = length(giant_pl()),value =
          1,step = 1
      )
    })
  
  output$seeds_er <- renderText(random_seeds_er())
  output$seeds_pl <- renderText(random_seeds_pl())
  output$hs_degree_er <- renderText(max_seeds_er())
  output$hs_degree_pl <- renderText(max_seeds_pl())
  output$output_plot1 <- renderPlot(plot(graph_er(),vertex.frame.color='gray',mark.expand=200))
  output$output_plot11 <- renderPlot(plot(random_seed_er_graph()))
  output$infected_random_er <-
    renderText(infected_nodes(random_seed_er_graph()))
  output$no_infected_random_er <-
    renderText(length(infected_nodes(random_seed_er_graph())))
  
    output$output_plot12 <- renderPlot(plot(max_seed_er_graph()))
  output$infected_max_er <-
    renderText(infected_nodes(max_seed_er_graph()))
  output$no_infected_max_er <-
    renderText(length(infected_nodes(max_seed_er_graph())))
  
  output$output_plot13 <-
    renderPlot(plot(degree_distribution(graph_er()),xlab="Node Degree",ylab="Network fraction"))
  output$output_plot2 <- renderPlot(plot(graph_pl()))
  output$output_plot21 <- renderPlot(plot(random_seed_pl_graph()))
  output$infected_random_pl <-
    renderText(infected_nodes(random_seed_pl_graph()))
  output$no_infected_random_pl <-
    renderText(length(infected_nodes(random_seed_pl_graph())))
  
  output$output_plot22 <- renderPlot(plot(max_seed_pl_graph()))
  output$infected_max_pl <-
    renderText(infected_nodes(max_seed_pl_graph()))
  output$no_infected_max_pl <-
    renderText(length(infected_nodes(max_seed_pl_graph())))
  
  output$output_plot23 <-
    renderPlot(plot(degree_distribution(graph_pl()),xlab = 'Node Degree',ylab='Network fraction'))
  output$text11 <- renderText(length(V(graph_er())))
  output$text12 <- renderText(ecount(graph_er()))
  output$text13 <- renderText(toString(length(giant_er())))
  output$text21 <- renderText(length(V(graph_pl())))
  output$text22 <- renderText(ecount(graph_pl()))
  output$text23 <- renderText(toString(length(giant_pl())))
  
})
