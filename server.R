source('functions.R')
shinyServer(function(input, output) {
  graph_er <-
    reactive({
      sample_gnp(as.numeric(input$no_nodes),as.numeric(input$probability))
    })

#     graph_pl <-
#     reactive({
#       sample_pa(
#         as.numeric(input$no_nodes),as.numeric(input$power_exponent),zero.appeal =
#           1,directed = FALSE
#       )
#     })
#   
    
    graph_pl <-
      reactive({
        barabasi.game(as.numeric(input$no_nodes),power=input$power_exponent,directed=FALSE)
      })
    
    
    
    
    giant_er <- reactive({
    giant_component(graph_er())
  })
  giant_pl <- reactive({
    giant_component(graph_pl())
  })
  random_seeds_er <-
    reactive({
      c(sample(giant_er(),as.numeric(input$no_seeds_er)))
    })
  random_seeds_pl <-
    reactive({
      c(sample(giant_pl(),as.numeric(input$no_seeds_pl)))
    })
  max_seeds_er <-
    reactive({
      max_degree(graph_er(),as.numeric(input$no_seeds_er))
    })
  
  max_seeds_pl <-
    reactive({
      max_degree(graph_pl(),as.numeric(input$no_seeds_pl))
    })
  
  random_seed_er_graph <- reactive({
    infect(
      graph_er(),as.numeric(input$no_runs),input$threshold_er,as.character(random_seeds_er())
    )
  })
  
  
  random_seed_pl_graph <- reactive({
    infect(graph_pl(),as.numeric(input$no_runs),input$threshold_pl,as.character(random_seeds_pl()))
  })
  
  max_seed_er_graph <- reactive({
    infect(
      graph_er(),as.numeric(input$no_runs),input$threshold_er,as.character(max_seeds_er())
    )
  })
  
  max_seed_pl_graph <- reactive({
    infect(
      graph_pl(),as.numeric(input$no_runs),input$threshold_pl,as.character(max_seeds_pl())
    )
  })
  
  #   degrees_er<-reactive({table(degree(induced.subgraph(graph_er(),(giant_er()))))})
  #   degrees_pl<-reactive({table(degree(induced.subgraph(graph_pl(),(giant_pl()))))})
  
  #
  output$threshold_er <- renderUI({
    numericInput(
      "threshold_er","Threshold for ER Graph ",value = "0",min = 0,max = max_degree_value(graph_er(),c(1))
    )
  })
  output$threshold_pl <- renderUI({
    numericInput(
      "threshold_pl","Threshold for power law Graph ",value = "0",min = 0,max = max_degree_value(graph_pl(),c(1))
    )
  })
  output$random_seeds_er <-
    renderUI({
      sliderInput(
        "no_seeds_er","Number of seeds for ER Graph",min = 1,max = length(giant_er()),value =
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
  output$output_plot1 <- renderPlot(plot(graph_er()))
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
    renderPlot(plot(degree_distribution(graph_er()),xlab = 'Node Degree',ylab='Network fraction'))
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
