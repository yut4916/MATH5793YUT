# First Shiny App
# Katy Yut
# Feb 22, 2021

# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(latex2exp)
library(rootSolve)

# Read in data
# data <- read.csv("/Users/Katy/Desktop/07_school/MATH5793/06_data/fourmeasure.csv", header=TRUE)
data("T4.3")
data <- T4.3

### UI ============================================================================#######
ui <- fluidPage(
  titlePanel("Katy Yut's Shiny App"),

  sidebarLayout(
    sidebarPanel(
      # Task 1 sidebar
      h1("Task 1 Plot"),
      selectInput("x_axis1", "Horizontal Axis Variable",
                  choices = colnames(data)),
      selectInput("y_axis1", "Vertical Axis Variable",
                  choices = colnames(data)),
      sliderInput("t1_pch", "Point Size",
                  min=0, max=5, value = 2
      ),
      selectInput("t1_color", "Point Color",
                  choices = c("Red", "Orange", "Yellow", "Green", "Blue", "Purple", "Black", "White")),


      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

      # Task 2 sidebar
      h1("Task 2 Plot"),
      selectInput("x_axis2", "Horizontal Axis Variable",
                  choices = colnames(data)),
      selectInput("y_axis2", "Vertical Axis Variable",
                  choices = colnames(data)),
      sliderInput(
        inputId = "theta",
        label="Theta",
        min=0,
        max=round(pi/2, 2),
        value=0,
        post = " radians"
      )
    ),

    mainPanel(
      plotOutput("task1", click = "plot_click"),
      textOutput("clickCor"),
      br(), br(), br(),
      plotOutput("task2")
    )
  )
)

### SERVER ========================================================================#######
server <- function(input, output) {
  # Task 1
  output$task1 <- renderPlot({
    ggplot(data=data, aes(x=data[,input$x_axis1], y=data[,input$y_axis1])) +
      geom_point(size=as.numeric(input$t1_pch), color=input$t1_color) +
      ggtitle("Task 1: Drop-One Correlation") +
      xlab(input$x_axis1) +
      ylab(input$y_axis1) +
      labs(subtitle="Click data point to calculate correlations")
  })

  output$clickCor <- renderText({
    req(input$plot_click)
    data$rowN <- 1:dim(data)[1]
    droppedPoint <- (nearPoints(data, input$plot_click, xvar=input$x_axis1, yvar=input$y_axis1))
    dropCor <- round(cor(data[-droppedPoint$rowN, c(input$x_axis1, input$y_axis1)])[1,2], 3)

    paste0("Correlation = ", round(cor(x=data[,input$x_axis1], y=data[,input$y_axis1]), 3), ", ", "\nDrop-One Correlation = ", dropCor)
  })



  # Task 2
  # calculate s12t
  corMat <- reactive({cor(data[,c(input$x_axis2,input$y_axis2)])})
  s12 <- reactive({corMat()[1,2]})
  s11 <- reactive({corMat()[1,1]})
  s22 <- reactive({corMat()[2,2]})
  x <- reactive({as.numeric(input$theta)})
  s12t <-  reactive({s12()*(cos(x())^2 - sin(x())^2) + (s22() - s11())*sin(x())*cos(x())})

  # calculate s12t=0 solution for quadrant 1
  q1_sol <- reactive({
    uniroot(function(x){s12()*(cos(x)^2 - sin(x)^2) + (s22() - s11())*sin(x)*cos(x)}, c(0, pi/2))$root
  })

  output$task2 <- renderPlot({
    ggplot(data=data, aes(x=data[,input$x_axis2], y=data[,input$y_axis2])) +
      geom_point() +
      geom_abline(slope=tan(q1_sol()), intercept=0, color="grey", linetype="dashed") +
      geom_text(aes(x=0.8*max(data[,input$x_axis2]), y=0.1*min(data[,input$y_axis2]), label=paste0("Q1 solution: theta = ", round(q1_sol(), 2))), color="grey") +
      geom_hline(yintercept=0) +
      geom_vline(xintercept=0) +
      geom_abline(slope=tan(input$theta), intercept=0, color="red") +
      geom_abline(slope=tan(input$theta - pi/2), intercept=0, color="red") +
      coord_fixed() +
      ggtitle("Task 2: Rotating Axes") +
      xlab(input$x_axis2) +
      ylab(input$y_axis2) +
      labs(subtitle = TeX(sprintf('$\\tilde{s_{12}} = %f$', s12t())))
  })
}


### CALL APP ========================================================================#######
shinyApp(ui = ui, server = server)
