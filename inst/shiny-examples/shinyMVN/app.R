# Second Shiny App
# Katy Yut
# March 24, 2021

# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(latex2exp)
library(rootSolve)
library(rlang) # .data ?
library(tidyr) # pivot_longer
library(MATH5793YUT)
library(gridExtra)
library(GGally)

# Read in data
# data <- read.csv("/Users/Katy/Desktop/07_school/MATH5793/06_data/fourmeasure.csv", header=TRUE)
data("T4.3")
data <- T4.3

### UI ============================================================================#######
ui <- fluidPage(
  titlePanel("Assessing Multivariate Normality"),

  sidebarLayout(
    sidebarPanel(
      # Task 1 sidebar
      h1("Task 1:"),
      fileInput("file", "Upload a file", accept = ".csv"),
      h2("Proportion Test of Normality"),
      radioButtons("t1_var", "Graph dotplots for the following variable",
                   choices = colnames(data)), #, selected=character(0)

      br(), br(), br(), br(), br(), br(), br(),

      # Task 2 sidebar
      h1("Task 2:"),
      h2("Assessing Univariate Normality"),
      selectInput("t2_var", "Q-Q Plot Variable",
                  choices = colnames(data)),

      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

      # Task 3 sidebar
      h1("Task 3:"),
      h2("Assessing Bivariate Normality"),
      selectInput("t3_var1", "First Variable",
                  choices = colnames(data)),
      selectInput("t3_var2", "Second Variable",
                  choices = colnames(data), selected=colnames(data)[2]),
      sliderInput("alpha", "Alpha",
                  min=0, max=1, value = 0.05),

      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

      # Task 4 sidebar
      h1("Task 4:"),
      h2("Detecting Outliers"),
      checkboxGroupInput("t4_vars", "Joint Distribution Variables",
                         choices = colnames(data), selected=colnames(data)[1:5]),

      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),

      # Task 5 sidebar
      h1("Task 5:"),
      h2("Transforming the Data to Near Normality"),
      selectInput("t5_var", "Box-Cox Variable",
                  choices = colnames(data)),
      numericInput("lambda", "Lambda",
                   min=-3, max=5, value = 1),

      br(), br(), br(), br(), br(), br(), br(), br(), br()
    ),

    mainPanel(
      h1("Task 1:"),
      h2("Proportion Test of Normality"),
      plotOutput("task1"),
      textOutput("t1_Results"),
      h1("Task 2:"),
      h2("Assessing Univariate Normality"),
      plotOutput("task2"),
      h2("Q-Q Plot Correlation Coefficients & Shapiro-Wilk Test P-Values"),
      tableOutput("task2T"),
      h1("Task 3:"),
      h2("Assessing Bivariate Normality"),
      plotOutput("task3a"),
      plotOutput("task3b"),
      h1("Task 4:"),
      h2("Detecting Outliers"),
      plotOutput("task4"),
      plotOutput("t4_Z"),
      h1("Task 5:"),
      h2("Transforming the Data to Near Normality"),
      plotOutput("task5"),
      plotOutput("t5_trans"),
      textOutput("t5_p")
    )
  )
)

### SERVER ========================================================================#######
server <- function(input, output) {
  # Task 1
  # Read in the data
  data <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)

    if(ext=="csv"){
      read.csv(input$file$datapath)
    } else{
      validate("Invalid file; Please upload a .csv file")
    }
  })

  # Task 1
  # Load data
  t1_data <- reactive(data()[input$t1_var])

  # Run test on data
  data_bool1sd <- reactive(MATH5793YUT::ptn4.29(t1_data())$within_1sd)
  data_bool2sd <- reactive(MATH5793YUT::ptn4.29(t1_data())$within_2sd)

  # Reformat test results
  bool1sd <- reactive(tidyr::pivot_longer(data_bool1sd(), cols=colnames(data_bool1sd()), names_to="Variable", values_to="Test1"))
  bool2sd <- reactive(tidyr::pivot_longer(data_bool2sd(), cols=colnames(data_bool2sd()), names_to="Variable", values_to="Test2"))

  # Re-format data for plot
  data2.0 <- reactive(tidyr::pivot_longer(t1_data(), cols=input$t1_var, names_to="Variable", values_to="Value"))

  # # Create columns & fill with test results
  # eventReactive(bool1sd(), {data2.0$Test1 <- bool1sd()$Test1})
  # eventReactive(bool2sd(), {data2.0$Test2 <- bool2sd()$Test2})

  # Create plot (1 sd)
  g1 <- reactive({
    ggplot2::ggplot(data=data2.0(), ggplot2::aes(x=factor(Variable), y=Value)) +
      ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=1, ggplot2::aes(fill=bool1sd()$Test1)) +
      ggplot2::ggtitle(paste0("Univariate Dotplot for ", input$t1_var)) +
      ggplot2::xlab("Variable") +
      ggplot2::ylab("Value") +
      ggplot2::labs(subtitle="Within 1 Standard Deviation", fill="Within")
  })

  # Create plot (2 sd)
  g2 <- reactive({
    ggplot2::ggplot(data=data2.0(), ggplot2::aes(x=factor(Variable), y=Value)) +
      ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=1, ggplot2::aes(fill=bool2sd()$Test2)) +
      ggplot2::ggtitle(paste0("Univariate Dotplot for ", input$t1_var)) +
      ggplot2::xlab("Variable") +
      ggplot2::ylab("Value") +
      ggplot2::labs(subtitle="Within 2 Standard Deviations", fill="Within")
  })

  output$task1 <- renderPlot({
    gridExtra::grid.arrange(g1(), g2(), ncol=2)
  })

  testResults <- reactive(MATH5793YUT::ptn4.29(t1_data())$testResults)

  output$t1_Results <- renderText({
    paste0("Variable ", input$t1_var, ":\n  ",
           if(as.logical(testResults()["Test_1SD",])){"Passed"}else{"Failed"}, " normality check for one standard deviation.\n  ",
           if(as.logical(testResults()["Test_2SD",])){"Passed"}else{"Failed"}, " normality check for two standard deviations.\n  ")
  })

  # Task 2
  output$task2 <- renderPlot({
    MATH5793YUT::qqplot(data()[[input$t2_var]])
  })

  t2_table <- reactive({
    rQ <- c()
    shapiroPs <- c()
    for(i in 1:length(colnames(data()))){
      col <- colnames(data())[i]
      rQ_i <- MATH5793YUT::rQ(data()[[col]])
      shapiro_i <- shapiro.test(data()[ ,col])$p.value

      rQ <- c(rQ, rQ_i)
      shapiroPs <- c(shapiroPs, shapiro_i)
    }

    t2_table <- data.frame(rbind(rQ, shapiroPs))
    colnames(t2_table) <- colnames(data)
    t2_table
  })


  output$task2T <- renderTable({
    t2_table()
  }, rownames=TRUE, width="80%")

  # Task 3
  t3_data <- reactive(data()[c(input$t3_var1, input$t3_var2)])

  output$task3a <- renderPlot({
    ggplot2::ggplot(t3_data(), ggplot2::aes(x=t3_data()[,input$t3_var1], y=t3_data()[,input$t3_var2])) +
      ggplot2::geom_point() +
      ggplot2::stat_ellipse(level=1-input$alpha) +
      ggplot2::ggtitle(paste0(100*(1-input$alpha),"% Data Ellipse")) +
      ggplot2::xlab(paste0("Variable ", input$t3_var1)) +
      ggplot2::ylab(paste0("Variable ", input$t3_var2))
  })

  n <- reactive(dim(t3_data())[1])
  t3_xbar <- reactive(matrix(colMeans(t3_data()), nrow=2))
  Si <- reactive(solve(cov(t3_data())))

  # Compute the generalized squared distance
  gsd_ord <- eventReactive(t3_data(), {
    gsd <- NA
    for(j in 1:n()){
      gsd[j] <- t(t(t3_data()[j,])-t3_xbar())%*%Si()%*%(t(t3_data()[j,])-t3_xbar())
    }
    gsd[order(gsd)]
  })

  # Compute the Chi-Squared statistic
  chi_stat <- reactive(qchisq((1:n() - 0.5)/n(), df=2))

  output$task3b <- renderPlot({
    ggplot2::ggplot(data=data.frame(chi_stat(), gsd_ord()), ggplot2::aes(x=chi_stat(), y=gsd_ord())) +
      ggplot2::geom_point()  +
      ggplot2::ggtitle("Bivariate Chi-Squared Plot") +
      ggplot2::xlab("Quantile") +
      ggplot2::ylab("Generalized Squared Distance") +
      ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed") +
      ggplot2::labs(subtitle=paste0("Assessing Joint Normality of Variable ", input$t3_var1, " and ", input$t3_var2))
  })

  # Task 4
  t4_data <- reactive(data()[input$t4_vars])

  output$task4 <- renderPlot({
    GGally::ggpairs(t4_data())
  })

  # Standardize the data
  t4_xbar <- reactive(colMeans(t4_data()))
  S <- reactive(cov(t4_data()))
  Z <- reactive(t((t(t4_data())-t4_xbar())/sqrt(diag(S()))))
  large <- reactive(abs(Z()) > 3)

  # Re-format data for plot
  t4_data2.0 <- reactive(tidyr::pivot_longer(data.frame(Z()), cols=all_of(input$t4_vars), names_to="Variable", values_to="Z"))

  # Run normality test of proportions
  t4_dataXL <- reactive(tidyr::pivot_longer(data.frame(large()), cols=all_of(input$t4_vars), names_to="Variable", values_to="Large"))
  # eventReactive(t4_dataXL(), {t4_data2.0$Large <- t4_dataXL()$Large})

  # Standardized Data Plot
  output$t4_Z <- renderPlot({
    ggplot2::ggplot(data=t4_data2.0(), ggplot2::aes(x=factor(Variable), y=Z)) +
      ggplot2::geom_dotplot(binaxis='y', stackdir='center', dotsize=1, ggplot2::aes(fill=t4_dataXL()$Large)) +
      ggplot2::ggtitle(paste0("Standardized Dotplots")) +
      ggplot2::xlab("Variable") +
      ggplot2::ylab("Z-score") +
      ggplot2::labs(subtitle="Large defined as |Z| > 3", fill="Large")
  })

  # Task 5
  t5_data <- reactive(data()[[input$t5_var]])
  l <- reactive(MATH5793YUT::boxCox(t5_data(), input$lambda))
  boxCocks <- reactive(MATH5793YUT::boxCoxMax(t5_data(), increment=0.01))

  output$task5 <- renderPlot({
    boxCocks()$plot +
      ggplot2::geom_point(x=input$lambda, y=l(), color="red") +
      ggplot2::geom_vline(xintercept=boxCocks()$solution, linetype="dashed") +
      ggplot2::geom_text(label=paste0("Max @ lambda = ", boxCocks()$solution),
                         x=boxCocks()$solution-2,
                         y=boxCocks()$maximum)
  })

  # Transform the data using user input
  t5_dataTrans <- reactive(MATH5793YUT::powerTrans(t5_data(), input$lambda))

  output$t5_trans <- renderPlot({
    GGally::ggpairs(data.frame(t5_dataTrans()))
  })

  output$t5_p <- renderText({
    paste0("Shapiro-Wilk Test p-value = ", round(shapiro.test(t5_dataTrans())$p.value, 4))
  })
}


### CALL APP ========================================================================#######
shinyApp(ui = ui, server = server)
