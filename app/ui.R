library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)
library(modelsummary)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("GLM"),
  
  # Sidebar with dynamic variables
  sidebarLayout(
    sidebarPanel(width = 3, 
       selectInput("outcome_var", 
                   "Outcome variable:",
                  c("continuous" = "gaussian", 
                    "count" = "poisson", 
                    "categorical" = "binomial")),
       selectInput("dist_family", 
                   "Distribution family:",
                  c("gaussian", "poisson", "binomial")),
       div(align = "center", 
       numericInput("data_n", 
                    withMathJax("$$N$$"), 
                    value = 100, 
                    min = 1, 
                    max = 1000)),
       div(align = "center", 
       numericInput("beta0", 
                   withMathJax("$$\\beta_0$$"), 
                   min = -20, 
                   max = 20, 
                   value = 0)), 
       div(align = "center", 
       numericInput("beta1", 
                   withMathJax("$$\\beta_1$$"), 
                   min = -10, 
                   max = 10, 
                   value = 0, 
                   step = 0.02)), 
       div(align = "center", 
       sliderInput("sigma", 
                   withMathJax("$$\\sigma$$"), 
                   min = 0, 
                   max = 10, 
                   value = 1)),
       br(),
       p(strong("Created by:"), 
         tags$a("Joseph V. Casillas", href="http://www.jvcasillas.com"),
       br(), 
         strong("Source code:"), 
         tags$a("Github", href="https://github.com/jvcasillas/shiny_glm"))),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(8,
        br(),
        tabsetPanel(type = "tabs",
          tabPanel("Plot", plotOutput("glm_plot")), 
          tabPanel("Residuals", plotOutput("resid_plot"))
          )
        ),
        column(4,
          br(),br(),br(),br(),br(),br(),br(),br(),
          div(align = "center", tableOutput("values"))
        )
      )
    )
  )
))
