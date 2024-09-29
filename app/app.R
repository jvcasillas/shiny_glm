library(shiny)
library(shinythemes)
library(ggplot2)
library(tibble)
library(stargazer)

# shinylive::export("app", "docs")
# httpuv::runStaticServer("docs")

# Define UI for application that draws a histogram

ui <- shinyUI(fluidPage(theme = shinytheme("spacelab"),
  
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



# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$currentTime <- renderText({
      invalidateLater(1000)
      paste(Sys.time())
    })
    
    # x predictor
    x <- reactive({
      x <- runif(n = input$data_n, min = 0, max = 20)
    })
    
    # Generate outcome
    y <- reactive({
      if (input$outcome_var == 'gaussian') {
        y <- input$beta0 + x() * input$beta1 + rnorm(input$data_n, sd = input$sigma)
      } else if (input$outcome_var == 'poisson'){
        # compute mu's
        mu <- exp(input$beta0 + x() * input$beta1)
        y <- rpois(n = input$data_n, lambda = mu)
      } else {
        s <- input$beta0 + input$beta1 * x() + rnorm(input$data_n, sd = input$sigma)
        y <- rbinom(input$data_n, size = 1, prob = exp(s) / (1 + exp(s)))
      }
    })
    
    mod <- reactive({
      # fit model
      mod <- glm(y() ~ x(), family = input$dist_family)
    })
    
    
    output$glm_plot <- renderPlot({
      
      my_theme <- function(...) {
        list(
          geom_point(size = 3, shape = 21, fill = "grey", color = 'black'), 
          geom_smooth(method = 'glm', formula = 'y ~ x', 
                      method.args = list(family = input$dist_family), 
                      linewidth = 2, color = "darkred"), 
          theme_bw(base_family = 'Palatino', base_size = 20), 
          theme(panel.grid.major = element_line(linewidth = 0.5), 
                panel.grid.minor = element_line(linewidth = 0.5))
        )
      }
      
      # Plot results
      p1 <- tibble(x = x(), y = y()) |>
        ggplot() + 
        aes(x = x, y = y) + 
        my_theme()
      p1
      
    })
    
    output$values <- renderPrint({
      fit <- mod()
      stargazer(fit, type = 'html', single.row=TRUE, 
                ci=TRUE, ci.level=0.95, align=FALSE,
                covariate.labels = c("x", "Intercept"), 
                dep.var.labels = "y")
    })
    
    output$resid_plot <- renderPlot({
      
      my_theme <- function(...) {
        list(
          theme_bw(base_family = 'Palatino', base_size = 20), 
          theme(panel.grid.major = element_line(linewidth = 0.5), 
                panel.grid.minor = element_line(linewidth = 0.5))
        )
      }
      
      gg_qqplot <- function(vec) {
        
        # Get slope and intercept for qqline
        # from base R's qqline()
        y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y) / diff(x)
        int <- y[1L] - slope * x[1L]
        
        # Calculate vec and qq correlation
        the_cor <- cor(vec, qqnorm(vec)$x)
        
        # Create plot
        tibble(resid = vec) |>
          ggplot() + 
          aes(sample = resid) + 
          geom_point(stat = 'qq', shape = 21, size = 3) + 
          geom_abline(slope = slope, intercept = int, linewidth = 2) + 
          annotate("text", x = -1, y = max(y), size = 8,
                   label = paste0("r = ", round(the_cor, 4))) +
          my_theme() 
      }
      
      vec <- y()
      p2 <- gg_qqplot(vec)
      p2
    })
  }



# Create Shiny app ----
shinyApp(ui = ui, server = server)
