library(shiny)
library(tidyverse)
library(shinythemes)
library(patchwork)
library(stargazer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
        geom_smooth(method = 'glm', 
                    method.args = list(family = input$dist_family), 
                    size = 2, color = "darkred"), 
        theme_minimal(base_family = 'Times', base_size = 20), 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
        )
    }

    # Plot results
    p1 <- tibble(x = x(), y = y()) %>%
      ggplot(., aes(x = x, y = y)) + 
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
        theme_minimal(base_family = 'Times', base_size = 20), 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
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
    tibble(resid = vec) %>%
    ggplot(., aes(sample = resid)) + 
      geom_point(stat = 'qq', shape = 21, size = 3) + 
      geom_abline(slope = slope, intercept = int, size = 2) + 
      annotate("text", x = -1, y = max(y), size = 8,
               label = paste0("r = ", round(the_cor, 4))) +
      my_theme() 
    }

    vec <- y()
    p2 <- gg_qqplot(vec)
    p2
  })
})


