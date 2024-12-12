# Required libraries ----
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

# Configuration ----
source("config/colors.R")        # Contains stanford_colors list
source("config/distributions.R") # Contains distributions list
source("helpers.R")              # Contains helper functions


# UI Definition ----
ui <- page_navbar(
  theme = create_theme(dark_mode = FALSE),

  title = "Probability Distributions Explorer",

  nav_item(
    input_dark_mode(id = "dark_mode")
  ),

  ## Sidebar ----
  sidebar = sidebar(
    ### Distribution Settings ----
    selectInput("dist_type", "Select Distribution:",
                choices = names(distributions),
                selected = "Normal"),
    uiOutput("parameter_inputs"),
    uiOutput("validation_message"),

    hr(),
    ### Visualization Options ----
    checkboxInput("show_cumulative", "Show Cumulative Function", TRUE),
    checkboxInput("show_prob", "Show Probability Region", FALSE),
    conditionalPanel(
      condition = "input.show_prob == true",
      numericInput("lower_bound", "Lower Bound:", 0),
      numericInput("upper_bound", "Upper Bound:", 1)
    ),

    hr(),
    ### Comparison settings ----
    checkboxInput("show_comparison", "Show Comparison Distribution", FALSE),
    conditionalPanel(
      condition = "input.show_comparison == true",
      selectInput("comp_dist", "Compare with:",
                  choices = names(distributions),
                  selected = "Normal"),
      uiOutput("comp_parameter_inputs")
    ),

    hr(),
    ### CLT Demonstration ----
    checkboxInput("show_clt", "Demonstrate Central Limit Theorem", FALSE),
    conditionalPanel(
      condition = "input.show_clt == true",
      numericInput("sample_size", "Sample Size:", 1, min = 1),
      numericInput("num_samples", "Number of Samples:", 1000, min = 100),
      tags$small(
        class = "text-muted",
        "Increase the sample size to see the distribution converge to normal."
      )
    )
  ),

  ## Main panel content ----
  nav_panel(
    "",
    uiOutput("plots_container")
  )
)

# Server Logic ----
server <- function(input, output, session) {

  # Theme Management ----
  observe({
    is_dark <- isTRUE(input$dark_mode == "dark")
    session$setCurrentTheme(create_theme(dark_mode = is_dark))
  })

  plot_theme <- reactive({
    is_dark <- isTRUE(input$dark_mode == "dark")
    create_plot_theme(dark_mode = is_dark)
  })

  # Parameter Management ----
  ## Get current parameter values for main distribution ----
  get_params <- reactive({
    dist <- distributions[[input$dist_type]]

    # Ensure all parameters exist before proceeding
    req(all(sapply(seq_along(dist$params), function(i) !is.null(input[[paste0("param_", i)]]))))

    # Get values for each parameter
    params <- lapply(seq_along(dist$params), function(i) {
      value <- input[[paste0("param_", i)]]
      # Use default value if input is invalid
      if (is.null(value) || !is.numeric(value)) {
        value <- dist$defaults[i]
      }
      value
    })

    # Name the parameters according to the distribution specification
    setNames(params, dist$params)
  })

  ## Get current parameter values for comparison distribution ----
  get_comp_params <- reactive({
    req(input$show_comparison)
    dist <- distributions[[input$comp_dist]]

    # Ensure all parameters exist before proceeding
    req(all(sapply(seq_along(dist$params), function(i) !is.null(input[[paste0("comp_param_", i)]]))))

    # Get values for each parameter
    params <- lapply(seq_along(dist$params), function(i) {
      value <- input[[paste0("comp_param_", i)]]
      # Use default value if input is invalid
      if (is.null(value) || !is.numeric(value)) {
        value <- dist$defaults[i]
      }
      value
    })

    # Name the parameters according to the distribution specification
    setNames(params, dist$params)
  })

  ## Generate dynamic parameter inputs for main distribution ----
  output$parameter_inputs <- renderUI({
    dist <- distributions[[input$dist_type]]
    param_inputs <- lapply(seq_along(dist$params), function(i) {
      param_name <- dist$params[i]
      default_value <- dist$defaults[i]
      param_range <- dist$param_ranges[[param_name]]

      # Set min/max based on the ranges
      min_val <- if(is.infinite(param_range[1])) -1000 else param_range[1]
      max_val <- if(is.infinite(param_range[2])) 1000 else param_range[2]

      # Adjust step size based on parameter type
      step_size <- if(dist$discrete && param_name == "size") {
        1
      } else if(param_name %in% c("prob", "rate", "scale")) {
        0.1
      } else {
        0.5
      }

      div(
        numericInput(
          paste0("param_", i),
          paste0(param_name, if(dist$discrete && param_name == "size") " (integer)" else ""),
          value = default_value,
          min = min_val,
          max = max_val,
          step = step_size
        ),
        tags$small(
          class = "text-muted",
          if(is.infinite(param_range[2])) {
            sprintf("Must be %s > %s", param_name, param_range[1])
          } else {
            sprintf("Range: [%s, %s]", param_range[1], param_range[2])
          }
        )
      )
    })
    do.call(tagList, param_inputs)
  })

  ## Generate dynamic parameter inputs for comparison distribution ----
  output$comp_parameter_inputs <- renderUI({
    req(input$show_comparison)
    dist <- distributions[[input$comp_dist]]
    param_inputs <- lapply(seq_along(dist$params), function(i) {
      param_name <- dist$params[i]
      default_value <- dist$defaults[i]
      param_range <- dist$param_ranges[[param_name]]

      # Set min/max based on the ranges
      min_val <- if(is.infinite(param_range[1])) -1000 else param_range[1]
      max_val <- if(is.infinite(param_range[2])) 1000 else param_range[2]

      # Adjust step size based on parameter type
      step_size <- if(dist$discrete && param_name == "size") {
        1
      } else if(param_name %in% c("prob", "rate", "scale")) {
        0.1
      } else {
        0.5
      }

      div(
        numericInput(
          paste0("comp_param_", i),
          paste0("Comparison ", param_name, if(dist$discrete && param_name == "size") " (integer)" else ""),
          value = default_value,
          min = min_val,
          max = max_val,
          step = step_size
        ),
        tags$small(
          class = "text-muted",
          if(is.infinite(param_range[2])) {
            sprintf("Must be %s > %s", param_name, param_range[1])
          } else {
            sprintf("Range: [%s, %s]", param_range[1], param_range[2])
          }
        )
      )
    })
    do.call(tagList, param_inputs)
  })

  # Parameter Validation Functions ----
  validate_params <- function(dist_type, params, is_comparison = FALSE) {
    dist <- distributions[[dist_type]]

    # Validate all parameters
    errors <- character(0)

    for(param_name in names(params)) {
      value <- params[[param_name]]
      range <- dist$param_ranges[[param_name]]

      # Check if value is numeric and finite
      if(is.null(value) || !is.numeric(value) || !is.finite(value)) {
        prefix <- if(is_comparison) "Comparison " else ""
        errors <- c(errors, sprintf("%s%s must be a valid number", prefix, param_name))
        next
      }

      # Check if value is within valid range
      if(value < range[1]) {
        prefix <- if(is_comparison) "Comparison " else ""
        errors <- c(errors, sprintf("%s%s must be greater than %s",
                                    prefix, param_name,
                                    if(range[1] == 0) "0" else range[1]))
      }
      if(value > range[2]) {
        prefix <- if(is_comparison) "Comparison " else ""
        errors <- c(errors, sprintf("%s%s must be less than %s",
                                    prefix, param_name,
                                    if(is.infinite(range[2])) "∞" else range[2]))
      }

      # Check for integer requirement for discrete parameters
      if(dist$discrete && param_name == "size") {
        if(abs(value - round(value)) > .Machine$double.eps^0.5) {
          prefix <- if(is_comparison) "Comparison " else ""
          errors <- c(errors, sprintf("%s%s must be an integer", prefix, param_name))
        }
      }
    }

    # Special case validations
    if(dist_type == "Uniform") {
      prefix <- if(is_comparison) "Comparison " else ""
      if(params$max <= params$min) {
        errors <- c(errors, sprintf("%smaximum must be greater than minimum", prefix))
      }
    }

    if(dist_type == "Hypergeometric") {
      prefix <- if(is_comparison) "Comparison " else ""
      if(params$k > (params$m + params$n)) {
        errors <- c(errors, sprintf("%snumber of draws (k) cannot exceed total number of balls (m + n)", prefix))
      }
    }

    if(length(errors) > 0) return(errors)
    return(NULL)
  }

  # In the server function, update these reactive expressions:
  get_validation_errors <- reactive({
    # Get main distribution errors
    main_errors <- validate_params(input$dist_type, get_params())

    # Get comparison distribution errors if needed
    comp_errors <- if(input$show_comparison) {
      validate_params(input$comp_dist, get_comp_params(), is_comparison = TRUE)
    } else NULL

    # Combine all errors
    c(main_errors, comp_errors)
  })

  # Update the validation message UI output
  output$validation_message <- renderUI({
    errors <- get_validation_errors()

    if(!is.null(errors)) {
      div(
        class = "alert alert-danger",
        style = "margin-top: 10px;",
        tags$b("Invalid parameters:"),
        tags$ul(
          lapply(errors, function(error) {
            tags$li(error)
          })
        )
      )
    }
  })


  # Distribution Data Generation ----
  generate_dist_data <- function(dist_type, params) {
    dist <- distributions[[dist_type]]

    if (dist$discrete) {
      max_x <- switch(dist_type,
                      "Binomial" = params$size,
                      "Poisson" = min(qpois(0.999, params$lambda), 100),
                      "Geometric" = min(qgeom(0.999, params$prob), 50),
                      "Negative Binomial" = min(qnbinom(0.999, params$size, params$prob), 100),
                      "Hypergeometric" = min(params$k, params$m),
                      30)
      x <- 0:max_x
    } else {
      if (!is.null(dist$range)) {
        x <- seq(dist$range[1], dist$range[2], length.out = 200)
      } else {
        x <- seq(-10, 10, length.out = 200)
      }
    }

    d_func <- get(paste0("d", dist$r_name))
    p_func <- get(paste0("p", dist$r_name))

    density <- do.call(d_func, c(list(x), params))
    cumulative <- do.call(p_func, c(list(x), params))

    data.frame(x = x, density = density, cumulative = cumulative)
  }

  dist_data <- reactive({
    tryCatch({
      generate_dist_data(input$dist_type, get_params())
    }, error = function(e) NULL)
  })

  comp_dist_data <- reactive({
    req(input$show_comparison)
    tryCatch({
      generate_dist_data(input$comp_dist, get_comp_params())
    }, error = function(e) NULL)
  })

  # Plot Generation ----
  ## Base Plot ----
  create_base_plot <- function(title = "", type = "density") {
    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    ggplot() +
      labs(title = title,
           x = "Value",
           y = if(type == "density") "Density" else "Cumulative Probability") +
      plot_theme()
  }

  ## Error Plot ----
  create_error_plot <- function(message = "Please fix parameter errors to display plot") {
    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = message,
               size = 6,
               color = colors$fg) +
      theme_void() +
      theme(
        panel.background = element_rect(
          fill = colors$bg,
          color = colors$border
        )
      )
  }

  # Plotting Functions with Error Handling ----

  ## Density Plot ----
  output$densityPlot <- renderPlot({
    # Check for validation errors first
    errors <- get_validation_errors()

    if(!is.null(errors)) {
      return(create_error_plot())
    }

    # Get data and check if valid
    data <- dist_data()
    if(is.null(data)) {
      return(create_error_plot("Unable to generate plot data"))
    }

    dist <- distributions[[input$dist_type]]
    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    p <- create_base_plot({
      dist <- distributions[[input$dist_type]]
      main_type <- if(dist$discrete) "PMF" else "PDF"

      if(input$show_comparison) {
        comp_dist <- distributions[[input$comp_dist]]
        comp_type <- if(comp_dist$discrete) "PMF" else "PDF"
        sprintf("%s %s vs. %s %s", input$dist_type, main_type, input$comp_dist, comp_type)
      } else {
        sprintf("%s %s", input$dist_type, main_type)
      }
    }, "density")

    # Add main distribution
    if(dist$discrete) {
      p <- p +
        geom_segment(data = data,
                     aes(x = x, xend = x, y = 0, yend = density),
                     color = colors$primary) +
        geom_point(data = data,
                   aes(x = x, y = density),
                   color = colors$primary)
    } else {
      p <- p + geom_line(data = data,
                         aes(x = x, y = density),
                         color = colors$primary)
    }

    # Add comparison distribution if requested
    if(input$show_comparison) {
      comp_data <- comp_dist_data()
      if(!is.null(comp_data)) {
        comp_dist <- distributions[[input$comp_dist]]
        if(comp_dist$discrete) {
          p <- p +
            geom_segment(data = comp_data,
                         aes(x = x, xend = x, y = 0, yend = density),
                         color = colors$info, alpha = 0.6) +
            geom_point(data = comp_data,
                       aes(x = x, y = density),
                       color = colors$info)
        } else {
          p <- p + geom_line(data = comp_data,
                             aes(x = x, y = density),
                             color = colors$info,
                             alpha = 0.6)
        }
      }
    }

    # Add probability region if requested
    if(input$show_prob) {
      # Add vertical lines for bounds
      p <- p +
        geom_vline(xintercept = c(input$lower_bound, input$upper_bound),
                   linetype = "dashed",
                   color = colors$secondary,
                   alpha = 0.5)

      # Add shading for main distribution
      if(dist$discrete) {
        subset_data <- subset(data, x >= input$lower_bound & x <= input$upper_bound)
        p <- p +
          geom_segment(data = subset_data,
                       aes(x = x, xend = x, y = 0, yend = density),
                       color = colors$primary,
                       alpha = 0.8,
                       linewidth = 2)
      } else {
        subset_data <- subset(data, x >= input$lower_bound & x <= input$upper_bound)
        p <- p + geom_area(data = subset_data,
                           aes(x = x, y = density),
                           fill = colors$primary,
                           alpha = 0.3)
      }

      # Add shading for comparison distribution if present
      if(input$show_comparison) {
        comp_data <- comp_dist_data()
        if(!is.null(comp_data)) {
          comp_dist <- distributions[[input$comp_dist]]
          if(comp_dist$discrete) {
            comp_subset_data <- subset(comp_data, x >= input$lower_bound & x <= input$upper_bound)
            p <- p +
              geom_segment(data = comp_subset_data,
                           aes(x = x, xend = x, y = 0, yend = density),
                           color = colors$info,
                           alpha = 0.8,
                           linewidth = 2)
          } else {
            comp_subset_data <- subset(comp_data, x >= input$lower_bound & x <= input$upper_bound)
            p <- p + geom_area(data = comp_subset_data,
                               aes(x = x, y = density),
                               fill = colors$info,
                               alpha = 0.3)
          }
        }
      }
    }

    p
  })

  ## Cumulative Plot ----
  output$cumulativePlot <- renderPlot({
    # Check for validation errors first
    errors <- get_validation_errors()

    if(!is.null(errors)) {
      return(create_error_plot())
    }

    # Get data and check if valid
    data <- dist_data()
    if(is.null(data)) {
      return(create_error_plot("Unable to generate plot data"))
    }

    dist <- distributions[[input$dist_type]]
    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    p <- create_base_plot({
      dist <- distributions[[input$dist_type]]
      main_type <- if(dist$discrete) "CMF" else "CDF"

      if(input$show_comparison) {
        comp_dist <- distributions[[input$comp_dist]]
        comp_type <- if(comp_dist$discrete) "CMF" else "CDF"
        sprintf("%s %s vs. %s %s", input$dist_type, main_type, input$comp_dist, comp_type)
      } else {
        sprintf("%s %s", input$dist_type, main_type)
      }
    }, "cumulative")

    # Add main distribution
    if(dist$discrete) {
      p <- p + geom_step(data = data,
                         aes(x = x, y = cumulative),
                         color = colors$primary)
    } else {
      p <- p + geom_line(data = data,
                         aes(x = x, y = cumulative),
                         color = colors$primary)
    }

    # Add comparison distribution if requested
    if(input$show_comparison) {
      comp_data <- comp_dist_data()
      if(!is.null(comp_data)) {
        comp_dist <- distributions[[input$comp_dist]]
        if(comp_dist$discrete) {
          p <- p + geom_step(data = comp_data,
                             aes(x = x, y = cumulative),
                             color = colors$info,
                             alpha = 0.6)
        } else {
          p <- p + geom_line(data = comp_data,
                             aes(x = x, y = cumulative),
                             color = colors$info,
                             alpha = 0.6)
        }
      }
    }

    # Add probability region if requested
    if(input$show_prob) {
      region_data <- subset(data,
                            x >= input$lower_bound & x <= input$upper_bound)
      p <- p +
        geom_vline(xintercept = c(input$lower_bound, input$upper_bound),
                   linetype = "dashed",
                   color = colors$secondary,
                   alpha = 0.5) +
        geom_ribbon(data = region_data,
                    aes(x = x, ymin = 0, ymax = cumulative),
                    fill = colors$primary,
                    alpha = 0.3)

      if(input$show_comparison) {
        comp_data <- comp_dist_data()
        if(!is.null(comp_data)) {
          comp_region_data <- subset(comp_data,
                                     x >= input$lower_bound & x <= input$upper_bound)
          p <- p + geom_ribbon(data = comp_region_data,
                               aes(x = x, ymin = 0, ymax = cumulative),
                               fill = colors$info,
                               alpha = 0.3)
        }
      }
    }

    p
  })

  ## Probability Calculation ----
  output$probCalc <- renderUI({
    # Check for validation errors first
    errors <- get_validation_errors()

    if(!is.null(errors)) {
      return(div(
        class = "alert alert-warning",
        "Please fix parameter errors to calculate probabilities"
      ))
    }

    req(input$show_prob)
    data <- dist_data()
    if(is.null(data)) return(NULL)

    dist <- distributions[[input$dist_type]]
    params <- get_params()
    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    # Calculate probability for main distribution
    p_func <- get(paste0("p", dist$r_name))
    main_prob <- tryCatch({
      do.call(p_func, c(list(input$upper_bound), params)) -
        do.call(p_func, c(list(input$lower_bound), params))
    }, error = function(e) NULL)

    # Calculate probability for comparison distribution if present
    comp_prob <- if(input$show_comparison) {
      comp_dist <- distributions[[input$comp_dist]]
      comp_params <- get_comp_params()
      comp_p_func <- get(paste0("p", comp_dist$r_name))

      tryCatch({
        do.call(comp_p_func, c(list(input$upper_bound), comp_params)) -
          do.call(comp_p_func, c(list(input$lower_bound), comp_params))
      }, error = function(e) NULL)
    } else NULL

    tagList(
      if(!is.null(main_prob)) {
        div(
          class = "mb-2",
          h5(class = "fw-bold text-primary",
             paste("Primary Distribution:", input$dist_type)),
          p(class = "mb-1",
            paste("Probability in region [",
                  round(input$lower_bound, 3), ", ",
                  round(input$upper_bound, 3), "]: ",
                  sprintf("%.4f", main_prob)))
        )
      },
      if(!is.null(comp_prob)) {
        div(
          class = "mb-2",
          h5(class = "fw-bold text-info",
             paste("Comparison Distribution:", input$comp_dist)),
          p(class = "mb-1",
            paste("Probability in region [",
                  round(input$lower_bound, 3), ", ",
                  round(input$upper_bound, 3), "]: ",
                  sprintf("%.4f", comp_prob)))
        )
      }
    )
  })

  ## Central Limit Theorem Plot ----
  output$cltPlot <- renderPlot({
    # Check for validation errors first
    errors <- get_validation_errors()

    if(!is.null(errors)) {
      return(create_error_plot())
    }

    req(input$show_clt)
    dist <- distributions[[input$dist_type]]
    params <- get_params()

    is_dark <- isTRUE(input$dark_mode == "dark")
    colors <- if(is_dark) bootstrap_colors$dark else bootstrap_colors$light

    # Generate samples
    r_func <- get(paste0("r", dist$r_name))
    samples <- tryCatch({
      replicate(input$num_samples, {
        sample_means <- mean(do.call(r_func, c(list(input$sample_size), params)))
        if(is.finite(sample_means)) sample_means else NA
      })
    }, error = function(e) NULL)

    if(is.null(samples)) {
      return(create_error_plot("Unable to generate samples"))
    }

    # Remove NA values
    samples <- samples[!is.na(samples)]

    if(length(samples) < 2) {
      return(create_error_plot("Insufficient valid samples"))
    }

    # Calculate statistics for normal curve
    sample_mean <- mean(samples)
    sample_sd <- sd(samples)

    # Create sequence for normal curve
    x_range <- range(samples)
    x_seq <- seq(x_range[1], x_range[2], length.out = 100)
    normal_y <- dnorm(x_seq, mean = sample_mean, sd = sample_sd)

    # Create plot
    ggplot() +
      geom_histogram(aes(x = samples, y = after_stat(density)),
                     bins = min(30, input$num_samples %/% 10),
                     fill = colors$primary,
                     alpha = 0.5) +
      geom_line(aes(x = x_seq, y = normal_y),
                color = colors$info,
                linewidth = 1) +
      labs(title = "Sampling Distribution of Means",
           subtitle = sprintf(
             "Sample Size = %d, Number of Samples = %d\nMean = %.2f, SD = %.2f",
             input$sample_size, input$num_samples, sample_mean, sample_sd
           ),
           x = "Sample Mean",
           y = "Density") +
      create_plot_theme(dark_mode = is_dark)
  })

  # UI Assembly ----
  output$plots_container <- renderUI({
    plots <- list()

    # Create density and cumulative plots row
    density_cumulative <- list()

    # Always add density plot
    density_cumulative[[1]] <- card(
      card_header("Density Function"),
      card_body(
        plotOutput("densityPlot")
      )
    )

    # Add cumulative plot if selected
    if(input$show_cumulative) {
      density_cumulative[[2]] <- card(
        card_header("Cumulative Function"),
        card_body(
          plotOutput("cumulativePlot")
        )
      )
    }

    # Add first row with density and (optional) cumulative plots
    plots[[1]] <- layout_column_wrap(
      fillable = TRUE,
      !!!density_cumulative
    )

    # Add CLT plot if requested
    if(input$show_clt) {
      plots[[length(plots) + 1]] <- layout_column_wrap(
          fillable = TRUE,
          card(
            card_header(
              "Central Limit Theorem"
            ),
            card_body(
              plotOutput("cltPlot")
            )
        )
      )
    }

    # Add probability analysis if requested
    if(input$show_prob) {
      plots[[length(plots) + 1]] <- layout_column_wrap(
        card(
          card_header(
            "Probability Analysis"
          ),
          card_body(
            uiOutput("probCalc")
          )
        )
      )
    }

    # Return all plots in a vertical layout
    tagList(!!!plots)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
