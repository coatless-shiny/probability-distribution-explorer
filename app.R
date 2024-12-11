library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

# Distribution Configuration ----
# Defines the properties and parameters for each supported probability distribution
# Structure:
# - params: Parameter names for the distribution
# - defaults: Default values for parameters
# - param_ranges: Valid ranges for each parameter
# - range: Suggested x-axis range for plotting
# - discrete: Boolean indicating if distribution is discrete
# - r_name: R function prefix for the distribution (e.g., "norm" for normal)
distributions <- list(
  "Beta" = list(
    params = c("shape1", "shape2"),
    defaults = c(2, 2),
    param_ranges = list(
      shape1 = c(0, Inf),  # Must be positive
      shape2 = c(0, Inf)   # Must be positive
    ),
    range = c(0, 1),
    discrete = FALSE,
    r_name = "beta"
  ),
  "Binomial" = list(
    params = c("size", "prob"),
    defaults = c(10, 0.5),
    param_ranges = list(
      size = c(1, Inf),    # Must be positive integer
      prob = c(0, 1)       # Must be between 0 and 1
    ),
    range = NULL,
    discrete = TRUE,
    r_name = "binom"
  ),
  "Cauchy" = list(
    params = c("location", "scale"),
    defaults = c(0, 1),
    param_ranges = list(
      location = c(-Inf, Inf),  # Any real number
      scale = c(0, Inf)         # Must be positive
    ),
    range = c(-10, 10),
    discrete = FALSE,
    r_name = "cauchy"
  ),
  "Chi-squared" = list(
    params = c("df"),
    defaults = c(1),
    param_ranges = list(
      df = c(0, Inf)  # Must be positive
    ),
    range = c(0, 20),
    discrete = FALSE,
    r_name = "chisq"
  ),
  "Exponential" = list(
    params = c("rate"),
    defaults = c(1),
    param_ranges = list(
      rate = c(0, Inf)  # Must be positive
    ),
    range = c(0, 10),
    discrete = FALSE,
    r_name = "exp"
  ),
  "F" = list(
    params = c("df1", "df2"),
    defaults = c(1, 1),
    param_ranges = list(
      df1 = c(0, Inf),  # Must be positive
      df2 = c(0, Inf)   # Must be positive
    ),
    range = c(0, 10),
    discrete = FALSE,
    r_name = "f"
  ),
  "Gamma" = list(
    params = c("shape", "rate"),
    defaults = c(1, 1),
    param_ranges = list(
      shape = c(0, Inf),  # Must be positive
      rate = c(0, Inf)    # Must be positive
    ),
    range = c(0, 20),
    discrete = FALSE,
    r_name = "gamma"
  ),
  "Geometric" = list(
    params = c("prob"),
    defaults = c(0.5),
    param_ranges = list(
      prob = c(0, 1)  # Must be between 0 and 1
    ),
    range = NULL,
    discrete = TRUE,
    r_name = "geom"
  ),
  "Hypergeometric" = list(
    params = c("m", "n", "k"),
    defaults = c(10, 7, 8),
    param_ranges = list(
      m = c(0, Inf),    # Number of white balls
      n = c(0, Inf),    # Number of black balls
      k = c(1, Inf)     # Number of draws
    ),
    range = NULL,
    discrete = TRUE,
    r_name = "hyper"
  ),
  "Logistic" = list(
    params = c("location", "scale"),
    defaults = c(0, 1),
    param_ranges = list(
      location = c(-Inf, Inf),  # Any real number
      scale = c(0, Inf)         # Must be positive
    ),
    range = c(-10, 10),
    discrete = FALSE,
    r_name = "logis"
  ),
  "Log-normal" = list(
    params = c("meanlog", "sdlog"),
    defaults = c(0, 1),
    param_ranges = list(
      meanlog = c(-Inf, Inf),  # Any real number
      sdlog = c(0, Inf)        # Must be positive
    ),
    range = c(0, 10),
    discrete = FALSE,
    r_name = "lnorm"
  ),
  "Negative Binomial" = list(
    params = c("size", "prob"),
    defaults = c(10, 0.5),
    param_ranges = list(
      size = c(1, Inf),  # Must be positive integer
      prob = c(0, 1)     # Must be between 0 and 1
    ),
    range = NULL,
    discrete = TRUE,
    r_name = "nbinom"
  ),
  "Normal" = list(
    params = c("mean", "sd"),
    defaults = c(0, 1),
    param_ranges = list(
      mean = c(-Inf, Inf),  # Any real number
      sd = c(0, Inf)        # Must be positive
    ),
    range = c(-10, 10),
    discrete = FALSE,
    r_name = "norm"
  ),
  "Poisson" = list(
    params = c("lambda"),
    defaults = c(1),
    param_ranges = list(
      lambda = c(0, Inf)  # Must be positive
    ),
    range = NULL,
    discrete = TRUE,
    r_name = "pois"
  ),
  "Student's t" = list(
    params = c("df"),
    defaults = c(1),
    param_ranges = list(
      df = c(0, Inf)  # Must be positive
    ),
    range = c(-10, 10),
    discrete = FALSE,
    r_name = "t"
  ),
  "Uniform" = list(
    params = c("min", "max"),
    defaults = c(0, 1),
    param_ranges = list(
      min = c(-Inf, Inf),  # Any real number
      max = c(-Inf, Inf)   # Any real number > min
    ),
    range = c(-10, 10),
    discrete = FALSE,
    r_name = "unif"
  ),
  "Weibull" = list(
    params = c("shape", "scale"),
    defaults = c(1, 1),
    param_ranges = list(
      shape = c(0, Inf),  # Must be positive
      scale = c(0, Inf)   # Must be positive
    ),
    range = c(0, 10),
    discrete = FALSE,
    r_name = "weibull"
  )
)

# Stanford Brand Colors ----
# Primary colors from Stanford's identity guide
stanford_colors <- list(
  cardinal_red = "#8C1515",
  cardinal_dark = "#820000",
  digital_red = "#B1040E",
  cool_grey = "#4D4F53",
  process_black = "#2E2D29",
  fog = "#DAD7CB",
  stone = "#544948",
  sky = "#0098DB",
  palo_alto = "#175E54",
  beige = "#F4F4F4",
  white =  "#FFFFFF"
)


# UI Definition ----
# Creates a sidebar layout with:
# - Distribution selection and parameter inputs
# - Visualization options
# - Comparison settings
# - CLT demonstration controls

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    foreground = stanford_colors$process_black,
    background = stanford_colors$white,
    primary    = stanford_colors$cardinal_red,
    secondary  = stanford_colors$cool_grey,
    tertiary   = stanford_colors$fog,
    success    = stanford_colors$palo_alto,
    info       = stanford_colors$sky,
    warning    = stanford_colors$digital_red,
    danger     = stanford_colors$cardinal_dark,
    light      = stanford_colors$beige,
    dark       = stanford_colors$process_black
  ),

  title = "Probability Distributions Explorer",

  ## Sidebar ----
  sidebar = sidebar(
    ### Distribution Settings ---
    selectInput("dist_type", "Select Distribution:",
                choices = names(distributions)),
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

    ### Comparison Settings ----
    checkboxInput("show_comparison", "Show Comparison Distribution", FALSE),
    conditionalPanel(
      condition = "input.show_comparison == true",
      selectInput("comp_dist", "Compare with:",
                  choices = names(distributions)),
      uiOutput("comparison_parameter_inputs")
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
  card(
    card_body(
      uiOutput("plots_container")  # Dynamic plot container
    )
  )
)


# Server logic ----
server <- function(input, output, session) {

  # Dynamic Parameter Input Generation ----
  # Creates numeric inputs for distribution parameters with appropriate ranges
  # and step sizes based on the parameter type

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


  # Parameter Value Retrieval and Validation Functions ----
  # get_params: Retrieves current parameter values
  # validate_params: Ensures parameters meet distribution constraints

  ## Get current parameter values for main distribution ----
  get_params <- reactive({
    dist <- distributions[[input$dist_type]]
    # Ensure all parameters exist before proceeding
    req(all(sapply(seq_along(dist$params), function(i) !is.null(input[[paste0("param_", i)]]))))

    params <- lapply(seq_along(dist$params), function(i) {
      value <- input[[paste0("param_", i)]]
      # Ensure numeric and valid values
      if (is.null(value) || !is.numeric(value)) {
        value <- dist$defaults[i]
      }
      value
    })
    setNames(params, dist$params)
  })


  ## Validation for main distribution parameters ----
  validate_params <- reactive({
    dist <- distributions[[input$dist_type]]
    params <- get_params()

    # Validate all parameters
    errors <- character(0)

    for(param_name in names(params)) {
      value <- params[[param_name]]
      range <- dist$param_ranges[[param_name]]

      # Check if value is numeric and finite
      if(is.null(value) || !is.numeric(value) || !is.finite(value)) {
        errors <- c(errors, sprintf("%s must be a valid number", param_name))
        next
      }

      # Check if value is within valid range
      if(value < range[1]) {
        errors <- c(errors, sprintf("%s must be greater than %s",
                                    param_name,
                                    if(range[1] == 0) "0" else range[1]))
      }
      if(value > range[2]) {
        errors <- c(errors, sprintf("%s must be less than %s",
                                    param_name,
                                    if(is.infinite(range[2])) "∞" else range[2]))
      }

      # Check for integer requirement for discrete parameters
      if(dist$discrete && param_name == "size") {
        if(abs(value - round(value)) > .Machine$double.eps^0.5) {
          errors <- c(errors, sprintf("%s must be an integer", param_name))
        }
      }
    }

    # Special case validations
    if(input$dist_type == "Uniform") {
      if(params$max <= params$min) {
        errors <- c(errors, "Maximum must be greater than minimum")
      }
    }

    if(input$dist_type == "Hypergeometric") {
      if(params$k > (params$m + params$n)) {
        errors <- c(errors, "Number of draws (k) cannot exceed total number of balls (m + n)")
      }
    }

    if(length(errors) > 0) return(errors)
    return(NULL)
  })

  # Distribution Data Generation ----
  # Creates datasets for plotting density and cumulative functions
  dist_data <- reactive({
    req(input$dist_type)

    # Check for validation errors
    errors <- validate_params()
    if(!is.null(errors)) {
      return(NULL)
    }

    dist <- distributions[[input$dist_type]]
    params <- get_params()

    # Generate x values
    if (dist$discrete) {
      max_x <- tryCatch({
        val <- switch(input$dist_type,
                      "Binomial" = params$size,
                      "Poisson" = min(qpois(0.999, params$lambda), 100),
                      "Geometric" = min(qgeom(0.999, params$prob), 50),
                      "Negative Binomial" = min(qnbinom(0.999, params$size, params$prob), 100),
                      "Hypergeometric" = min(params$k, params$m),
                      30
        )
        val
      }, error = function(e) 30)

      x <- 0:max_x
    } else {
      if (!is.null(dist$range)) {
        x <- seq(dist$range[1], dist$range[2], length.out = 200)
      } else {
        x <- seq(-10, 10, length.out = 200)
      }
    }

    # Safely calculate density and cumulative
    result <- tryCatch({
      d_func <- get(paste0("d", dist$r_name))
      p_func <- get(paste0("p", dist$r_name))

      density <- do.call(d_func, c(list(x), params))
      cumulative <- do.call(p_func, c(list(x), params))

      # Remove any NA or infinite values
      valid_idx <- !is.na(density) & !is.na(cumulative) &
        is.finite(density) & is.finite(cumulative)

      if (any(!valid_idx)) {
        x <- x[valid_idx]
        density <- density[valid_idx]
        cumulative <- cumulative[valid_idx]
      }

      if (length(x) > 0) {
        data.frame(x = x, density = density, cumulative = cumulative)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    result
  })

  # Comparison Distribution Parameter Functions ----
  # Handles the generation and validation of parameters for the comparison
  # distribution when enabled

  ## Dynamic parameter inputs for comparison distribution ----
  output$comparison_parameter_inputs <- renderUI({
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

  ## Get current parameter values for comparison distribution ----
  get_comp_params <- reactive({
    req(input$show_comparison)
    dist <- distributions[[input$comp_dist]]
    # Ensure all parameters exist before proceeding
    req(all(sapply(seq_along(dist$params), function(i) !is.null(input[[paste0("comp_param_", i)]]))))

    params <- lapply(seq_along(dist$params), function(i) {
      value <- input[[paste0("comp_param_", i)]]
      # Use default value if input is invalid
      if (is.null(value) || !is.numeric(value)) {
        value <- dist$defaults[i]
      }
      value
    })
    setNames(params, dist$params)
  })


  ## Validation for comparison distribution parameters ----
  validate_comp_params <- reactive({
    req(input$show_comparison)
    dist <- distributions[[input$comp_dist]]
    params <- get_comp_params()

    # Validate all parameters
    errors <- character(0)

    # Check each parameter against its constraints
    for(param_name in names(params)) {
      value <- params[[param_name]]
      range <- dist$param_ranges[[param_name]]

      # Check if value is numeric and finite
      if(is.null(value) || !is.numeric(value) || !is.finite(value)) {
        errors <- c(errors, sprintf("Comparison %s must be a valid number", param_name))
        next
      }

      # Check if value is within valid range
      if(value < range[1]) {
        errors <- c(errors, sprintf("Comparison %s must be greater than %s",
                                    param_name,
                                    if(range[1] == 0) "0" else range[1]))
      }
      if(value > range[2]) {
        errors <- c(errors, sprintf("Comparison %s must be less than %s",
                                    param_name,
                                    if(is.infinite(range[2])) "∞" else range[2]))
      }

      # Check for integer requirement for discrete parameters
      if(dist$discrete && param_name == "size") {
        if(abs(value - round(value)) > .Machine$double.eps^0.5) {
          errors <- c(errors, sprintf("Comparison %s must be an integer", param_name))
        }
      }
    }

    # Special case validations for comparison distribution
    if(input$comp_dist == "Uniform") {
      if(params$max <= params$min) {
        errors <- c(errors, "Comparison maximum must be greater than minimum")
      }
    }

    if(input$comp_dist == "Hypergeometric") {
      if(params$k > (params$m + params$n)) {
        errors <- c(errors, "Comparison number of draws (k) cannot exceed total number of balls (m + n)")
      }
    }

    if(length(errors) > 0) return(errors)
    return(NULL)
  })



  # Add error message UI
  output$validation_message <- renderUI({
    errors <- validate_params()
    comp_errors <- if(input$show_comparison) validate_comp_params() else NULL

    all_errors <- c(errors, comp_errors)

    if(!is.null(all_errors)) {
      div(
        class = "alert alert-danger",
        style = "margin-top: 10px;",
        tags$b("Invalid parameters:"),
        tags$ul(
          lapply(all_errors, function(error) {
            tags$li(error)
          })
        )
      )
    }
  })


  ## Generate distribution data for comparison distribution ----
  comp_dist_data <- reactive({
    req(input$show_comparison)

    # Check for validation errors
    errors <- validate_comp_params()
    if(!is.null(errors)) {
      return(NULL)
    }

    dist <- distributions[[input$comp_dist]]
    params <- get_comp_params()
    main_data <- dist_data()

    # Use main distribution's x values if available
    if (!is.null(main_data)) {
      x <- main_data$x
    } else {
      if (dist$discrete) {
        max_x <- tryCatch({
          val <- switch(input$comp_dist,
                        "Binomial" = params$size,
                        "Poisson" = min(qpois(0.999, params$lambda), 100),
                        "Geometric" = min(qgeom(0.999, params$prob), 50),
                        "Negative Binomial" = min(qnbinom(0.999, params$size, params$prob), 100),
                        "Hypergeometric" = min(params$k, params$m),
                        30
          )
          val
        }, error = function(e) 30)

        x <- 0:max_x
      } else {
        if (!is.null(dist$range)) {
          x <- seq(dist$range[1], dist$range[2], length.out = 200)
        } else {
          x <- seq(-10, 10, length.out = 200)
        }
      }
    }

    # Calculate comparison distribution values
    result <- tryCatch({
      d_func <- get(paste0("d", dist$r_name))
      p_func <- get(paste0("p", dist$r_name))

      density <- do.call(d_func, c(list(x), params))
      cumulative <- do.call(p_func, c(list(x), params))

      # Remove invalid values
      valid_idx <- !is.na(density) & !is.na(cumulative) &
        is.finite(density) & is.finite(cumulative)

      if (any(!valid_idx)) {
        x <- x[valid_idx]
        density <- density[valid_idx]
        cumulative <- cumulative[valid_idx]
      }

      if (length(x) > 0) {
        data.frame(x = x, density = density, cumulative = cumulative)
      } else {
        NULL
      }
    }, error = function(e) NULL)

    result
  })

  # Plot Generation Functions ----

  ## Density Plot ----
  # Creates density/mass and cumulative function plots
  output$densityPlot <- renderPlot({
    main_data <- dist_data()

    if(is.null(main_data)) {
      # Return an error plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Please fix parameter errors to display plot",
                 size = 6) +
        theme_void() +
        theme(panel.background = element_rect(fill = stanford_colors$fog))
    } else {
      dist <- distributions[[input$dist_type]]

      # Get comparison data if needed
      comp_data <- NULL
      comp_dist <- NULL
      if(input$show_comparison) {
        comp_errors <- validate_comp_params()
        if(is.null(comp_errors)) {
          comp_data <- comp_dist_data()
          comp_dist <- distributions[[input$comp_dist]]
        }
      }

      # Determine if we have any discrete distributions
      has_discrete <- dist$discrete || (!is.null(comp_dist) && comp_dist$discrete)

      # Determine x-axis range based on both distributions
      x_range <- NULL
      if(has_discrete) {
        if(dist$discrete) {
          x_range <- range(main_data$x)
        }
        if(!is.null(comp_dist) && comp_dist$discrete) {
          comp_range <- range(comp_data$x)
          if(is.null(x_range)) {
            x_range <- comp_range
          } else {
            x_range <- range(c(x_range, comp_range))
          }
        }
        # Add a small buffer
        x_range <- c(floor(x_range[1]), ceiling(x_range[2]))
      }

      # Initialize plot
      func_name <- if(dist$discrete) "PMF" else "PDF"
      p <- ggplot() +
        labs(title = if(input$show_comparison) {
          paste(input$dist_type, "vs", input$comp_dist)
        } else {
          paste(input$dist_type, func_name)
        },
        x = "Value",
        y = func_name) +
        theme_minimal() +
        # Update theme colors
        theme(
          plot.title = element_text(color = stanford_colors$cardinal_red),
          axis.title = element_text(color = stanford_colors$process_black),
          axis.text = element_text(color = stanford_colors$cool_grey),
          panel.grid.major = element_line(color = stanford_colors$fog),
          panel.grid.minor = element_line(color = stanford_colors$fog)
        )

      # If we have a discrete distribution, set the plot limits first
      if(has_discrete) {
        p <- p +
          scale_x_continuous(
            limits = x_range,
            breaks = seq(x_range[1], x_range[2], by = 1)
          ) +
          theme(panel.grid.minor = element_blank())
      }

      # Function to filter continuous data to match discrete range
      filter_continuous_data <- function(data, x_range) {
        if(!is.null(x_range)) {
          data |> filter(x >= x_range[1], x <= x_range[2])
        } else {
          data
        }
      }

      # Plot continuous distributions first (if any)
      if(!dist$discrete) {
        main_data <- filter_continuous_data(main_data, x_range)
        p <- p + geom_line(data = main_data,
                           aes(x = x, y = density),
                           linewidth = 1,
                           color = stanford_colors$cardinal_red)
      }

      if(!is.null(comp_dist) && !comp_dist$discrete) {
        comp_data <- filter_continuous_data(comp_data, x_range)
        p <- p + geom_line(data = comp_data,
                           aes(x = x, y = density),
                           linewidth = 1,
                           color = stanford_colors$sky,
                           alpha = 0.8)
      }

      # Then plot discrete distributions (if any)
      if(dist$discrete) {
        p <- p +
          geom_segment(data = main_data,
                       aes(x = x, xend = x, y = 0, yend = density),
                       color = stanford_colors$cardinal_red, linewidth = 1, alpha = 0.6) +
          geom_point(data = main_data,
                     aes(x = x, y = density),
                     color = stanford_colors$cardinal_red, size = 3)
      }

      if(!is.null(comp_dist) && comp_dist$discrete) {
        p <- p +
          geom_segment(data = comp_data,
                       aes(x = x, xend = x, y = 0, yend = density),
                       color = stanford_colors$sky, linewidth = 1, alpha = 0.6) +
          geom_point(data = comp_data,
                     aes(x = x, y = density),
                     color = stanford_colors$sky, size = 3)
      }

      # Add probability regions if requested
      if(input$show_prob) {
        # Add vertical lines for bounds
        p <- p +
          geom_vline(xintercept = input$lower_bound,
                     linetype = "dashed",
                     color = stanford_colors$cool_grey) +
          geom_vline(xintercept = input$upper_bound,
                     linetype = "dashed",
                     color = stanford_colors$cool_grey)

        # Add regions for main distribution
        if(dist$discrete) {
          subset_data <- main_data |>
            filter(x >= input$lower_bound & x <= input$upper_bound)
          p <- p +
            geom_segment(data = subset_data,
                         aes(x = x, xend = x, y = 0, yend = density),
                         color = stanford_colors$cardinal_dark, linewidth = 1.5, alpha = 0.8) +
            geom_point(data = subset_data,
                       aes(x = x, y = density),
                       color = stanford_colors$cardinal_dark, size = 3.5)
        } else {
          subset_data <- main_data |>
            filter(x >= input$lower_bound & x <= input$upper_bound)
          p <- p + geom_area(data = subset_data,
                             aes(x = x, y = density),
                             fill = stanford_colors$cardinal_red,
                             alpha = 0.5)
        }

        # Add regions for comparison distribution
        if(!is.null(comp_dist)) {
          if(comp_dist$discrete) {
            comp_subset_data <- comp_data |>
              filter(x >= input$lower_bound & x <= input$upper_bound)
            p <- p +
              geom_segment(data = comp_subset_data,
                           aes(x = x, xend = x, y = 0, yend = density),
                           color = stanford_colors$sky, linewidth = 1.5, alpha = 0.8) +
              geom_point(data = comp_subset_data,
                         aes(x = x, y = density),
                         color = stanford_colors$sky, size = 3.5)
          } else {
            comp_subset_data <- comp_data |>
              filter(x >= input$lower_bound & x <= input$upper_bound)
            p <- p + geom_area(data = comp_subset_data,
                               aes(x = x, y = density),
                               fill = stanford_colors$sky,
                               alpha = 0.5)
          }
        }
      }

      p
    }
  })

  ## Cumulative plot ----
  output$cumulativePlot <- renderPlot({
    main_data <- dist_data()

    if(is.null(main_data)) {
      # Return an error plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Please fix parameter errors to display plot",
                 size = 6) +
        theme_void() +
        theme(panel.background = element_rect(fill = stanford_colors$fog))
    } else {
      dist <- distributions[[input$dist_type]]
      func_name <- if(dist$discrete) "CMF" else "CDF"

      p <- ggplot() +
        labs(title = paste(input$dist_type, func_name),
             x = "Value",
             y = func_name) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = stanford_colors$cardinal_red),
          axis.title = element_text(color = stanford_colors$process_black),
          axis.text = element_text(color = stanford_colors$cool_grey),
          panel.grid.major = element_line(color = stanford_colors$fog),
          panel.grid.minor = element_line(color = stanford_colors$fog)
        )

      # Base main distribution
      if (dist$discrete) {
        p <- p + geom_step(data = main_data,
                           aes(x = x, y = cumulative),
                           linewidth = 1,
                           color = stanford_colors$cardinal_red)
      } else {
        p <- p + geom_line(data = main_data,
                           aes(x = x, y = cumulative),
                           linewidth = 1,
                           color = stanford_colors$cardinal_red)
      }

      # Add comparison distribution if requested
      if (input$show_comparison) {
        comp_errors <- validate_comp_params()
        if(is.null(comp_errors)) {
          comp_data <- comp_dist_data()
          if (!is.null(comp_data)) {
            comp_dist <- distributions[[input$comp_dist]]

            if (comp_dist$discrete) {
              p <- p + geom_step(data = comp_data,
                                 aes(x = x, y = cumulative),
                                 linewidth = 1,
                                 color = stanford_colors$sky,
                                 alpha = 0.8)
            } else {
              p <- p + geom_line(data = comp_data,
                                 aes(x = x, y = cumulative),
                                 linewidth = 1,
                                 color = stanford_colors$sky,
                                 alpha = 0.8)
            }

            p <- p + labs(title = paste(input$dist_type, "vs", input$comp_dist, func_name))
          }
        }
      }

      # Add probability region indicators if requested
      if (input$show_prob) {
        # Vertical lines for region bounds
        p <- p +
          geom_vline(xintercept = input$lower_bound,
                     linetype = "dashed",
                     color = stanford_colors$cool_grey) +
          geom_vline(xintercept = input$upper_bound,
                     linetype = "dashed",
                     color = stanford_colors$cool_grey)

        # Highlight regions for main distribution
        p <- p + geom_ribbon(data = subset(main_data,
                                           x >= input$lower_bound & x <= input$upper_bound),
                             aes(x = x, ymin = 0, ymax = cumulative),
                             fill = stanford_colors$cardinal_red,
                             alpha = 0.3)

        # Highlight regions for comparison distribution if present
        if (input$show_comparison) {
          comp_data <- comp_dist_data()
          if (!is.null(comp_data)) {
            p <- p + geom_ribbon(data = subset(comp_data,
                                               x >= input$lower_bound & x <= input$upper_bound),
                                 aes(x = x, ymin = 0, ymax = cumulative),
                                 fill = stanford_colors$sky,
                                 alpha = 0.3)
          }
        }
      }

      # Scale adjustments for discrete distributions
      if (dist$discrete) {
        p <- p +
          scale_x_continuous(breaks = main_data$x) +
          theme(panel.grid.minor = element_blank())
      }

      p
    }
  })

  output$plots_container <- renderUI({
    plots <- list()

    # Create density and cumulative plots row if both are present
    density_cumulative <- list()

    # Always add density plot
    density_cumulative[[1]] <- card(
      card_header("Density Function"),
      card_body(
        plotOutput("densityPlot")
      )
    )

    # Add cumulative plot if selected
    if (input$show_cumulative) {
      density_cumulative[[2]] <- card(
        card_header("Cumulative Function"),
        card_body(
          plotOutput("cumulativePlot")
        )
      )
    }

    # Add first row with density and (optional) cumulative plots
    plots[[1]] <- layout_column_wrap(
      width = 1/length(density_cumulative),
      !!!density_cumulative
    )

    # Add probability analysis in its own row if requested
    if (input$show_prob) {
      plots[[length(plots) + 1]] <- card(
        card_header("Probability Analysis"),
        card_body(
          uiOutput("probCalc")
        )
      )
    }

    # Add CLT plot in its own row if requested
    if (input$show_clt) {
      plots[[length(plots) + 1]] <- card(
        card_header("Central Limit Theorem"),
        card_body(
          plotOutput("cltPlot")
        )
      )
    }

    # Return all plots in a vertical layout
    tagList(!!!plots)
  })

  # Probability calculation ----
  # Computes probabilities for selected regions
  output$probCalc <- renderUI({
    req(input$show_prob)

    result <- tryCatch({
      # Calculate probability for main distribution
      main_data <- req(dist_data())
      if(is.null(main_data)) return(NULL)

      dist <- distributions[[input$dist_type]]
      p_func <- get(paste0("p", dist$r_name))
      params <- req(get_params())

      main_prob <- do.call(p_func, c(list(input$upper_bound), params)) -
        do.call(p_func, c(list(input$lower_bound), params))

      # Calculate probability for comparison distribution if present
      comp_prob <- if (input$show_comparison) {
        comp_dist <- distributions[[input$comp_dist]]
        comp_p_func <- get(paste0("p", comp_dist$r_name))
        comp_params <- req(get_comp_params())

        do.call(comp_p_func, c(list(input$upper_bound), comp_params)) -
          do.call(comp_p_func, c(list(input$lower_bound), comp_params))
      } else {
        NULL
      }

      if (is.na(main_prob) || !is.finite(main_prob)) {
        return(p("Unable to calculate probability for the given parameters"))
      }

      # Create formatted output
      tagList(
        div(
          class = "mb-3",
          h5(class = "fw-bold",
             style = sprintf("color: %s;", stanford_colors$cardinal_red),
             paste("Primary Distribution:", input$dist_type)),
          p(class = "mb-2",
            paste("Probability in region [",
                  round(input$lower_bound, 3), ", ",
                  round(input$upper_bound, 3), "]: ",
                  sprintf("%.4f", main_prob)))
        ),
        if (!is.null(comp_prob)) {
          if (is.na(comp_prob) || !is.finite(comp_prob)) {
            div(
              class = "mb-3",
              h5(class = "fw-bold",
                 style = sprintf("color: %s;", stanford_colors$sky),
                 paste("Comparison Distribution:", input$comp_dist)),
              p("Unable to calculate probability")
            )
          } else {
            div(
              class = "mb-3",
              h5(class = "fw-bold",
                 style = sprintf("color: %s;", stanford_colors$sky),
                 paste("Comparison Distribution:", input$comp_dist)),
              p(class = "mb-2",
                paste("Probability in region [",
                      round(input$lower_bound, 3), ", ",
                      round(input$upper_bound, 3), "]: ",
                      sprintf("%.4f", comp_prob)))
            )
          }
        }
      )
    }, error = function(e) {
      p(class = "text-danger",
        style = sprintf("color: %s;", stanford_colors$cardinal_red),
        "Unable to calculate probability for the given parameters")
    })

    result
  })

  # CLT Demonstration ----
  # Simulates and visualizes the Central Limit Theorem
  output$cltPlot <- renderPlot({
    req(input$show_clt)

    errors <- validate_params()
    if(!is.null(errors)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Please fix parameter errors to display plot",
                   size = 6) +
          theme_void() +
          theme(panel.background = element_rect(fill = stanford_colors$fog))
      )
    }

    params <- get_params()

    tryCatch({
      # Get the random generation function for the selected distribution
      dist <- distributions[[input$dist_type]]
      r_func <- get(paste0("r", dist$r_name))

      # Generate samples
      samples <- replicate(input$num_samples, {
        sample_data <- do.call(r_func, c(list(input$sample_size), params))
        mean(sample_data)
      })

      # Check for valid results
      if (any(is.na(samples)) || any(!is.finite(samples))) {
        return(NULL)
      }

      # Plot histogram of sample means
      data.frame(sample_means = samples) |>
        ggplot(aes(x = sample_means)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = stanford_colors$cardinal_red, alpha = 0.5) +
        geom_density(color = stanford_colors$sky) +
        labs(title = "Sampling Distribution of Means",
             subtitle = paste("Sample Size =", input$sample_size,
                              ", Number of Samples =", input$num_samples),
             x = "Sample Mean",
             y = "Density") +
        theme_minimal() +
        theme(
          plot.title = element_text(color = stanford_colors$cardinal_red),  # Update title color
          plot.subtitle = element_text(color = stanford_colors$cool_grey),  # Update subtitle color
          axis.title = element_text(color = stanford_colors$process_black),         # Update axis title color
          axis.text = element_text(color = stanford_colors$cool_grey),      # Update axis text color
          panel.grid.major = element_line(color = stanford_colors$fog),     # Update major grid lines
          panel.grid.minor = element_line(color = stanford_colors$fog)      # Update minor grid lines
        )
    }, error = function(e) NULL)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
