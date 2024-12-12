# Parameter creation ----

#' Create Parameter Inputs
#'
#' Dynamically generates input UI elements for distribution parameters
#'
#' @param dist_type Character string specifying the distribution
#' @param ns Namespace function for the module
#' @param prefix Optional prefix for parameter names (default: "")
#' @return A tagList of input UI elements
#' @examples
#' createParameterInputs("Normal", NS("dist"))
createParameterInputs <- function(dist_type, ns, prefix = "") {
  dist <- distributions[[dist_type]]
  param_inputs <- lapply(seq_along(dist$params), function(i) {
    createParameterInput(
      dist$params[i],
      dist$defaults[i],
      dist$param_ranges[[dist$params[i]]],
      dist$discrete,
      prefix,
      ns
    )
  })
  do.call(tagList, param_inputs)
}

#' Create Single Parameter Input
#'
#' Creates a single parameter input UI element with validation
#'
#' @param param_name Character string name of the parameter
#' @param default_value Numeric default value
#' @param param_range Numeric vector of length 2 specifying valid range
#' @param is_discrete Logical indicating if parameter should be integer
#' @param prefix Optional prefix for parameter name (default: "")
#' @param ns Namespace function
#' @return A div containing the input UI element and help text
createParameterInput <- function(param_name, default_value, param_range, is_discrete, prefix = "", ns) {
  div(
    numericInput(
      ns(paste0(prefix, "param_", param_name)),
      paste0(
        if(nchar(prefix) > 0) "Comparison " else "",
        param_name,
        if(is_discrete && param_name == "size") " (integer)" else ""
      ),
      value = default_value,
      min = if(is.infinite(param_range[1])) -1000 else param_range[1],
      max = if(is.infinite(param_range[2])) 1000 else param_range[2],
      step = if(is_discrete && param_name == "size") 1 else 0.1
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
}

# Validation ----

#' Validate Distribution Inputs
#'
#' Validates all input parameters for a distribution
#'
#' @param input Shiny input object
#' @param ns Namespace function
#' @return Character vector of error messages (empty if all valid)
validateInputs <- function(input, ns, is_comparison = FALSE) {
  errors <- character(0)

  if (is_comparison) {
    # Validate comparison distribution parameters
    dist <- distributions[[input$comp_dist]]
    params <- getValidParams(input, dist, "comp_")
    param_prefix <- "Comparison "
  } else {
    # Validate main distribution parameters
    dist <- distributions[[input$dist_type]]
    params <- getValidParams(input, dist)
    param_prefix <- ""
  }

  # Check each parameter
  for(param_name in names(params)) {
    value <- params[[param_name]]
    range <- dist$param_ranges[[param_name]]

    # Numeric check
    if(is.null(value) || !is.numeric(value) || !is.finite(value)) {
      errors <- c(errors, sprintf("%s%s must be a valid number", param_prefix, param_name))
      next
    }

    # Range check
    if(value < range[1]) {
      errors <- c(errors, sprintf("%s%s must be greater than %s",
                                  param_prefix, param_name,
                                  if(range[1] == 0) "0" else range[1]))
    }
    if(value > range[2]) {
      errors <- c(errors, sprintf("%s%s must be less than %s",
                                  param_prefix, param_name,
                                  if(is.infinite(range[2])) "∞" else range[2]))
    }

    # Integer check for discrete parameters
    if(dist$discrete && param_name == "size") {
      if(abs(value - round(value)) > .Machine$double.eps^0.5) {
        errors <- c(errors, sprintf("%s%s must be an integer", param_prefix, param_name))
      }
    }
  }

  # Special case validations
  if(!is_comparison) {
    if(input$dist_type == "Uniform" && params$max <= params$min) {
      errors <- c(errors, "maximum must be greater than minimum")
    }

    if(input$dist_type == "Hypergeometric" && params$k > (params$m + params$n)) {
      errors <- c(errors, "number of draws (k) cannot exceed total number of balls (m + n)")
    }
  } else {
    if(input$comp_dist == "Uniform" && params$max <= params$min) {
      errors <- c(errors, "comparison maximum must be greater than minimum")
    }

    if(input$comp_dist == "Hypergeometric" && params$k > (params$m + params$n)) {
      errors <- c(errors, "comparison number of draws (k) cannot exceed total number of balls (m + n)")
    }
  }

  errors
}

#' Get Valid Parameters
#'
#' Retrieves and validates parameters for a distribution
#'
#' @param input Shiny input object
#' @param dist Distribution configuration list
#' @param prefix Optional prefix for parameter names (default: "")
#' @return Named list of validated parameters
getValidParams <- function(input, dist, prefix = "") {
  params <- setNames(
    lapply(dist$params, function(param) {
      val <- input[[paste0(prefix, "param_", param)]]
      if (is.null(val) || !is.numeric(val) || !is.finite(val)) {
        idx <- which(dist$params == param)
        dist$defaults[idx]
      } else {
        val
      }
    }),
    dist$params
  )

  if (dist$discrete && "size" %in% names(params)) {
    params$size <- floor(params$size)
  }
  params
}

# Distribution Data Generation ----

#' Generate Distribution Data
#'
#' Generates data points for plotting a distribution
#'
#' @param dist_type Character string specifying the distribution
#' @param params Named list of distribution parameters
#' @return Data frame with x, density, and cumulative columns
generateDistData <- function(dist_type, params) {
  tryCatch({
    dist <- distributions[[dist_type]]
    if (is.null(dist)) return(NULL)

    # Generate x values
    x <- if (dist$discrete) {
      max_x <- switch(dist_type,
                      "Binomial" = params$size,
                      "Poisson" = min(qpois(0.999, params$lambda), 100),
                      "Geometric" = min(qgeom(0.999, params$prob), 50),
                      "Negative Binomial" = min(qnbinom(0.999, params$size, params$prob), 100),
                      "Hypergeometric" = min(params$k, params$m),
                      30)
      seq(0, max_x)
    } else {
      range <- dist$range %||% c(-10, 10)
      seq(range[1], range[2], length.out = 200)
    }

    # Calculate density and cumulative
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
}

# Plot Creation Functions ----

#' Create Density Plot
#'
#' Creates a density or PMF plot for the selected distribution
#'
#' @param dist_data Reactive list of distribution parameters and settings
#' @param dark_mode Logical indicating dark mode state
#' @return ggplot object of density/PMF plot
createDensityPlot <- function(dist_data, dark_mode) {
  # Check for validation errors
  if (is.null(dist_data$params())) {
    return(createErrorPlot(dark_mode = dark_mode))
  }

  # Get plot data
  data <- generateDistData(dist_data$dist_type(), dist_data$params())
  if (is.null(data)) {
    return(createErrorPlot("Unable to generate plot data", dark_mode = dark_mode))
  }

  # Create base plot
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light
  dist <- distributions[[dist_data$dist_type()]]

  # Create title
  plot_title <- if(dist_data$show_comparison()) {
    comp_dist <- distributions[[dist_data$comp_dist_type()]]
    sprintf(
      "%s %s vs. %s %s",
      dist_data$dist_type(),
      if(dist$discrete) "PMF" else "PDF",
      dist_data$comp_dist_type(),
      if(comp_dist$discrete) "PMF" else "PDF"
    )
  } else {
    sprintf(
      "%s %s",
      dist_data$dist_type(),
      if(dist$discrete) "PMF" else "PDF"
    )
  }

  p <- ggplot() +
    labs(
      title = plot_title,
      x = "Value",
      y = "Density"
    ) +
    create_plot_theme(dark_mode)

  # Add main distribution
  if (dist$discrete) {
    p <- p +
      geom_segment(
        data = data,
        aes(x = x, xend = x, y = 0, yend = density),
        color = colors$primary
      ) +
      geom_point(
        data = data,
        aes(x = x, y = density),
        color = colors$primary
      )
  } else {
    p <- p +
      geom_line(
        data = data,
        aes(x = x, y = density),
        color = colors$primary
      )
  }

  # Add comparison distribution if requested
  if (dist_data$show_comparison()) {
    comp_data <- generateDistData(
      dist_data$comp_dist_type(),
      dist_data$comp_params()
    )

    if (!is.null(comp_data)) {
      comp_dist <- distributions[[dist_data$comp_dist_type()]]

      if (comp_dist$discrete) {
        p <- p +
          geom_segment(
            data = comp_data,
            aes(x = x, xend = x, y = 0, yend = density),
            color = colors$info,
            alpha = 0.6
          ) +
          geom_point(
            data = comp_data,
            aes(x = x, y = density),
            color = colors$info
          )
      } else {
        p <- p +
          geom_line(
            data = comp_data,
            aes(x = x, y = density),
            color = colors$info,
            alpha = 0.6
          )
      }
    }
  }

  # Add probability region if requested
  if (dist_data$show_prob()) {
    lower <- dist_data$lower_bound()
    upper <- dist_data$upper_bound()

    # Add vertical lines for bounds
    p <- p +
      geom_vline(
        xintercept = c(lower, upper),
        linetype = "dashed",
        color = colors$secondary,
        alpha = 0.5
      )

    # Add shading for main distribution
    subset_data <- subset(data, x >= lower & x <= upper)
    if (dist$discrete) {
      p <- p +
        geom_segment(
          data = subset_data,
          aes(x = x, xend = x, y = 0, yend = density),
          color = colors$primary,
          alpha = 0.8,
          linewidth = 2
        )
    } else {
      p <- p +
        geom_area(
          data = subset_data,
          aes(x = x, y = density),
          fill = colors$primary,
          alpha = 0.3
        )
    }

    # Add shading for comparison distribution if present
    if (dist_data$show_comparison()) {
      comp_data <- generateDistData(
        dist_data$comp_dist_type(),
        dist_data$comp_params()
      )

      if (!is.null(comp_data)) {
        comp_dist <- distributions[[dist_data$comp_dist_type()]]
        comp_subset_data <- subset(comp_data, x >= lower & x <= upper)

        if (comp_dist$discrete) {
          p <- p +
            geom_segment(
              data = comp_subset_data,
              aes(x = x, xend = x, y = 0, yend = density),
              color = colors$info,
              alpha = 0.8,
              linewidth = 2
            )
        } else {
          p <- p +
            geom_area(
              data = comp_subset_data,
              aes(x = x, y = density),
              fill = colors$info,
              alpha = 0.3
            )
        }
      }
    }
  }

  p
}

#' Create Cumulative Plot
#'
#' Creates a cumulative distribution function plot
#'
#' @param dist_data Reactive list of distribution parameters and settings
#' @param dark_mode Logical indicating dark mode state
#' @return ggplot object of CDF/CMF plot
createCumulativePlot <- function(dist_data, dark_mode) {
  # Check for validation errors
  if (is.null(dist_data$params())) {
    return(createErrorPlot(dark_mode = dark_mode))
  }

  # Get plot data
  data <- generateDistData(dist_data$dist_type(), dist_data$params())
  if (is.null(data)) {
    return(createErrorPlot("Unable to generate plot data", dark_mode = dark_mode))
  }

  # Create base plot
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light
  dist <- distributions[[dist_data$dist_type()]]

  # Title generation
  plot_title <- if(dist_data$show_comparison()) {
    comp_dist <- distributions[[dist_data$comp_dist_type()]]
    sprintf(
      "%s %s vs. %s %s",
      dist_data$dist_type(),
      if(dist$discrete) "CMF" else "CDF",
      dist_data$comp_dist_type(),
      if(comp_dist$discrete) "CMF" else "CDF"
    )
  } else {
    sprintf(
      "%s %s",
      dist_data$dist_type(),
      if(dist$discrete) "CMF" else "CDF"
    )
  }

  p <- ggplot() +
    labs(
      title = plot_title,
      x = "Value",
      y = "Cumulative Probability"
    ) +
    create_plot_theme(dark_mode)

  # Add main distribution
  if (dist$discrete) {
    p <- p +
      geom_step(
        data = data,
        aes(x = x, y = cumulative),
        color = colors$primary
      )
  } else {
    p <- p +
      geom_line(
        data = data,
        aes(x = x, y = cumulative),
        color = colors$primary
      )
  }

  # Add comparison distribution if requested
  if (dist_data$show_comparison()) {
    comp_data <- generateDistData(
      dist_data$comp_dist_type(),
      dist_data$comp_params()
    )

    if (!is.null(comp_data)) {
      comp_dist <- distributions[[dist_data$comp_dist_type()]]

      if (comp_dist$discrete) {
        p <- p +
          geom_step(
            data = comp_data,
            aes(x = x, y = cumulative),
            color = colors$info,
            alpha = 0.6
          )
      } else {
        p <- p +
          geom_line(
            data = comp_data,
            aes(x = x, y = cumulative),
            color = colors$info,
            alpha = 0.6
          )
      }
    }
  }

  # Add probability region if requested
  if (dist_data$show_prob()) {
    lower <- dist_data$lower_bound()
    upper <- dist_data$upper_bound()

    # Add vertical lines for bounds
    p <- p +
      geom_vline(
        xintercept = c(lower, upper),
        linetype = "dashed",
        color = colors$secondary,
        alpha = 0.5
      )

    # Add shading for probability region
    region_data <- subset(data, x >= lower & x <= upper)
    p <- p +
      geom_ribbon(
        data = region_data,
        aes(x = x, ymin = 0, ymax = cumulative),
        fill = colors$primary,
        alpha = 0.3
      )

    # Add shading for comparison distribution if present
    if (dist_data$show_comparison()) {
      comp_data <- generateDistData(
        dist_data$comp_dist_type(),
        dist_data$comp_params()
      )

      if (!is.null(comp_data)) {
        comp_region_data <- subset(comp_data, x >= lower & x <= upper)
        p <- p +
          geom_ribbon(
            data = comp_region_data,
            aes(x = x, ymin = 0, ymax = cumulative),
            fill = colors$info,
            alpha = 0.3
          )
      }
    }
  }

  p
}

#' Create CLT Plot
#'
#' Creates a Central Limit Theorem demonstration plot
#'
#' @param dist_data Reactive list of distribution parameters and settings
#' @param dark_mode Logical indicating dark mode state
#' @return ggplot object of CLT demonstration
createCLTPlot <- function(dist_data, dark_mode) {
  req(dist_data$show_clt())

  # Generate samples
  dist <- distributions[[dist_data$dist_type()]]
  params <- dist_data$params()

  r_func <- get(paste0("r", dist$r_name))
  samples <- tryCatch({
    replicate(dist_data$num_samples(), {
      sample_means <- mean(do.call(r_func, c(list(dist_data$sample_size()), params)))
      if(is.finite(sample_means)) sample_means else NA
    })
  }, error = function(e) NULL)

  if (is.null(samples)) {
    return(createErrorPlot("Unable to generate samples"))
  }

  # Remove NA values and create plot
  samples <- samples[!is.na(samples)]
  if (length(samples) < 2) {
    return(createErrorPlot("Insufficient valid samples"))
  }

  # Create plot
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light

  sample_mean <- mean(samples)
  sample_sd <- sd(samples)
  x_range <- range(samples)
  x_seq <- seq(x_range[1], x_range[2], length.out = 100)
  normal_y <- dnorm(x_seq, mean = sample_mean, sd = sample_sd)

  ggplot() +
    geom_histogram(
      aes(x = samples, y = after_stat(density)),
      bins = min(30, dist_data$num_samples() %/% 10),
      fill = colors$primary,
      alpha = 0.5
    ) +
    geom_line(
      aes(x = x_seq, y = normal_y),
      color = colors$info,
      linewidth = 1
    ) +
    labs(
      title = "Sampling Distribution of Means",
      subtitle = sprintf(
        "Sample Size = %d, Number of Samples = %d\nMean = %.2f, SD = %.2f",
        dist_data$sample_size(),
        dist_data$num_samples(),
        sample_mean,
        sample_sd
      ),
      x = "Sample Mean",
      y = "Density"
    ) +
    create_plot_theme(dark_mode)
}

#' Create Probability Analysis
#'
#' Creates probability analysis UI elements
#'
#' @param dist_data Reactive list of distribution parameters and settings
#' @param dark_mode Logical indicating dark mode state
#' @return UI elements showing probability calculations
createProbabilityAnalysis <- function(dist_data, dark_mode) {
  req(dist_data$show_prob())

  # Calculate probability for main distribution
  dist <- distributions[[dist_data$dist_type()]]
  params <- dist_data$params()
  p_func <- get(paste0("p", dist$r_name))

  main_prob <- tryCatch({
    do.call(p_func, c(list(dist_data$upper_bound()), params)) -
      do.call(p_func, c(list(dist_data$lower_bound()), params))
  }, error = function(e) NULL)

  # Calculate probability for comparison distribution if present
  comp_prob <- if (dist_data$show_comparison()) {
    comp_dist <- distributions[[dist_data$comp_dist_type()]]
    comp_params <- dist_data$comp_params()
    comp_p_func <- get(paste0("p", comp_dist$r_name))

    tryCatch({
      do.call(comp_p_func, c(list(dist_data$upper_bound()), comp_params)) -
        do.call(comp_p_func, c(list(dist_data$lower_bound()), comp_params))
    }, error = function(e) NULL)
  } else NULL

  # Create UI elements
  tagList(
    if (!is.null(main_prob)) {
      div(
        h5(
          class = "fw-bold text-primary",
          paste("Primary Distribution:", dist_data$dist_type())
        ),
        p(
          class = "mb-1",
          paste(
            "Probability in region [",
            round(dist_data$lower_bound(), 3), ", ",
            round(dist_data$upper_bound(), 3), "]: ",
            sprintf("%.4f", main_prob)
          )
        )
      )
    },
    if (!is.null(comp_prob)) {
      div(
        class = "mb-2",
        h5(
          class = "fw-bold text-info",
          paste("Comparison Distribution:", dist_data$comp_dist_type())
        ),
        p(
          class = "mb-1",
          paste(
            "Probability in region [",
            round(dist_data$lower_bound(), 3), ", ",
            round(dist_data$upper_bound(), 3), "]: ",
            sprintf("%.4f", comp_prob)
          )
        )
      )
    }
  )
}

# Error Handling Functions ----

#' Create Error Plot
#'
#' Creates an error message plot when distribution plotting fails
#'
#' @param message Character string error message
#' @param dark_mode Logical indicating dark mode state
#' @return ggplot object displaying error message
createErrorPlot <- function(message = "Please fix parameter errors to display plot", dark_mode = FALSE) {
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light

  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = message,
      size = 6,
      color = colors$fg
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(
        fill = colors$bg,
        color = colors$border
      )
    )
}

# Plot Container Creation ----

#' Create Plot Container
#'
#' Creates the container layout for all plots and analysis elements
#'
#' @param dist_data Reactive list of distribution parameters and settings
#' @param ns Namespace function
#' @return tagList of UI elements arranged in layout
createPlotContainer <- function(dist_data, ns) {
  plots <- list()

  # Create density and cumulative plots row
  density_cumulative <- list()

  # Always add density plot
  density_cumulative[[1]] <- card(
    card_header("Density Function"),
    card_body(
      plotOutput(ns("densityPlot"))
    )
  )

  # Add cumulative plot if selected
  if (isTRUE(dist_data$show_cumulative())) {  # Add isTRUE() check
    density_cumulative[[2]] <- card(
      card_header("Cumulative Function"),
      card_body(
        plotOutput(ns("cumulativePlot"))
      )
    )
  }

  # Add first row with density and (optional) cumulative plots
  plots[[1]] <- layout_column_wrap(
    width = "250px",  # Add width to ensure proper layout
    fillable = TRUE,
    !!!density_cumulative
  )

  # Add CLT plot if requested
  if (isTRUE(dist_data$show_clt())) {  # Add isTRUE() check
    plots[[length(plots) + 1]] <- layout_column_wrap(
      fillable = TRUE,
      card(
        card_header("Central Limit Theorem"),
        card_body(
          plotOutput(ns("cltPlot"))
        )
      )
    )
  }

  # Add probability analysis if requested
  if (isTRUE(dist_data$show_prob())) {  # Add isTRUE() check
    plots[[length(plots) + 1]] <- layout_column_wrap(
      card(
        card_header("Probability Analysis"),
        card_body(
          uiOutput(ns("probCalc"))
        )
      )
    )
  }

  # Return all plots
  tagList(!!!plots)
}

#' Create Validation Message
#'
#' Creates validation error message UI elements
#'
#' @param input Shiny input object
#' @param ns Namespace function
#' @return UI element containing validation messages or NULL if no errors
createValidationMessage <- function(input, ns) {
  # Get validation errors
  errors <- validateInputs(input, ns)

  # If there are errors, create an error message div
  if (length(errors) > 0) {
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
}

