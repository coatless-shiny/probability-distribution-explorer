
createParameterInput <- function(param_name, default_value, param_range, is_discrete, prefix = "", ns) {
  div(
    numericInput(
      ns(paste0(prefix, "param_", param_name)),
      paste0(if(nchar(prefix) > 0) "Comparison " else "", param_name,
             if(is_discrete && param_name == "size") " (integer)" else ""),
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

  # Additional validation
  if (dist$discrete && "size" %in% names(params)) {
    params$size <- floor(params$size)
  }
  params
}

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
