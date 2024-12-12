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

