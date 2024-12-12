#' Probability Distributions Explorer Application
#'
#' A Shiny application for interactive exploration of probability distributions.
#' Provides visualization and analysis tools for various probability distributions.
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import bslib

# Library Imports ----
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

# Source Dependencies  ----
source("R/config/colors.R")                # Color schemes and themes
source("R/config/distributions.R")         # Distribution configurations
source("R/modules/distribution_module.R")  # Distribution UI and logic
source("R/modules/plot_module.R")          # Plotting UI and logic
source("R/utils/helpers.R")                # Helper functions

# UI Definition ----
ui <- page_navbar(
  theme = create_theme(dark_mode = FALSE),
  title = "Probability Distributions Explorer",

  # Dark mode toggle
  nav_item(input_dark_mode(id = "dark_mode")),

  # Sidebar with distribution controls
  sidebar = distributionSidebarUI("dist"),

  # Main panel for plots
  nav_panel("", plotContainerUI("plots"))
)

# Server Logic ----
server <- function(input, output, session) {
  # Theme management
  dark_mode <- reactive({
    isTRUE(input$dark_mode == "dark")
  })

  observe({
    session$setCurrentTheme(create_theme(dark_mode()))
  })

  ## Initialize modules and handle data flow -----
  dist_data <- distributionServer("dist", dark_mode)
  plotServer("plots", dist_data, dark_mode)

}

# Run the application ----
shinyApp(ui = ui, server = server)
