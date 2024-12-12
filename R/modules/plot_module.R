#' Create Plot Container UI
#'
#' Creates the UI container for all plots
#'
#' @param id Character string module ID
#' @return A UI element containing plot outputs
#' @export
plotContainerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("plots_container"))
}

#' Plot Server Logic
#'
#' Handles the server-side logic for plot generation and updates
#'
#' @param id Character string module ID
#' @param dist_data Reactive list of distribution parameters and settings
#' @param dark_mode Reactive expression for dark mode state
#' @export
plotServer <- function(id, dist_data, dark_mode) {
  moduleServer(id, function(input, output, session) {
    ## Density plot ----
    output$densityPlot <- renderPlot({
      is_dark <- isTRUE(dark_mode())
      createDensityPlot(dist_data, is_dark)
    })

    ## Cumulative plot ----
    output$cumulativePlot <- renderPlot({
      is_dark <- isTRUE(dark_mode())
      createCumulativePlot(dist_data, is_dark)
    })

    ## CLT plot ----
    output$cltPlot <- renderPlot({
      is_dark <- isTRUE(dark_mode())
      createCLTPlot(dist_data, is_dark)
    })

    ## Probability analysis ----
    output$probCalc <- renderUI({
      is_dark <- isTRUE(dark_mode())
      createProbabilityAnalysis(dist_data, is_dark)
    })

    ## Plot container assembly ----
    output$plots_container <- renderUI({
      createPlotContainer(dist_data, session$ns)
    })
  })
}

