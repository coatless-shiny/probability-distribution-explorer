#' Create Distribution Sidebar UI
#'
#' Creates the sidebar interface for distribution selection and parameter input
#'
#' @param id Character string module ID
#' @return A sidebar UI element containing distribution controls
#' @export
distributionSidebarUI <- function(id) {
  ns <- NS(id)  # Namespace function for module

  ## Sidebar ----
  sidebar(
    ### Main distribution selection and parameters ----
    selectInput(ns("dist_type"), "Select Distribution:",
                choices = names(distributions),
                selected = "Normal"),

    ## Dynamic parameter inputs for main distribution  ----
    uiOutput(ns("parameter_inputs")),
    uiOutput(ns("validation_message")),

    hr(),

    ## Visualization options  ----
    checkboxInput(ns("show_cumulative"), "Show Cumulative Function", TRUE),
    checkboxInput(ns("show_prob"), "Show Probability Region", FALSE),

    ## Probability region inputs  ----
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("show_prob")),
      numericInput(ns("lower_bound"), "Lower Bound:", 0),
      numericInput(ns("upper_bound"), "Upper Bound:", 1)
    ),

    hr(),

    ## Comparison distribution options  ----
    checkboxInput(ns("show_comparison"), "Show Comparison Distribution", FALSE),

    ## Comparison distribution controls in a conditional panel ----
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("show_comparison")),
      selectInput(ns("comp_dist"), "Compare with:",
                  choices = names(distributions),
                  selected = "Normal"),
      uiOutput(ns("comp_parameter_inputs")),
      uiOutput(ns("comp_validation_message"))
    ),

    hr(),

    ## CLT demonstration options  ----
    checkboxInput(ns("show_clt"), "Demonstrate Central Limit Theorem", FALSE),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("show_clt")),
      numericInput(ns("sample_size"), "Sample Size:", 1, min = 1),
      numericInput(ns("num_samples"), "Number of Samples:", 1000, min = 100),
      tags$small(
        class = "text-muted",
        "Increase the sample size to see the distribution converge to normal."
      )
    )
  )
}

#' Distribution Server Logic
#'
#' Handles the server-side logic for distribution parameter management
#'
#' @param id Character string module ID
#' @param dark_mode Reactive expression for dark mode state
#' @return List of reactive values for distribution parameters and settings
#' @export
distributionServer <- function(id, dark_mode) {
  moduleServer(id, function(input, output, session) {
    ## Render parameter inputs ----
    output$parameter_inputs <- renderUI({
      createParameterInputs(input$dist_type, session$ns)
    })

    ## Render comparison parameter inputs ----
    output$comp_parameter_inputs <- renderUI({
      req(input$show_comparison)
      createParameterInputs(input$comp_dist, session$ns, prefix = "comp_")
    })

    ## Render main validation message ----
    output$validation_message <- renderUI({
      errors <- validateInputs(input, session$ns, is_comparison = FALSE)
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
    })

    ## Render comparison validation message ----
    output$comp_validation_message <- renderUI({
      req(input$show_comparison)
      errors <- validateInputs(input, session$ns, is_comparison = TRUE)
      if (length(errors) > 0) {
        div(
          class = "alert alert-danger",
          style = "margin-top: 10px;",
          tags$b("Invalid comparison parameters:"),
          tags$ul(
            lapply(errors, function(error) {
              tags$li(error)
            })
          )
        )
      }
    })

    ## Return reactive values for use in plot module ----
    list(
      params = reactive(getValidParams(input, distributions[[input$dist_type]])),
      comp_params = reactive({
        req(input$show_comparison)
        getValidParams(input, distributions[[input$comp_dist]], "comp_")
      }),
      dist_type = reactive(input$dist_type),
      comp_dist_type = reactive(input$comp_dist),
      show_comparison = reactive(input$show_comparison),
      show_cumulative = reactive(input$show_cumulative),
      show_prob = reactive(input$show_prob),
      show_clt = reactive(input$show_clt),
      lower_bound = reactive(input$lower_bound),
      upper_bound = reactive(input$upper_bound),
      sample_size = reactive(input$sample_size),
      num_samples = reactive(input$num_samples)
    )
  })
}
