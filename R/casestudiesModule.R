#' @title Create a casestudies selector UI module
#'
#' @description Contains php1, hypopara, hypoD3
#'
#' @param id module id.
#'
#' @export
casestudiesSelectUi <- function(id) {

  ns <- NS(id)

  casestudies <- list(
    labels = c("Baroreceptor Stimulation", "Haemorrhage", "High/Low Salt Diet"),
    ids = c("run_php1", "run_hypopara", "run_hypoD3")
  )

  tagList(
    h6("Case Studies"),
    lapply(seq_along(casestudies$ids), function(i) {
      casestudiesCheckBox(inputId = ns(casestudies$ids[[i]]), label = casestudies$labels[[i]])
    }),
    # reset button
    shinyWidgets::actionBttn(
      inputId = ns("cure"),
      label = "Cure Patient",
      style = "fill",
      icon = icon("medkit"),
      color = "success",
      block = TRUE,
    )
  )
}


#' @title Create a checkbox for \link{casestudiesSelectUi}
#'
#' @description Create a \link[shinyWidgets]{prettyCheckbox}.
#'
#' @param inputId Checkbox Input id.
#' @param label Checkbox label.
#'
casestudiesCheckBox <- function(inputId, label) {
  shinyWidgets::prettyCheckbox(
    inputId = inputId,
    label = label,
    value = FALSE,
    animation = "pulse",
    thick = TRUE,
    status = "primary"
  )
}


#' @title Create a casestudies selector server logic
#'
#' @description Only returns inputs associated with php1, hypopara, hypoD3
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#'
#' @export
casestudiesSelect <- function(input, output, session) {

  #  Prevent user from selecting multiple boxes using shinyjs functions
  observeEvent(c(input$run_php1, input$run_hypopara, input$run_hypoD3), {

    casestudies <- list(
      run_php1 = input$run_php1,
      run_hypopara = input$run_hypopara,
      run_hypoD3 = input$run_hypoD3
    )
    # extract the list of simulations and the current one as well as its index
    # to properly select boxes to enable/disable
    current_simulation <- unlist(
      lapply(seq_along(casestudies), FUN = function(i) {
        if (casestudies[[i]]) names(casestudies)[[i]]
      })
    )
    index <- which(names(casestudies) == current_simulation)

    # if one simulation run, disable all boxes that are not related to that one
    if (!is.null(current_simulation)) {
      lapply(seq_along(casestudies[-index]), FUN = function(i) {
        shinyjs::disable(id = names(casestudies[-index])[[i]])
      })
    } else {# if no simulation runs, all boxes are available
      lapply(seq_along(casestudies), FUN = function(i) {
        shinyjs::enable(id = names(casestudies)[[i]])
      })
    }
  })


  # reset rat state
  observeEvent(input$cure, {
    ids <- c("run_php1", "run_hypopara", "run_hypoD3")
    purrr::map(ids, shinyjs::reset)
  })


  return(
    list(
      php1 = reactive(input$run_php1),
      hypopara = reactive(input$run_hypopara),
      hypoD3 = reactive(input$run_hypoD3)
    )
  )
}
