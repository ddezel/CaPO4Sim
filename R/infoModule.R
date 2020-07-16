#' @title Info UI module
#'
#' @description Create modals, alerts, ...
#'
#' @param id module id.
#'
#' @importFrom shiny tagList
#' @importFrom shinyWidgets prettySwitch
#'
#' @export
infosUi <- function(id) {

  ns <- NS(id)

  infos <- list(
    ids = c("notif2_switch", "modal_switch"),
    labels = c("Notifications?", "Descriptions?")
  )

  tagList(
    # Enable/Disable informations
    lapply(seq_along(infos$ids), function(i) {
      infoSwitch(inputId = ns(infos$ids[[i]]), label = infos$labels[[i]])
    })
  )
}



#' @title Create a switch input for \link{infosUi}
#'
#' @description Create a \link[shinyWidgets]{prettySwitch}.
#'
#' @param inputId Checkbox Input id.
#' @param label Checkbox label.
#'
infoSwitch <- function(inputId, label) {
  shinyWidgets::prettySwitch(
    inputId = inputId,
    label = label,
    value = TRUE,
    status = "success",
    slim = TRUE,
    bigger = TRUE
  )
}


#' @title Info server module
#'
#' @description Create modals, alerts, ...
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#' @param casestudies Shiny input casestudies selector. See \link{casestudiesSelect}.
#' @param animation_counter Give the current temporal state of the animation. See \link{networkCaPO4}.
#' @param regulations Shiny input to toggle hormone display. See \link{networkOptions}.
#'
#' @importFrom shiny observeEvent observe showModal
#'
#' @export
infos <- function(input, output, session, casestudies, animation_counter, regulations) {

  #-------------------------------------------------------------------------
  #  Educational content: modals, network and graph notifications,
  #     glossary
  #-------------------------------------------------------------------------

  # Modal for primary hyperparathyroidism, hypopara, ...
  # gives the user some extra information
  observeEvent(c(casestudies$php1(), casestudies$hypopara(), casestudies$hypoD3()), {

    # show the modal related to the current running simulation
    current_sim <- extract_running_sim(casestudies)
    req(current_sim)
    if (input$modal_switch) {
      showModal(eval(parse(text = paste("modal", current_sim, sep = "_"))))
    }
  })


  # Notification events for PHP1, hypoD3 and hypopara in the CaPO4 network
  # as well as the graph
  observeEvent(
    c(casestudies$php1(), casestudies$hypopara(), casestudies$hypoD3(),
      animation_counter(), input$notif2_switch), {

        current_simulation <- extract_running_sim(casestudies)
        req(current_simulation)

        if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {

          generate_notification(
            counter = animation_counter(),
            simulation = current_simulation,
            allowed = input$notif2_switch
          )
          # make it draggable
          shinyjqui::jqui_draggable(selector = "#shiny-notification-notifid")
        }
      }, priority = 10)


  # indicates the user to enable regulations when he launches case studies
  # if they are not already enabled
  observeEvent(c(casestudies$php1(), casestudies$hypopara(), casestudies$hypoD3()), {

      current_simulation <- extract_running_sim(casestudies)
      input_current_simulation <- paste0("casestudies$", current_simulation, "()")
      if (!is_empty(current_simulation)) {
        if (eval(parse(text = input_current_simulation)) & !regulations()) {
          shinyWidgets::sendSweetAlert(
            session = session,
            type = "error",
            title = NULL,
            text = HTML(
              paste(
                "Before going deeper in the case study,
                please enable hormonal regulations
                in the option part and select both Ca and Pi",
                icon("sliders")
              )
            ),
            html = TRUE
          )
        }
      }
    })

  # indicate when a user has finished the current activity
  observe({
    if (animation_counter() == 6) {
      current_simulation <- extract_running_sim(casestudies)
      shinyWidgets::sendSweetAlert(
        session = session,
        type = "success",
        title = NULL,
        text = HTML(
          paste(
            "You just finished", current_simulation, "activity.
            You are free to replay the animation or choose another
            activity. Click on the <b>next</b> button again to reset the current
            activity"
          )
        ),
        html = TRUE
      )
    }
  })

}
