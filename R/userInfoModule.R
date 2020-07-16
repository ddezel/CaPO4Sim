#' @title CaPO4 user info UI module
#'
#' @description Create a CaPO4 user info card
#'
#' @param id module id.
#'
#' @export
userInfoUi <- function(id) {

  ns <- NS(id)

  tagAppendAttributes(
    shinydashboardPlus::userOutput(ns("user")),
    style = "margin-left: 100px; border:none;"
  )
}




#' @title CaPO4 user info server module
#'
#' @description Create a CaPO4 user info card
#'
#' @param input Shiny inputs
#' @param output Shiny Outputs
#' @param session Session object.
#' @param casestudies Shiny input casestudies selector. See \link{casestudiesSelect}.
#' @param sliderCasestudies Shiny input casestudy severity selector. See \link{plotBox}.
#' @param help Help input.
#'
#' @export
userInfo <- function(input, output, session, casestudies, sliderCasestudies, help) {

  # generate a patient profile
  output$user <- shinydashboardPlus::renderUser({

    ns <- session$ns

    req(!is.null(casestudies$php1()) | !is.null(casestudies$hypopara()) | !is.null(casestudies$hypoD3()))

    shinydashboardPlus::dashboardUser(
      name = "Vital Parameters",
      src = if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {
        generate_userFields(casestudies, sliderCasestudies)$image
      } else {
        "images_patient_info/happy.png"
      },
      title = if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {
        generate_userFields(casestudies, sliderCasestudies)$description
      } else {
        "healthy"
      },
      subtitle = if (casestudies$php1()) {
        "Rat has primary-hyperparathyroidism"
      } else if (casestudies$hypopara()) {
        "Rat suffers from hypoparathyroidism"
      } else if (casestudies$hypoD3()) {
        "Rat has vitamin D3 defficiency"
      } else {
        "nothing to declare!"
      },
      if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {
        shinydashboardPlus::dashboardUserItem(width = 6, generate_userFields(casestudies, sliderCasestudies)$stat1)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          HTML(paste("<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>Ca<sup>2+</sup></b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "1.21 mM","<br>",
                     "(1.1-1.4 mM)"))
        )
      },
      if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {
        shinydashboardPlus::dashboardUserItem(width = 6, generate_userFields(casestudies, sliderCasestudies)$stat2)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 6,
          HTML(paste("<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>P<sub>i</sub></b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "2.96 mM","<br>",
                     "(2.1-3.4 mM)"))
        )
      },
      if (casestudies$php1() | casestudies$hypopara() | casestudies$hypoD3()) {
        shinydashboardPlus::dashboardUserItem(width = 12, generate_userFields(casestudies, sliderCasestudies)$stat3)
      } else {
        shinydashboardPlus::dashboardUserItem(
          width = 12,
          HTML(paste("<br>",
                     "<p style=\"text-align:center;line-height:2.0\">",
                     "<font face =\"TimesNewRoman\" size=\"+1\">[<em><b>PTH</b></em>]<sub><em><b>p</b></em></sub></font>","<br>",
                     "6.87 pM","<br>",
                     "(3-16 pM)"))
        )
      },
      br()
    )
  })


  # Open the userInfo menu. Useless since rintrojs does not work with modules
  #observeEvent(help(), {
  #  shinyjs::toggleClass(
  #    id = "user",
  #    class = "user-menu open",
  #    condition = help()
  #  )
  #})

}
