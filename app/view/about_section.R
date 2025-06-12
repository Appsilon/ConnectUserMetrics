box::use(
  htmltools[
    h4,
  ],
  shiny,
)

box::use(
  app/logic/ui_utils[
    brand
  ],
  app/logic/utils[
    create_image_path
  ],
  app/view/ui_components[
    card,
    link
  ],
)

about_credits <- brand$meta$credits$about

about_section <- shiny$div(
  class = "about-section",
  shiny$div(
    class = "about-links",
    lapply(about_credits$references, \(x) link(x$name, x$link))
  )
)

about_section <- shiny$div(
  class = "about-section",
  shiny$div(
    shiny$markdown("
> Monitor application adoption, track user engagement,
and get usage logs for all your Shiny applications.

This dashboard provides comprehensive analytics for Shiny applications deployed on
your Posit Connect account:

- **Analyze usage patterns**: Choose time periods and view data by day, week, or month
- **Flexible grouping**: Combine data by application, user, and/or
date to get different perspectives
- **Interactive visualizations**: View session counts and unique user metrics through
charts and summary tables
- **Smart filtering**: Filter by specific applications and users, set minimum session
duration thresholds
- **Export capabilities**: Download both raw and aggregated data as CSV files
- **User insights**: Track both total sessions and unique user engagement across
your applications
")
  )
)


tech_section <- shiny$div(
  class = "tech-section",
  h4(class = "tech-heading", "Powered by"),
  lapply(
    about_credits$powered_by,
    function(x) {
      card(
        href_link = x$link,
        img_link = create_image_path(x$img_name),
        card_header = x$name,
        card_text = x$desc
      )
    }
  )
)

brand_section <- shiny$div(
  class = "brand-section",
  shiny$a(
    class = "brand-logo",
    href = about_credits$references$homepage$link,
    target = "_blank",
    rel = "noopener noreferrer",
    shiny$img(
      src = create_image_path(brand$logo),
      alt = "Appsilon"
    )
  ),
  shiny$p(
    class = "brand-summary",
    about_credits$summary
  )
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "info-btns",
    shiny$actionButton(
      inputId = ns("open_about_modal"),
      class = "info-btn",
      label = "",
      icon = shiny$icon("info-circle")
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny$observeEvent(
      input$open_about_modal,
      ignoreNULL = TRUE,
      {
        shiny$showModal(
          shiny$modalDialog(
            easyClose = TRUE,
            title = brand$meta$app_title,
            size = "l",
            about_section,
            tech_section,
            brand_section
          )
        )
      }
    )
  })
}
