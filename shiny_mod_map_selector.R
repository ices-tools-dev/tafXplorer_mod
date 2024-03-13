mod_map_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(6, leaflet::leafletOutput(ns("map_selector"))),
      column(
        6,
        virtualSelectInput(
          inputId = ns("selected_locations"),
          label = "ICES Ecoregions",
          choices = vocabs$ecoregions,
          selected = grep("North Sea", vocabs$ecoregions, value = TRUE),
          multiple = TRUE,
          width = "100%"
        )
      )
    )
  )
}

mod_map_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map_selector <- renderLeaflet(map)

    proxy_map <- leafletProxy("map_selector")

    # create empty character vector to hold all click ids
    selected <- reactiveValues(groups = character())

    observeEvent(input$map_selector_shape_click, {
      if (input$map_selector_shape_click$group == "Eco_regions") {
        proxy_map %>% showGroup(input$map_selector_shape_click$id)

        selected$groups <- c(selected$groups, input$map_selector_shape_click$id)
      }

      if (input$map_selector_shape_click$group %in% vocabs$ecoregions) {
        proxy_map %>% hideGroup(input$map_selector_shape_click$group)

        selected$groups <- setdiff(selected$groups, input$map_selector_shape_click$group)
      }

      updateVirtualSelect("selected_locations", selected = selected$groups)
    })

    observeEvent(input$selected_locations,
      {
        removed <- setdiff(selected$groups, input$selected_locations)
        added <- setdiff(input$selected_locations, selected$groups)

        selected$groups <- input$selected_locations

        if (length(removed)) {
          proxy_map %>% hideGroup(removed)
        }

        if (length(added)) {
          proxy_map %>% showGroup(added)
        }
      },
      ignoreNULL = FALSE
    )

  })
}
