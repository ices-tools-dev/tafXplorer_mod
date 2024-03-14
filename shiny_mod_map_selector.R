mod_map_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      column(4, 
        leaflet::leafletOutput(ns("map_selector")),
      
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

    # create empty character vector to hold map selected locations
    selected_map <- reactiveValues(groups = character())

    observeEvent(input$map_selector_shape_click, {
      # ecoregion added
      if (input$map_selector_shape_click$group == "Eco_regions") {
        proxy_map %>% showGroup(input$map_selector_shape_click$id)

        selected_map$groups <- c(selected_map$groups, input$map_selector_shape_click$id)
      }

      # ecoregion removed
      if (input$map_selector_shape_click$group %in% vocabs$ecoregions) {
        proxy_map %>% hideGroup(input$map_selector_shape_click$group)

        selected_map$groups <- setdiff(selected_map$groups, input$map_selector_shape_click$group)
      }

      updateVirtualSelect("selected_locations", selected = selected_map$groups)
    })

    observeEvent(input$selected_locations,
      {
        removed <- setdiff(selected_map$groups, input$selected_locations)
        added <- setdiff(input$selected_locations, selected_map$groups)

        selected_map$groups <- input$selected_locations

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
