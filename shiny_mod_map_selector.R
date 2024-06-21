mod_map_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_fillable(
    layout_columns(
      card(
        #4,
        leaflet::leafletOutput(ns("map_selector")),
        virtualSelectInput(
          inputId = ns("selected_locations"),
          label = "ICES Ecoregions",
          choices = vocabs$ecoregions,
          selected = grep("North Sea", vocabs$ecoregions, value = TRUE),
          multiple = TRUE,
          width = "100%"
        ),
        select_group_ui(
          id = ns("my-filters"),
          params = list(
            year = list(inputId = "year", label = "Assessment year:"),
            stockCode = list(inputId = "stockCode", label = "Stock code:"),
            species = list(inputId = "species", label = "Common name:"),
            expertGroup = list(inputId = "expertGroup", label = "Expert group:"),
            dataCategory = list(inputId = "dataCategory", label = "Data category:")
          ),
          inline = FALSE,
          vs_args = list(
            search = TRUE,
            optionsCount = 5
          )
        )
      ),
      column(
        12,
         reactableOutput(ns("table"))
      ),
      col_widths = c(4,8)
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

      repo_list <- reactive({
        req(input$selected_locations)
        stock_list_long <- getListStockAssessments()
        stock_list_long <- purrr::map_dfr(
          .x = input$selected_locations,
          .f = function(.x) stock_list_long %>% dplyr::filter(str_detect(ecoregion, .x))
        )

        if (nrow(stock_list_long) != 0) {
          stock_list_long %>%
            dplyr::arrange(stockCode) %>%
            dplyr::mutate(
              # EcoRegion = removeWords(EcoRegion, "Ecoregion"),
              # Select = sprintf('<input type="radio" name="rdbtn" value="rdbtn_%s"/>', 1:nrow(.)),
              RepoUrl = paste0("<a href='", gitHubUrl, "' target='_blank'>Link")
              # stock_description = purrr::map_chr(StockKeyLabel, .f = ~ access_sag_data_local(.x, input$selected_years)$StockDescription[1]),
              # stock_location = parse_location_from_stock_description(stock_description)
            )
        }
      })
      group_filter_temp <- select_group_server(
        id = "my-filters",
        data = repo_list(),
        vars = reactive(c(
          "year", "stockCode", "species", "expertGroup", "dataCategory"
        ))
      )

      group_filter <- reactive({
        validate(
          need(!nrow(repo_list()) == 0, "No published stocks in the selected ecoregion and year")
        )

        group_filter_temp() %>%
          select(
            # "Select",
            "stockCode",
            "year",
            # "EcoRegion",
            # "icon",
            "species",
            "expertGroup",
            "dataCategory",
            "RepoUrl"
          ) %>%
          rename(
            # "Select" = Select,
            "Stock code" = stockCode,
            "Year" = year,
            # "Ecoregion" = EcoRegion,
            # " " = icon,
            "Common name" = species,
            "Expert group" = expertGroup,
            "Data category" = dataCategory,
            "Repo Url" = RepoUrl
          )
      })


      output$table <- renderReactable({
        reactable(group_filter(),
          selection = "single",
          filterable = TRUE,
          onClick = "select",
          # defaultPageSize = 300,
          highlight = TRUE,
          striped = TRUE,
          defaultColDef = colDef(
            headerStyle = list(
              background = "#CCF1ED",
              color = "#002B5F"
            )
          ),
          columns = list(
            "Repo Url" = reactable::colDef(
              html = TRUE,
              filterable = FALSE
            )
          ),
          theme = reactableTheme(
            stripedColor = "#E6E7E8",
            highlightColor = "#00B7A31A",
            cellPadding = "2px 2px"
          )
        )
      })

      selected <- reactive(getReactableState("table", "selected"))

      observe({
        selected_row <- group_filter_temp()[selected(), ]
        if (nrow(selected_row) > 0) {
          updateURL(repo = basename(selected_row$gitHubUrl))
        }
      })
    
  })
}
