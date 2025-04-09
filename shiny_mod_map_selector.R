mod_map_selector_ui <- function(id) {
  ns <- NS(id)

  leftCard <- card(
    tags$img(id = "logo", class = "center-block", src = "TAFXplorer blue.png"),
    leaflet::leafletOutput(ns("map_selector"), width = "95%"),
    virtualSelectInput(
      inputId = ns("selected_locations"),
      label = "ICES Ecoregions",
      choices = vocabs$ecoregions,
      selected = grep("North Sea", vocabs$ecoregions, value = TRUE),
      multiple = TRUE,
      width = "90%"
    ),
    select_group_ui(
      id = ns("my-filters"),
      params = list(
        year = list(inputId = "year", label = "Assessment year:"),
        stockCode = list(inputId = "stockCode", label = "Stock code:"),
        species = list(inputId = "CommonName", label = "Common name:"),
        expertGroup = list(inputId = "expertGroup", label = "Expert group:"),
        dataCategory = list(inputId = "dataCategory", label = "Data category:")
      ),
      inline = FALSE,
      vs_args = list(
        search = TRUE,
        optionsCount = 5
      )
    )
  )

  rightCard <- card(
    reactableOutput(ns("table"))
  )


  tagList(
    page_fillable(
      layout_column_wrap(
        width = NULL,
        style = htmltools::css(grid_template_columns = "1fr 2fr"),
        leftCard,
        rightCard
      )
    )
  )
}

mod_map_selector_server <- function(id, token) {
  moduleServer(id, function(input, output, session) {
    print("mod_map_selector_server running")
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
      # stock_list_long <- getListStockAssessments()

      stock_list_long <- jsonlite::fromJSON("www/test_list.json")
      
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
          ) %>% 
          mutate(
                Diagnostics = mapply(function(df, mf, of, rf, sr) {
                  paste(
                    # paste0(rep('<i class="fa-solid fa-database" title="Data file"></i>', df), collapse = " "),
                    if (df == "1") {
                      '<img src="data_green.png" title="Data files correct" height="20px"/>'
                    } else if (df == "2") {
                      '<img src="data_red.png" title="Data files warning/error" height="20px"/>'
                    } else if (df == "3") {
                      '<img src="data_grey.png" title="No data files" height="20px"/>'
                    },                    
                    # paste0(rep('<i class="fa-solid fa-chart-diagram" title="Model file"></i>', mf), collapse = " "),
                    if (mf == "1") {
                      '<img src="model_green.png" title="Model files correct" height="20px"/>'
                    } else if (mf == "2") {
                      '<img src="model_red.png" title="Model files warning/error" height="20px"/>'
                    } else if (mf == "3") {
                      '<img src="model_grey.png" title="No model files" height="20px"/>'
                    },
                    # paste0(rep('<i class="fa-solid fa-file-export" title="Output file"></i>', of), collapse = " "),
                    if (of == "1") {
                      '<img src="output_green.png" title="Output files correct" height="20px"/>'
                    } else if (of == "2") {
                      '<img src="output_red.png" title="Output files warning/error" height="20px"/>'
                    } else if (of == "3") {
                      '<img src="output_grey.png" title="No output files" height="20px"/>'
                    },
                    # paste0(rep('<i class="fa-solid fa-file-import" title="Report file"></i>', rf), collapse = " "),
                    if (rf == "1") {
                      '<img src="report_green.png" title="Report files correct" height="20px"/>'
                    } else if (rf == "2") {
                      '<img src="report_red.png" title="Report files warning/error" height="20px"/>'
                    } else if (rf == "3") {
                      '<img src="report_grey.png" title="No report files" height="20px"/>'
                    },
                    # paste0(rep('<i class="fa-solid fa-chart-line" title="Output file"></i>', of), collapse = " "),
                    # paste0(rep('<i class="fa-solid fa-file-contract" title="Report file"></i>', rf), collapse = " "),
                    if (sr == "TRUE") {
                      '<img src="check.png" title="Run successful" height="20px"/>'
                    } else {
                      '<img src="delete.png" title="Run failed" height="20px"/>'
                    },
                    sep = " "
                  )
                }, DataFiles, ModelFiles, OutputFiles, ReportFiles, SuccessfulRun)
              )

      }
    })

    group_filter_full <- select_group_server(
      id = "my-filters",
      data = repo_list(),
      vars = reactive(c(
        "year", "stockCode", "CommonName", "expertGroup", "dataCategory", "Diagnostics"
      ))
    )

    group_filter <- reactive({
      validate(
        need(!nrow(repo_list()) == 0, "No published stocks in the selected ecoregion and year")
      )

      group_filter_full() %>%
        select(
          # "Select",
          "stockCode",
          "year",
          # "EcoRegion",
          # "icon",
          "CommonName",
          "expertGroup",
          "dataCategory",
          "RepoUrl",
          "Diagnostics"
        ) %>%
        rename(
          # "Select" = Select,
          "Stock code" = stockCode,
          "Year" = year,
          # "Ecoregion" = EcoRegion,
          # " " = icon,
          "Common name" = CommonName,
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
        defaultPageSize = 30,
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
          ),
          Diagnostics = colDef(
            name = "Diagnostics",
            cell = function(value) HTML(value),
            html = TRUE
          )
        ),
        theme = reactableTheme(
          stripedColor = "#E6E7E8",
          highlightColor = "#00B7A31A",
          cellPadding = "2px 2px"
        )
      )
    })

    #selected <- reactive(getReactableState("table", "selected"))

    observe({
      req(input$selected_locations)
      selected_row <- group_filter_full()[getReactableState("table", "selected"), ]
      if (nrow(selected_row) > 0) {
        updateURL(repo = basename(selected_row$gitHubUrl))
      }
    })
  })
}
