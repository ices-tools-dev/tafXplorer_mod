# the app logic
server <- function(input, output, session) {
  onload <- reactiveVal(TRUE)

  # we take the first free slot
  nslots <- 3
  free_slots <- reactiveVal(paste(1:nslots))
  file_tree <- reactiveValues()
  repos <- reactiveVal(character(0))
  filenames <- reactiveVal(character(0))

  # observe first url
  observeEvent(session$clientData$url_search,
    {
      if (onload()) {
        # print("observing first url")
        query <- getQueryString()

        if (is.null(query$repo)) {
          # not valid
          updateURL()
        } else {
          repos(query$repo)
          file_tree[[paste0("file_tree_", free_slots()[1])]] <- CreateInteractiveTreeDF(query$repo)
          appendTab(
            "tabset",
            tabPanel(
              query$repo,
              layout_sidebar(
                sidebar = sidebar(
                  mod_file_tree_ui(paste0("file_tree_", free_slots()[1])),
                  width = "40%"
                ),
                mod_file_viz_ui(paste0("file_viz_", free_slots()[1]))
              )
            ),
            select = TRUE
          )
          # remove from empy slot
          free_slots(free_slots()[-1])
        }
        onload(FALSE)
      }
    },
    priority = 99
  )

  # these only run after initialisation
  observeEvent(session$clientData$url_search,
    {
      print("observing url")
      query <- getQueryString()

      if (is.null(query$repo) || query$repo == "") {
        updateURL()
      } else if (!query$repo %in% repos() && length(repos()) >= nslots) {
        updateURL()
      } else if (query$repo %in% repos()) {
        updateURL(repo = query$repo, file = query$file) # trim url
      if (input$tabset != query$repo) {
        updateTabsetPanel(inputId = "tabset", selected = query$repo)
      }
        # some logic for file selection
      } else {
        updateURL(repo = query$repo, file = query$file) # trim url
        repos(c(repos(), query$repo))
        file_tree[[paste0("file_tree_", free_slots()[1])]] <- CreateInteractiveTreeDF(query$repo)
        appendTab(
          "tabset",
          tabPanel(
            query$repo,
            layout_sidebar(
              sidebar = sidebar(
                mod_file_tree_ui(paste0("file_tree_", free_slots()[1])),
                width = "40%"
              ),
              mod_file_viz_ui(paste0("file_viz_", free_slots()[1]))
            )
          ),
          select = TRUE
        )

        # remove from empy slot
        free_slots(free_slots()[-1])
      }

      onload(FALSE)
    },
    ignoreInit = TRUE
  )

  # these only run after initialisation
  observeEvent(input$tabset,
    {
      print("observing tab click")
      if (input$tabset == "Stock assessment selection") {
        updateURL()
      } else {
        query <- getQueryString()
        updateURL(repo = input$tabset, file = query$file, mode = "replace")
      }
      onload(FALSE)
    },
    ignoreInit = TRUE
  )

  observe({
    print("---changes---")
    print(free_slots())
    print(input$tabset)
    print(session$clientData$url_search)
    print(paste("on load", onload()))
    print(repos())
    print(filenames())
    print(input$clicked_text)
    print("=============")
  })

  observeEvent(input$clicked_text, {
    if (!input$clicked_text %in% filenames()) {
      filenames(c(filenames(), input$clicked_text))
    }
  })

  mod_map_selector_server("map_selector_1")
  mod_file_tree_server("file_tree_1", file_tree)
  mod_file_tree_server("file_tree_2", file_tree)
  mod_file_tree_server("file_tree_3", file_tree)

  mod_file_viz_server("file_viz_1", repos, filenames)
  mod_file_viz_server("file_viz_2", repos, filenames)
  mod_file_viz_server("file_viz_3", repos, filenames)
}
