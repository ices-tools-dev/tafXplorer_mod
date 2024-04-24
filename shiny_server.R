# the app logic
server <- function(input, output, session) {
  onload <- reactiveVal(TRUE)

  repos <- reactiveVal(character(0))

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
          appendTab(
            "tabset",
            tabPanel(
              query$repo,
              layout_sidebar(
                sidebar = sidebar(
                  mod_file_tree_ui(paste0("file_tree_", length(repos()))),
                  width = "40%"
                ),
                card(
                  card_header(paste0("File: ", "filename")),
                  card_body(mod_file_viz_ui(paste0("file_viz_", length(repos()))))
                )
              )
            ),
            select = TRUE
          )
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
      } else if (query$repo %in% repos() || length(repos()) >= 3) {
        updateURL(repo = query$repo) # trim url
        if (input$tabset != query$repo) {
          updateTabsetPanel(inputId = "tabset", selected = query$repo)
        }
      } else {
        updateURL(repo = query$repo) # trim url
        repos(c(repos(), query$repo))

        if (!onload()) {
          appendTab(
            "tabset",
            tabPanel(
              query$repo,
              layout_sidebar(
                sidebar = sidebar(
                  mod_file_tree_ui(paste0("file_tree_", length(repos()))),
                  width = "40%"
                ),
                card(
                  card_header(paste0("File: ", "filename")),
                  card_body(mod_file_viz_ui(paste0("file_viz_", length(repos()))))
                )
              )
            ),
            select = TRUE
          )
        }
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
        updateURL(repo = input$tabset)
      }
      onload(FALSE)
    },
    ignoreInit = TRUE
  )

  observe({
    print("---changes---")
    print(input$tabset)
    print(session$clientData$url_search)
    print(paste("on load", onload()))
    print(repos())
    print("=============")
  })

  mod_map_selector_server("map_selector_1")
  filename1 <- mod_file_tree_server("file_tree_1", repos)
  filename2 <- mod_file_tree_server("file_tree_2", repos)
  filename3 <- mod_file_tree_server("file_tree_3", repos)

  mod_file_viz_server("file_viz_1", filename1)
  mod_file_viz_server("file_viz_2", filename2)
  mod_file_viz_server("file_viz_3", filename3)
}
