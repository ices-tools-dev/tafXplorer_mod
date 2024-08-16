
# the app logic
server <- function(input, output, session) {
  onload <- reactiveVal(TRUE)

  # log in values
  token <- reactiveVal("")
  user <- reactiveVal(list())

  # we take the first free slot
  nslots <- 3
  free_slots <- reactiveVal(paste(1:nslots))
  file_tree <- reactiveValues()
  repos <- reactiveValues()
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
          repos[[paste0("file_tree_", free_slots()[1])]] <- query$repo
          query_file_tree <- CreateInteractiveTreeDF(query$repo)
          file_tree[[paste0("file_tree_", free_slots()[1])]] <- query_file_tree

          appendTab(
            "tabset",
            tabPanel(
              tab_title(query$repo),
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

          # add files
          if (!is.null(query$file)) {
            query_files <- strsplit(query$file, ",")[[1]]
            query_file_ids <- which(query_file_tree$pathString %in% file.path(query$repo, query_files))
            filenames(paste0("file_tree_1-", query_file_ids))
          }

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

      repos_vec <- unlist(reactiveValuesToList(repos))

      if (is.null(query$repo) || query$repo == "") {
        updateURL()
      } else if (!query$repo %in% repos_vec && length(repos_vec) >= nslots) {
        updateURL()
      } else if (query$repo %in% repos_vec) {
        updateURL(repo = query$repo, file = query$file) # trim url
        if (trimws(strsplit(input$tabset, "\n")[[1]][2]) != query$repo) {
          updateTabsetPanel(inputId = "tabset", selected = query$repo)
        }
      } else {
        updateURL(repo = query$repo, file = query$file) # trim url
        repos[[paste0("file_tree_", free_slots()[1])]] <- query$repo
        file_tree[[paste0("file_tree_", free_slots()[1])]] <- CreateInteractiveTreeDF(query$repo)
        appendTab(
          "tabset",
          tabPanel(
            tab_title(query$repo),
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

  observeEvent(input$remove_tab,
    {
      removeTab(inputId = "tabset", target = paste(tab_title(input$remove_tab)))
      slot_to_free <- names(which(unlist(reactiveValuesToList(repos)) == input$remove_tab))

      repos[[slot_to_free]] <- NULL
      file_tree[[slot_to_free]] <- NULL
      filenames(filenames()[!grepl(slot_to_free, filenames())])

      free_slots(c(free_slots(), gsub("file_tree_", "", slot_to_free)))
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
        repo <- trimws(strsplit(input$tabset, "\n")[[1]][2])
        updateURL(repo = repo, file = query$file, mode = "replace")
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
    print(unlist(reactiveValuesToList(repos)))
    print(filenames())
    print(input$clicked_text)
    print("here comes the token:")
    print(token())
    print("here comes the user:")
    print(user())
    print("=============")
  })

  observeEvent(input$clicked_text, {
    if (!input$clicked_text %in% filenames()) {
      filenames(c(filenames(), input$clicked_text))
    }
  })


  values <- reactiveValues(authenticated = FALSE)

  # Show modal when button is clicked.
  # This `observe` is suspended only whith right user credential
  obs1 <- observe({
    showModal(dataModal())
  })

  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal.
  obs2 <- observeEvent(input$login, {
    isolate({
      Username <- input$username
      Password <- input$password
    })
    jwt <- ices_token(username = Username, password = Password, refresh = TRUE)
    token(jwt)

    if (!is.empty(token())) {
        values$authenticated <- TRUE
        userinfo <- get_with_token("https://taf.ices.dk/api/User", token())
        user(userinfo)
        obs1$suspend()
        removeModal()
      } else {
        values$authenticated <- FALSE
        showModal(dataModal(failed = TRUE))
      }
  })


  # Main modules
  mod_map_selector_server("map_selector_1")
  mod_file_tree_server("file_tree_1", file_tree)
  mod_file_tree_server("file_tree_2", file_tree)
  mod_file_tree_server("file_tree_3", file_tree)

  mod_file_viz_server("file_viz_1", repos, file_tree, filenames)
  mod_file_viz_server("file_viz_2", repos, file_tree, filenames)
  mod_file_viz_server("file_viz_3", repos, file_tree, filenames)
  # }
}
