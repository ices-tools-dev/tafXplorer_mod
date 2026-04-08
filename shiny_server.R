# # the app logic
# server <- function(input, output, session) {
#   onload <- reactiveVal(TRUE)

#   # log in values
#   token <- reactiveVal("")
#   user <- reactiveVal(list())

#   # we take the first free slot
#   nslots <- 3
#   free_slots <- reactiveVal(paste(1:nslots))
#   file_tree <- reactiveValues()
#   repos <- reactiveValues()
#   filenames <- reactiveVal(character(0))


#   # # When OK button is pressed, attempt to authenticate. If successful,
#   # # remove the modal.
#   # observeEvent(input$login, {
#   #   isolate({
#   #     Username <- input$username
#   #     Password <- input$password
#   #   })
#   #   jwt <- ices_token(username = Username, password = Password, refresh = TRUE)
#   #   token(jwt)

#   #   if (!is.empty(token())) {
#   #     userinfo <- get_with_token("https://taf.ices.dk/api/User", token())
#   #     user(userinfo)
#   #     removeModal()
#   #     # add map selector
#   #     mod_map_selector_server("map_selector_1")
#   #     appendTab(
#   #       "tabset",
#   #       tabPanel(
#   #         title = "Stock assessment selection",
#   #         value = "Stock assessment selection",
#   #         mod_map_selector_ui("map_selector_1")
#   #       ),
#   #       select = TRUE
#   #     )
#   #   } else {
#   #     showModal(loginModal(failed = TRUE))
#   #   }
#   # })

#   mod_map_selector_server("map_selector_1")
#   # appendTab(
#   #   "tabset",
#   #   tabPanel(
#   #     title = "TAF assessment selection",
#   #     value = "TAF assessment selection",
#   #     mod_map_selector_ui("map_selector_1")
#   #   ),
#   #   select = TRUE
#   # )
#   # insertTab(
#   #   inputId = "tabset",
#   #   target = "resources",
#   #   tabPanel(
#   #     title = "TAF assessment selection",
#   #     value = "TAF assessment selection",
#   #     mod_map_selector_ui("map_selector_1")
#   #   ),
#   #   position = "before",
#   #   select = TRUE
#   # )


#   # observe first url
#   observeEvent(session$clientData$url_search,
#     {
#       if (onload()) {
#         # showModal(loginModal())

#         # print("observing first url")
#         query <- getQueryString()

#         if (is.null(query$repo)) {
#           # not valid
#           updateURL()
#         } else {
#           repos[[paste0("file_tree_", free_slots()[1])]] <- query$repo
#           query_file_tree <- CreateInteractiveTreeDF(query$repo)
#           file_tree[[paste0("file_tree_", free_slots()[1])]] <- query_file_tree

#           appendTab(
#             "tabset",
#             tabPanel(
#               tab_title(query$repo),
#               layout_sidebar(
#                 sidebar = sidebar(
#                   mod_file_tree_ui(paste0("file_tree_", free_slots()[1])),
#                   width = "40%"
#                 ),
#                 mod_file_viz_ui(paste0("file_viz_", free_slots()[1]))
#               )
#             ),
#             select = TRUE
#           )

#           # add files
#           if (!is.null(query$file)) {
#             query_files <- strsplit(query$file, ",")[[1]]
#             query_file_ids <- which(query_file_tree$pathString %in% file.path(query$repo, query_files))
#             filenames(paste0("file_tree_1-", query_file_ids))
#           }

#           # remove from empy slot
#           free_slots(free_slots()[-1])
#         }
#         onload(FALSE)
#       }
#     },
#     priority = 99
#   )

#   # only run after initialisation
#   observeEvent(session$clientData$url_search,
#     {
#       print("observing url")
#       query <- getQueryString()

#       repos_vec <- unlist(reactiveValuesToList(repos))

#       if (is.null(query$repo) || query$repo == "") {
#         updateURL()
#       } else if (!query$repo %in% repos_vec && length(repos_vec) >= nslots) {
#         updateURL()
#       } else if (query$repo %in% repos_vec) {
#         updateURL(repo = query$repo, file = query$file) # trim url
#         if (trimws(strsplit(input$tabset, "\n")[[1]][2]) != query$repo) {
#           updateTabsetPanel(inputId = "tabset", selected = query$repo)
#         }
#       } else {
#         updateURL(repo = query$repo, file = query$file) # trim url
#         repos[[paste0("file_tree_", free_slots()[1])]] <- query$repo
#         file_tree[[paste0("file_tree_", free_slots()[1])]] <- CreateInteractiveTreeDF(query$repo)
#         appendTab(
#           "tabset",
#           tabPanel(
#             tab_title(query$repo),
#             layout_sidebar(
#               sidebar = sidebar(
#                 mod_file_tree_ui(paste0("file_tree_", free_slots()[1])),
#                 width = "40%"
#               ),
#               mod_file_viz_ui(paste0("file_viz_", free_slots()[1]))
#             )
#           ),
#           select = TRUE
#         )

#         # remove from empy slot
#         free_slots(free_slots()[-1])
#       }

#       onload(FALSE)
#     },
#     ignoreInit = TRUE
#   )

#   # only run after initialisation
#   observeEvent(input$remove_tab,
#     {
#       removeTab(inputId = "tabset", target = paste(tab_title(input$remove_tab)))
#       slot_to_free <- names(which(unlist(reactiveValuesToList(repos)) == input$remove_tab))

#       repos[[slot_to_free]] <- NULL
#       file_tree[[slot_to_free]] <- NULL
#       filenames(filenames()[!grepl(slot_to_free, filenames())])

#       free_slots(c(free_slots(), gsub("file_tree_", "", slot_to_free)))
#     },
#     ignoreInit = TRUE
#   )

#   # only run after initialisation
#   ## NEVER RUNS!! input$tabset is always NULL
#   observeEvent(input$tabset,
#     {
#       print("observing tab click")
#       if (input$tabset == "Stock assessment selection") {
#         updateURL()
#       } else {
#         query <- getQueryString()
#         repo <- trimws(strsplit(input$tabset, "\n")[[1]][2])
#         updateURL(repo = repo, file = query$file, mode = "replace")
#       }
#       onload(FALSE)
#     },
#     ignoreInit = TRUE
#   )

#   observeEvent(input$clicked_text, {
#     if (!input$clicked_text %in% filenames()) {
#       filenames(c(filenames(), input$clicked_text))
#     }
#   })

#   # Main modules
#   mod_file_tree_server("file_tree_1", file_tree)
#   mod_file_tree_server("file_tree_2", file_tree)
#   mod_file_tree_server("file_tree_3", file_tree)

#   mod_file_viz_server("file_viz_1", repos, file_tree, filenames)
#   mod_file_viz_server("file_viz_2", repos, file_tree, filenames)
#   mod_file_viz_server("file_viz_3", repos, file_tree, filenames)


#   # # Debugging
#   # observe({
#   #   print("---changes---")
#   #   print(free_slots())
#   #   print("input$tabset")
#   #   print(input$tabset)
#   #   print(session$clientData$url_search)
#   #   print(paste("on load", onload()))
#   #   print(unlist(reactiveValuesToList(repos)))
#   #   print(filenames())
#   #   print("input$clicked_text:")
#   #   print(input$clicked_text)
#   #   # print("here comes the token:")
#   #   # print(token())
#   #   print("here comes the user:")
#   #   print(user())
#   #   print("input$remove_tab")
#   #   print(input$remove_tab)
#   # })
# }

server <- function(input, output, session) {
  onload <- reactiveVal(TRUE)

  # log in values
  token <- reactiveVal("")
  user <- reactiveVal(list())

  # maximum number of dynamic stock tabs
  nslots <- 3
  free_slots <- reactiveVal(as.character(1:nslots))

  file_tree <- reactiveValues()
  repos <- reactiveValues()
  filenames <- reactiveVal(character(0))

  #--------------------------------------------------
  # Helpers
  #--------------------------------------------------

  open_stock_tab <- function(repo, slot, select = TRUE) {
    insertTab(
      inputId = "tabset",
      target = "dynamic_anchor",
      position = "before",
      tabPanel(
        title = tab_title(repo),
        value = repo,
        layout_sidebar(
          sidebar = sidebar(
            mod_file_tree_ui(paste0("file_tree_", slot)),
            width = "40%"
          ),
          mod_file_viz_ui(paste0("file_viz_", slot))
        )
      ),
      select = select
    )
  }

  close_stock_tab <- function(repo) {
    removeTab(inputId = "tabset", target = repo)
  }

  current_repos <- reactive({
    unlist(reactiveValuesToList(repos), use.names = TRUE)
  })

  update_repo_url <- function(repo = NULL, file = NULL, mode = "replace") {
    if (is.null(repo) || identical(repo, "") || identical(repo, "map_selector")) {
      updateQueryString("", mode = mode, session = session)
    } else {
      params <- paste0("?repo=", URLencode(repo, reserved = TRUE))
      if (!is.null(file) && !identical(file, "")) {
        params <- paste0(params, "&file=", URLencode(file, reserved = TRUE))
      }
      updateQueryString(params, mode = mode, session = session)
    }
  }

  add_repo_to_slot <- function(repo, slot) {
    repos[[paste0("file_tree_", slot)]] <- repo
    file_tree[[paste0("file_tree_", slot)]] <- CreateInteractiveTreeDF(repo)
  }

  get_slot_for_repo <- function(repo) {
    slot_name <- names(which(current_repos() == repo))
    if (length(slot_name) == 0) return(NULL)
    slot_name
  }

  #--------------------------------------------------
  # Main fixed module
  #--------------------------------------------------

  mod_map_selector_server("map_selector_1")

  #--------------------------------------------------
  # Initial URL load
  #--------------------------------------------------

  observeEvent(session$clientData$url_search, {
    if (!onload()) return()

    query <- getQueryString()

    if (is.null(query$repo) || identical(query$repo, "")) {
      update_repo_url()
      onload(FALSE)
      return()
    }

    # Open repo from URL only if there is a free slot
    if (length(free_slots()) > 0) {
      slot <- free_slots()[1]

      add_repo_to_slot(query$repo, slot)

      query_file_tree <- file_tree[[paste0("file_tree_", slot)]]

      open_stock_tab(query$repo, slot, select = TRUE)

      # Preselect files if present in URL
      if (!is.null(query$file) && !identical(query$file, "")) {
        query_files <- strsplit(query$file, ",")[[1]]
        query_file_ids <- which(
          query_file_tree$pathString %in% file.path(query$repo, query_files)
        )

        if (length(query_file_ids) > 0) {
          filenames(c(
            filenames(),
            paste0("file_tree_", slot, "-", query_file_ids)
          ))
        }
      }

      free_slots(free_slots()[-1])
    } else {
      update_repo_url()
    }

    onload(FALSE)
  }, priority = 99)

  #--------------------------------------------------
  # React to URL changes after initialization
  #--------------------------------------------------

  observeEvent(session$clientData$url_search, {
    query <- getQueryString()
    repos_vec <- unname(current_repos())

    if (is.null(query$repo) || identical(query$repo, "")) {
      update_repo_url()
      return()
    }

    # Repo already open -> just select it
    if (query$repo %in% repos_vec) {
      update_repo_url(repo = query$repo, file = query$file)
      updateTabsetPanel(session, inputId = "tabset", selected = query$repo)
      return()
    }

    # Repo not open and no slots left
    if (length(repos_vec) >= nslots) {
      update_repo_url()
      return()
    }

    # Open new repo in free slot
    slot <- free_slots()[1]

    add_repo_to_slot(query$repo, slot)
    open_stock_tab(query$repo, slot, select = TRUE)

    free_slots(free_slots()[-1])

  }, ignoreInit = TRUE)

  #--------------------------------------------------
  # Remove dynamic stock tab
  #--------------------------------------------------

  observeEvent(input$remove_tab, {
    req(input$remove_tab)

    repo_to_remove <- input$remove_tab
    slot_to_free <- get_slot_for_repo(repo_to_remove)

    req(!is.null(slot_to_free))

    close_stock_tab(repo_to_remove)

    repos[[slot_to_free]] <- NULL
    file_tree[[slot_to_free]] <- NULL

    filenames(filenames()[!grepl(slot_to_free, filenames(), fixed = TRUE)])

    freed_slot <- gsub("file_tree_", "", slot_to_free)
    free_slots(sort(unique(c(free_slots(), freed_slot))))

    # If removed tab was selected, go back to main selector
    updateTabsetPanel(session, inputId = "tabset", selected = "map_selector")
    update_repo_url()

  }, ignoreInit = TRUE)

  #--------------------------------------------------
  # Navbar tab selection -> update URL
  #--------------------------------------------------

  observeEvent(input$tabset, {
    req(input$tabset)

    # Main tab
    if (identical(input$tabset, "map_selector")) {
      update_repo_url()
      return()
    }

    # Right-side fixed tab or hidden anchor
    if (input$tabset %in% c("resources", "dynamic_anchor")) {
      return()
    }

    # Dynamic repo tab
    query <- getQueryString()
    update_repo_url(repo = input$tabset, file = query$file, mode = "replace")

  }, ignoreInit = TRUE)

  #--------------------------------------------------
  # Track clicked files
  #--------------------------------------------------

  observeEvent(input$clicked_text, {
    req(input$clicked_text)

    if (!input$clicked_text %in% filenames()) {
      filenames(c(filenames(), input$clicked_text))
    }
  })

  #--------------------------------------------------
  # Dynamic modules
  #--------------------------------------------------

  mod_file_tree_server("file_tree_1", file_tree)
  mod_file_tree_server("file_tree_2", file_tree)
  mod_file_tree_server("file_tree_3", file_tree)

  mod_file_viz_server("file_viz_1", repos, file_tree, filenames)
  mod_file_viz_server("file_viz_2", repos, file_tree, filenames)
  mod_file_viz_server("file_viz_3", repos, file_tree, filenames)

  #--------------------------------------------------
  # Optional debugging
  #--------------------------------------------------
  # observe({
  #   cat("\n--- changes ---\n")
  #   print(free_slots())
  #   print(input$tabset)
  #   print(session$clientData$url_search)
  #   print(onload())
  #   print(current_repos())
  #   print(filenames())
  #   print(input$clicked_text)
  #   print(user())
  #   print(input$remove_tab)
  # })
}