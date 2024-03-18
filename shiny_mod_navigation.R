mod_navigation_ui <- function(id) {
  ns <- NS(id)
  tagList()
}




mod_navigation_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # values of the query string and first visit flag
        query <- reactiveValues(query_from_table = FALSE, update_from_url = TRUE)
        selectedFile <- reactiveValues()

        old_tabset <- ""

        observe({
            # reacts to tab click, and url update
            # get stuff we need
            print("")
            print("")
            print("*** observing url and input ***")
            query_string <- getQueryString()
            names(query_string) <- tolower(names(query_string))

            tabset <- input$tabset

            print("old tab:")
            print(old_tabset)
            print("current url:")
            print(str(query_string))
            print("current tab:")
            print(input$tabset)

            if (old_tabset == "") {
                # then server loop has restarted: i.e. a url has been entered

                if (is.null(query_string$tab)) {
                    # no tab specified, update url and old tabset
                    print("updating url from empty")
                    updateQueryString(paste0("?tab=", tabset), mode = "push")
                    old_tabset <<- tabset # always will be 1st tab
                } else {
                    # no tab specified, update url and old tabset
                    print("updating tab from new URL")
                    old_tabset <<- query_string$tab
                    updateNavbarPage(session, "tabset", selected = query_string$tab)
                }
            } else {
                if (input$tabset != old_tabset) {
                    # then a tab has been clicked inside a session
                    # page is correct, url out of date
                    print("updating query - tab clicked")
                    old_tabset <<- tabset
                    updateQueryString(paste0("?tab=", tabset), mode = "push")
                } else if (query_string$tab != old_tabset) {
                    # then a url has been written inside a session without a tab click
                    # url is correct, page out of date
                    print("updating tab - url modified")
                    old_tabset <<- query_string$tab
                    updateNavbarPage(session, "tabset", selected = query_string$tab)
                } else {
                    print("nothing to do")
                }
            }

            # special case for assessment results tab
            if (query_string$tab == "Assessment results" && !is.null(query_string$repo) && !is.null(query_string$file)) {
                print("setting selected file to:")
                print(query_string$file)
                selectedFile$name <- paste0(query_string$repo, "/", query_string$file)
            }

            print("*** END observing query and input ***")
            print("")
            print("")
        })
    })
}