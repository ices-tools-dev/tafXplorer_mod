
# the app logic
server <- function(input, output, session) {
  stock_select_server <- mod_map_selector_server("map_selector_1")
  fileTree_server <- mod_fileTree_server("fileTree_1")
}
