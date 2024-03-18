
# the app logic
server <- function(input, output, session) {
  mod_navigation_server("navigation_1")
  mod_map_selector_server("map_selector_1")
}
