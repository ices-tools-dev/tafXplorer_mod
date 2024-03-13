

# load models
# source("utilities_models.R")
# source("utilities_vpa.R")

# the app logic
server <- function(input, output, session) {
  mod_map_selector_server("map_selector_1")
  # mod_03_biological_production_server("biological_production")
  # mod_06_VPA_server("vpa")
  # mod_examples_server("examples")
}
