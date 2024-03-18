## Before:
## After:

library(icesTAF)

# create all folders
mkdir("shiny")
mkdir("shiny/data")

# copy in www data
cp(taf.data.path("www"), "shiny")
# need to move css and js code out of here I think
# and clean up - lots of excess files in here

# copy in server data
#cp(taf.data.path("tafstocks.csv"), "shiny/data")
cp("data/map_data.RData", "shiny/data")


# copy in utilities
cp("utilities_webservices.R", "shiny/utilities_webservices.R")

# copy in server and ui scripts
cp("shiny_ui.R", "shiny/ui.R")
cp("shiny_server.R", "shiny/server.R")

# copy in modules' scripts
cp("shiny_mod_map_selector.R", "shiny/mod_map_selector.R")
cp("shiny_mod_fileTree.R", "shiny/mod_fileTree.R")

msg("Created shiny app. To run, use: \n\n\tlibrary(shiny)\n\trunApp('shiny')\n\n")
