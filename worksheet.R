
library(icesTAF)
library(shiny)

# draft.data() #
install.deps()

taf.boot()
source.all()
source.taf("shiny.R"); runApp('shiny', launch.browser = FALSE)


taf.boot.path()
source.taf("data.R")