
library(icesTAF)
library(shiny)

# draft.data() #
install.deps()

taf.boot()
source.all()
source.taf("shiny.R"); runApp('shiny', launch.browser = TRUE)


taf.boot.path()
source.taf("data.R")



#deploy on shiny.io
rsconnect::deployApp(appDir = "shiny", appName = "TAFXplorer", account = 'ices-taf',  forceUpdate = TRUE)

