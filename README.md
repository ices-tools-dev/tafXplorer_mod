# tafXplorer_mod
Modules version of tafXplorer

To run this app use
```r
library(icesTAF)
library(shiny)

# draft.data() #
install.deps()

taf.boot()
source.all()
source.taf("shiny.R"); runApp('shiny', launch.browser = FALSE)
```
