
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
rsconnect::deployApp(appDir = "shiny", 
                    appName = "TAFXplorer", 
                    account = 'ices-tools-dev',  
                    forceUpdate = TRUE)


install.packages("gh")
library(gh)
library(stringr)
repos <- gh(
  "GET /orgs/{org}/repos",
  org = "ices-advice",
  type = "all",
  per_page = 100,
  .limit = Inf
)
head(repos)
names(repos[[1]])
repos[[1]]$html_url
repo_names <- vapply(repos, `[[`, character(1), "name")
repo_names
repo_urls <- vapply(repos, `[[`, character(1), "html_url")

# split "<year>_<StockKeyLabel>" (e.g. "2026_sol.27.7e") on the first underscore
repo_df <- as.data.frame(str_split_fixed(repo_names, "_", 2))
names(repo_df) <- c("year", "StockKeyLabel")
repo_df$html_url <- repo_urls
repo_df

# list contents of a single repo (root, or pass path = "some/dir")
repo_contents <- gh(
  "GET /repos/{owner}/{repo}/contents/{path}",
  owner = "ices-advice",
  repo = repo_names[6],
  path = ""
)
vapply(repo_contents, `[[`, character(1), "name")

