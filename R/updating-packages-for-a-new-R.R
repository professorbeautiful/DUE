##  updating packages for a new R.
# 
# gsub(' ', '-', 'updating packages for a new R')
# 'updating-packages-for-a-new-R'
# # linked to 
# 
# # packagesToUpdate = readLines(pipe('pbpaste'))
# previous.lib = "/Library/Frameworks/R.framework/Versions/4.3-x86_64/"
# update.packages(previous.lib, ask = F)
# install.packages(packagesToUpdate)
# devtools::install_github('professorbeautiful/shinyDebuggingPanel')

# also needed to put options(repos = c(CRAN = "https://cloud.r-project.org"))
# into .Rprofile
