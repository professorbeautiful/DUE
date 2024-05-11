# To view the vignette doc,
# in the Terminal run "open ./DUEshiny/www/DUE_vignette.html"
launchAppLocally = function(){
  if(!require('DUE'))
    install_github('professorbeautiful/DUE')
  if(!require('shinyDebuggingPanel'))
    install_github('professorbeautiful/shinyDebuggingPanel')
  runApp('inst/DUEshiny/')
  }
launchAppAtShinyApps = function(
  app = 'DUEshiny', account='trials',
  browser=getOption('browser')
) {
  browseURL(paste0('https://',
                   account, '.shinyapps.io/',
                   app),
            browser = browser)
}
#launchAppAtShinyApps()
