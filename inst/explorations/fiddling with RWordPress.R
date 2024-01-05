#  Ha Ha, just copy/paste the html!!!
##  But to deal with th no-base64 nonsense, see ready-for-wordpress.sh
# https://yihui.org/knitr/demo/wordpress/

# "(but please note that this package has not been updated for many years, which is not a good sign). "

# if (!require('RWordPress'))
#   install.packages('RWordPress', repos = 'https://www.omegahat.org/R', type = 'source')

install.packages("XML")
devtools:::install_github("duncantl/XMLRPC")
devtools:::install_github("duncantl/RWordPress")
library(RWordPress)
library(knitr)
options(WordpressLogin = c(user = 'password'),
       WordpressURL = 'https://user.wordpress.com/xmlrpc.php')
options(WordpressLogin = 
          c(user = 'admin', password='four^crowZZ'),
        WordpressURL = 'http://www.professorbeautiful.org/IveBeenThinkin/wp-admin')
knit2wp('inst/explorations/DUE explorations.Rmd', 
        title = 'Your post title')
