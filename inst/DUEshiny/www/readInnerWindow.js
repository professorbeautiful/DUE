//tags$head(tags$script('
$(document).on("shiny:connected", 
function(e) {
    Shiny.onInputChange("innerHeight", window.innerHeight);
    Shiny.onInputChange("innerWidth", window.innerWidth);
});
$(window).resize(function(e) {
     Shiny.onInputChange("innerHeight", window.innerHeight);
     Shiny.onInputChange("innerWidth", window.innerWidth);
});
//                      '))