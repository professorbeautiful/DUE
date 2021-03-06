// See
// https://stackoverflow.com/questions/40538365/r-shiny-how-to-get-square-plot-to-use-full-width-of-panel/40539526#40539526
//  Passes the value of innerWidth to the input$ structure in R,
// for use in renderPlot calls.
function innerWidthHandler(e) {
    Shiny.onInputChange("innerWidth", window.innerWidth);
}
$(document).on(
  "shiny:connected", 
  innerWidthHandler);
$(window).resize( 
  innerWidthHandler );
                            
                            
//  All these attempts don't work well
/* jQuery("*").css("zoom", "97%") */
/*  $("#ThresholdContour{height:100vh !important;}");  
$("#ThresholdContour{width:100vw !important;}"); 
$("#popLinePlot{height:100vh !important;}");  
$("#popLinePlot{width:100vw !important;}");  
//  # tags$head(tags$style("#popLinePlot{width:100vw !important;}") ), 
*/

/*  This does change the appearance of the plot:  
$("#ThresholdContour").css("zoom", "50%");
$("#ThresholdContour").css("zoom", "100%");
$("#popLinePlot").css("zoom", "100%");
This does nothing:
$("#ThresholdContour").css("width", "100vw");
This moves the plot left (100px) or right (1000px)
$("#popLinePlot").css("width", "1000px");
*/

var snap = function (r, snaps, ratios)
{
  var i;
  for (i=0; i < 16; i++) { if ( r < snaps[i] ) return eval(ratios[i]); }
};
var snapThis = function()
{
  var w, l, r;
  w = window.outerWidth, l = window.innerWidth;
  return snap((w - 16) / l,
       [ 0.29, 0.42, 0.58, 0.71, 0.83, 0.95, 1.05, 1.18, 1.38, 1.63, 1.88, 2.25, 2.75, 3.5, 4.5, 100 ],
       [ 0.25, '1/3', 0.5, '2/3', 0.75, 0.9, 1, 1.1, 1.25, 1.5, 1.75, 2, 2.5, 3, 4, 5 ]
  );
};

// This works when the app is first opened.
// 2458 looks perfect.

/*
if(window.innerWidth > 3500) {
  alert('Window zoom is too low. For best appearance, please zoom in (e.g. on OSX, Cmd-plus).');
}

if(window.innerWidth < 2000) {
  alert('Window zoom is too high. For best appearance, please zoom out (e.g. on OSX, Cmd-minus).');
}
*/