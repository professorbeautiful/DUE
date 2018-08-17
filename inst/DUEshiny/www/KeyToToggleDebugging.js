var ctrlDpressed = false;

$(document).on("keyup", function (e) {
    var x = event.which || event.keyCode;
    if(x==68 && e.ctrlKey) { // control d
      ctrlDpressed = ! ctrlDpressed;  // toggle
      Shiny.onInputChange("ctrlDpressed", ctrlDpressed);

      //navigateToId(currentLocationId);
      //alert('control d ' + e.ctrlKey + ' ' + event.ctrlKey);
      // The alert works.
      // The panel appears only after something else JS happens.
      //
    }
});
