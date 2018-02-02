// This works when the app is first opened.


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

if(window.innerWidth > 3500) {
  alert('Window zoom is too low. For best appearance, please zoom in (e.g. on OSX, Cmd-plus).');
}

if(window.innerWidth < 2500) {
  alert('Window zoom is too high. For best appearance, please zoom out (e.g. on OSX, Cmd-minus).');
}

/* jQuery("body").css("zoom", "50%") */
