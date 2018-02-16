function b(b){
  return this.each(
    function(){
      var d=a(this),
      e=d.data("bs.popover"),
      f="object"==typeof b&&b;
      !e&&/destroy|hide/.test(b)||
      (e||d.data("bs.popover",
                  e=new c(this,f))
      ,"string"==typeof b&&e[b]()
      )
    }
  )
}