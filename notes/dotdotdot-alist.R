temp = function(...) {
  params <- eval(substitute(alist(...)))
  # alist handles its arguments as if they described function arguments. So the
  # values are not evaluated, and tagged arguments with no value are allowed
  # whereas list simply ignores them
  lapply(params, eval)
}

temp(a=1:4, b=letters[3:7])