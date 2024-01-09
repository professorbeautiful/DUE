rt.outcome.colors = function(outcome)
  c(R='#00ff00', T='#ff0000', rt='#8E9233', rT='#F007E6', 
    Rt='#009215', RT='#7367D4', RLT='#6C9291', EU='#000000') [outcome]

probLineNames <- rt.outcome.strings <- function(outcome)
  names(rt.outcome.colors(outcome))
