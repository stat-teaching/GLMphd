eqs <- function(eq){
  eq <- switch(eq,
         "gamma" = "mean  = shape * scale   mean  = shape/rate \nvar   = shape * rate^2  var   = shape/rate^2",
         NA)
  cat(eq)
}
