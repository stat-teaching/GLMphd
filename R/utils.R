#'@description
#'read rda file as rds giving a name
#'
read_rda <- function(x){
  env <- new.env()
  load(x, envir = env)
  get(ls(env), envir = env)
}


cut_extreme <- function(x, min, max){
  x[x < min] <- min
  x[x > max] <- max
  return(x)
}
