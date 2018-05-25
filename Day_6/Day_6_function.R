
fun_drawing <- function(x, f, ...){
  if(!is.numeric(x)) stop("'x' must be numeric vector",
                          call. = FALSE)
  if(!is.function(f)) stop("'f' is not a function",
                           call. = FALSE)
  vec <- f(x)
  if(length(x) != length(vec)){
    vec <- rep(vec, length(x))
  }
  plot(x = x, y = vec, ...)
}

fun_drawing(mtcars$mpg, median)
fun_drawing(mtcars$mpg, function(x) median(x) + sample(seq(.1,1,.1), length(x), replace = T))
fun_drawing(mtcars$mpg, function(x) mean(x) + sample(seq(.1,1,.1), length(x), replace = T), pch = 24, col = c('red', 'green'))