pigeon_picross <- function(x = 10, y = 10, seed = 112211, locations = .5){
  picross_fill <- c(rep(1, trunc(x*y*locations)), rep(0, x*y - trunc(x*y*locations) ))
  set.seed(seed)
  picross_fill <- sample(picross_fill)
  picross_inner <- matrix(picross_fill, nrow = y, ncol = x)
  return(picross_inner)
}
