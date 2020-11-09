#' Generate a winding cave
#' 
#' @description
#' 
#' Create a data table of coordinates representing the interior of a winding
#' cave generated from random walks on an integer grid.
#' 
#' @param steps the number of steps in cave.
#' @return A `data.table` of `x` and `y` coordinates of a cave interior.
#' @example
#' 
#' # Generate a horizontal winding cave
#' cave <- carve_winding(100, bias = c(1,0))
#' plot(cave, pch = 16, cex = 0.5, asp = 1)
#'
#' # Generate a vertical winding cave
#' cave <- carve_winding(100, bias = c(0,1))
#' plot(cave, pch = 16, cex = 0.5, asp = 1)

carve_winding <- function(steps, bias){
  rw1 <- random_walk(steps, bias)
  rw2 <- copy(rw1)
  rw3 <- copy(rw1)
  rw4 <- copy(rw1)
  if (bias[1] > 0){
    rw2[, y := y + 1]
    rw3[sample(.N, round(steps/2)), y := y - 1]
    rw4[sample(.N, round(steps/2)), y := y + 2]
  }
  if (bias[2] > 0) {
    rw2[, x := x + 1]
    rw3[sample(.N, round(steps/2)), x := x - 1]
    rw4[sample(.N, round(steps/2)), x := x + 2]
  }
  cave <- 
  list(rw1, rw2, rw3, rw4) %>%
    rbindlist() %>%
    unique()
  if (sample(0:1, 1) == 1){
    cave[, x:= -x]
  }
  if (sample(0:1, 1) == 1){
    cave[, y:= -y]
  }
  cave
}
