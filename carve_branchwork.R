#' Generate a branchwork cave
#' 
#' @description
#' 
#' Create a data table of coordinates representing the interior of a branchwork
#' cave generated from random walks on an integer grid.
#' 
#' @param steps the number of steps in cave.
#' @return A `data.table` of `x` and `y` coordinates of a cave interior.
#' @example
#' 
#' # Generate a branchwork cave
#' cave <- carve_branchwork(2000)
#' plot(cave, pch = 16, cex = 0.5, asp = 1)

carve_branchwork <- function(steps){
  cave <- 
  list(carve_winding(steps/20, bias = c(0.4,0.2)),
       carve_winding(steps/20, bias = c(0.2,0.4)),
       carve_winding(steps/20, bias = c(0.7,0)),
       carve_winding(steps/20, bias = c(0,0.7))) %>% 
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
