#' Generate a minework cave
#' 
#' @description
#' 
#' Create a data table of coordinates representing the interior of a mine-like
#' cave generated from random walks on an integer grid.
#' 
#' @param steps the number of steps in cave.
#' @return A `data.table` of `x` and `y` coordinates of a cave interior.
#' @example
#' 
#' # Generate a minework cave
#' cave <- carve_minework(2000)
#' plot(cave, pch = 16, cex = 0.5, asp = 1)

carve_minework <- function(steps = 2000){
  list(random_walk(steps, runs = round(steps/100)),
       random_walk(steps, runs = round(steps/100))) %>%
    rbindlist() %>%
    unique()
}
