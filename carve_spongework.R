#' Generate a spongework cave
#' 
#' @description
#' 
#' Create a data table of coordinates representing the interior of a spongework
#' cave generated from random walks on an integer grid.
#' 
#' @param steps the number of steps in cave.
#' @return A `data.table` of `x` and `y` coordinates of a cave interior.
#' @example
#' 
#' # Generate a spongework cave
#' cave <- carve_spongework(2000)
#' plot(cave, pch = 16, cex = 0.5, asp = 1)

carve_spongework <- function(steps = 2000){
  list(random_walk(steps),
       random_walk(steps),
       random_walk(steps)) %>%
    rbindlist() %>%
    unique()
}
