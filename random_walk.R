#' Create a two-dimensional random walk
#' 
#' Generates a series of coordinates representing an unbiased two-dimensional 
#' random talk on an integer grid.
#' 
#' @param steps the number of steps in walk.
#' @param diagonal logical: should the walk include 8-way movement?
#' @param runs the number of contiguous runs to include in the walk.
#' @param reach the number of steps in each contiguous run.
#' @return A `data.table` of `x` and `y` coordinates representing a random walk.
#' @example
#' # Random walk without runs
#' rw <- random_walk(3000)
#' plot(rw, pch = 16, cex = 0.5, asp = 1)
#' lines(rw, col = "grey", lwd = 0.5)
#' 
#' #Random walk with 8-way movement
#' rwr <- random_walk(3000, diagonal = FALSE)
#' plot(rwr, pch = 16, cex = 0.5, asp = 1)
#' lines(rwr, col = "grey", lwd = 0.5)
#' 
#' #Random walk with runs
#' rwr <- random_walk(3000, runs = 15)
#' plot(rwr, pch = 16, cex = 0.5, asp = 1)
#' lines(rwr, col = "grey", lwd = 0.5)

random_walk <- function(steps, diagonal = FALSE, runs = 0, reach = 10){
  moves <- data.table(expand.grid(x = -1:1, y = -1:1, tag = NA))
  if (diagonal == FALSE){
    moves <- moves[(abs(x) + abs(y)) < 2]
  }
  trail <- moves[sample(.N, steps, replace = TRUE)]
  if (runs > 0){
    trail$tag <- sample(1:round(steps/reach), steps, replace = TRUE)
    trail <- trail[order(tag)]
    for (i in 1:runs){
      trail[tag %in% sample(1:round(steps/reach),1)] <- moves[sample(.N, 1)]
    }
  }
  trail[,tag := NULL]
  trail[, x:=cumsum(x)][, y:=cumsum(y)]
}

