#' Generate a random walk
#' 
#' @description
#' 
#' Create a data table of coordinates representing a random walk on an integer 
#' grid. Walks can be one- or two-dimensional and include four- or eight-way
#' (i.e. diagonal) steps. Random walks can be interspersed with contiguous runs 
#' of identical steps.
#' 
#' @param steps the number of steps in walk.
#' @param diagonal logical: should the walk include 8-way movement?
#' @param bias the (x, y) bias between 0 (no bias) and 1 (one-dimensional walk).
#' @param runs the number of contiguous runs to include in the walk.
#' @param reach the number of steps in each contiguous run.
#' @return A `data.table` of `x` and `y` coordinates representing a random walk.
#' @example
#' 
#' # One-dimensional random walk
#' rwr <- random_walk(200, diagonal = FALSE, bias = c(1, 0))
#' plot(rwr, pch = 16, cex = 0.5, asp = 1)
#' lines(rwr, col = "grey", lwd = 0.5)
#' 
#' # Two-dimensional random walk without runs
#' rw <- random_walk(3000)
#' plot(rw, pch = 16, cex = 0.5, asp = 1)
#' lines(rw, col = "grey", lwd = 0.5)
#' 
#' # Two-dimensional random walk with 8-way movement
#' rwr <- random_walk(3000, diagonal = FALSE)
#' plot(rwr, pch = 16, cex = 0.5, asp = 1)
#' lines(rwr, col = "grey", lwd = 0.5)
#' 
#' # Two-dimensional random walk with runs
#' rwr <- random_walk(3000, runs = 15)
#' plot(rwr, pch = 16, cex = 0.5, asp = 1)
#' lines(rwr, col = "grey", lwd = 0.5)

random_walk <- function(steps,
                        diagonal = FALSE, 
                        bias = c(0, 0), 
                        runs = 0, 
                        reach = 10) {
  moves <- data.table(expand.grid(x = -1:1, y = -1:1, tag = NA))
  if (diagonal == FALSE) {
    moves <- moves[(abs(x) + abs(y)) < 2]
  }
  trail <- moves[sample(.N, steps, replace = TRUE)]
  if (runs > 0) {
    trail$tag <- sample(1:round(steps / reach), steps, replace = TRUE)
    trail <- trail[order(tag)]
    for (i in 1:runs) {
      trail[tag %in% sample(1:round(steps / reach), 1)] <- moves[sample(.N, 1)]
    }
  }
  trail[sample(.N, round(steps * bias[1]), replace = TRUE), x := 1]
  trail[sample(.N, round(steps * bias[2]), replace = TRUE), y := 1]
  trail[, tag := NULL]
  trail[, x := cumsum(x)][, y := cumsum(y)]
}


