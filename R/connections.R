#' Horizontal branches connecting up two levels
#'
#'
#' @param index index of the middle connection branches
#' @param selected whether the connection branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#'
#' @import grid
#'
#'
connection = function(index, selected = F, ...) {
  midstick = segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                          gp=gpar(lwd = 3*(1+0.1*index) + selected*3,
                                  col = ifelse(selected, "red", "black")), ...)
  grid.draw(midstick)
}


#' Vertical branches connecting up two levels in the middle
#'
#'
#' @param y y coordinates for each node in a vector
#' @param segement.index indices in y of where to draw the vertical tree branches
#' @param index tree branches (lines) index
#' @param selected whether the vertical branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#'
#' @import grid
#'
#'
vertsticks = function(y, segement.index, index, selected = F, ...) {
  y0 = y[cumsum(segement.index)-segement.index+1]
  y1 = y[cumsum(segement.index)]
  grid.draw(segmentsGrob(x0 = 1, x1 = 1, y0 = y0,
                         y1 = y1, gp=gpar(lwd = 3*(1+0.1*index) + selected*3,
                                          col = ifelse(selected, "red", "black")),...))
  return((y0+y1)/2)
}

