#' Horizontal branches connecting up two levels
#'
#'
#' @param index the tree branch (line) size index
#' @param selected whether the tree branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#'
#' @import grid
#'
#'
branches = function(index, selected = F, ...){
  grid.draw(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                         gp=gpar(lwd = 3*(1+0.1*index) + selected*3,
                                 col = ifelse(selected, "red", "black")), ...))
}


#' Horizontal branches connecting up two levels --- slight shifted version
#'
#' @param x0 x coordinate of where the tree branch starts
#' @param x1 x coordinate of where the tree branch ends
#' @param index the tree branch (line) size index
#' @param ... other arguments passed on to the function
#'
#'
#' @import grid
#'
branches.shifted = function(x0 = 0, x1 = 1, index, ...){
  grid.draw(segmentsGrob(x0 = x0, x1 = x1, y0 = 0.5, y1 = 0.5,
                         gp=gpar(lwd = 3*(1+0.1*index),
                                 col = "black"), ...))
}


#' Vertical branches connecting up two levels in the middle
#'
#'
#' @param y y coordinates for each node in a vector
#' @param segement.index indices in y of where to draw the vertical tree branches
#' @param index tree branches (lines) size index
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
