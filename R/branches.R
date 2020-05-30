#' Function to draw branch for each node
#'
#'
#' @param index tree branches (lines) index
#' @param selected whether the vertical branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#
#' @import grid
#'
#'
branches = function(index, selected = F, ...){
  grid.draw(segmentsGrob(x0 = 0, x1 = 1, y0 = 0.5, y1 = 0.5,
                         gp=gpar(lwd = 3*(1+0.1*index) + selected*3,
                                 col = ifelse(selected, "red", "black")), ...))
}


#' Function to draw branch for each node --- slight shifted version
#'
#' @param x0 x0 coordinate
#' @param x1 x1 coordinate
#' @param index tree branches (lines) index
#' @param selected whether the vertical branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#
#' @import grid
#'
branches.shifted = function(x0 = 0, x1 = 1, index, selected = F, ...){
  grid.draw(segmentsGrob(x0 = x0, x1 = x1, y0 = 0.5, y1 = 0.5,
                         gp=gpar(lwd = 3*(1+0.1*index) + selected*3,
                                 col = ifelse(selected, "red", "black")), ...))
}
