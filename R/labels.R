#' Draw the node names on each branch
#'
#' @param label label to be drawn as the name of node
#' @param current.level current hierarchy level we are drawing at
#' @param no.nodes number of nodes in the current level
#' @param index labels size
#' @param selected whether the vertical branch is connecting to a selected node
#' @param ... other arguments passed on to the function
#'
#'
#' @import grid
#'
#'
labels = function(label, current.level = 1, no.nodes = 1, index, selected = F, ...) {
  grid.text(label,
            x = 0.5,
            y = 0.5 + 0.005/no.nodes,
            gp=gpar(fontsize = (10 + 1/current.level)*(1+0.1*index) + selected,
                    cex = 1.5*(1+0.1*index), col = ifelse(selected, "red", "black")),
            just = c("left","bottom"), ...)
}
