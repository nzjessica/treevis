#' Split a dataframe and execute a function on each splited group
#'
#' @param df a dataframe
#' @param by variables to split data frame by
#' @param fun function to be executed on each splited group
#' @param ... other arguments passed on to the function
#'
#'
#' @importFrom  plyr dlply
#'
#'
nested.dlply <- function(df, by, fun, ...) {
  if (length(by) == 1) {
    dlply(df, by, fun, ...)
  } else {
    dlply(df, by[1], nested.dlply, by[-1], fun, ...)
  }
}


#' Generate ggplot objects for a certain level in the tree
#'
#'
#' @param df a dataframe
#' @param level which level to be plotted
#' @param x.name x variable
#' @param y.name y variable
#' @param color variable to be coloured by
#' @param index labels size and tree branches (lines) thickness
#' @param aspect plot aspect ratio
#' @param ... ggplot functions and layers to be passed on
#'
#' @import ggplot2
#'
plot_prep = function(df, level, x.name, y.name, color, index, aspect, ...) {
  if (level == 1) {
    return(
      if (is.null(y.name)) {
        ggplot(df, aes_string(x.name, color = color, na.rm = T)) + theme_bw() +
          theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
                plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...)
      }
      else ggplot(df, aes_string(x.name, y.name, color = color, na.rm = T)) + theme_bw() +
        theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
              plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...))
  }
  if (level == 2) return(nested.dlply(df, "level_2", function(z) {
    if (is.null(y.name)) {
      ggplot(z, aes_string(x.name, color = color, na.rm = T)) + theme_bw() +
        theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
              plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...)
    }
    else ggplot(z, aes_string(x.name, y.name, color = color, na.rm = T)) + theme_bw() +
      theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
            plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...)
  }))
  else {
    vars = names(df)
    levelnames = vars[grep(paste0("level_", level-1),
                           vars):grep(paste0("level_",level), vars)]
    return(nested.dlply(df, levelnames, function(z) {
      if (is.null(y.name)) {
        ggplot(z, aes_string(x.name, color = color, na.rm = T)) + theme_bw() +
          theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
                plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...)
      }
      else ggplot(z, aes_string(x.name, y.name, color = color, na.rm = T)) + theme_bw() +
        theme(aspect.ratio = aspect, axis.text = element_text(size = 8+(index-1)*2.5),
              plot.title = element_text(hjust = 1, size = 8+(index-1)*2.5)) + list(...)
    }))
  }

}



#' A nested list of ggplot objects
#'
#'
#'
#' @param tree a data tree object
#' @param x.name x variable
#' @param y.name y variable
#' @param color variable to be coloured by
#' @param levels.shown which level to be shown
#' @param index labels size and tree branches (lines) thickness
#' @param aspect plot aspect ratio
#' @param ... ggplot functions and layers to be passed on
#
#' @return a nested list of ggplot objects in a tree-structure
#'
#'
#' @export
plots_prep = function(tree, x.name, y.name, color, levels.shown, index, aspect, ...) {
  df = tree_to_df(tree, x.name, y.name, color)
  a = lapply(1:levels.shown, function(z) plot_prep(df, level = z,
                                                   x.name, y.name, color, index, aspect, ...))
  names(a) = paste0("p",1:levels.shown)

  if (levels.shown > 3) {
    a[[4]] <- a[[4]][unlist(sapply(a[[3]], function(j) names(j)))]
  }
  return(a)
}
