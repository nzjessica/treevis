#' Convert a data tree object to a long format data frame
#'
#' @param tree the data tree object
#' @param ... the attributes to be added as columns in the data frame
#'
#' @return A long format data frame
#'
#' @examples
#' \dontrun{
#' # Convert the employee.tree to a long format data frame with Age and Education attribute variables
#' tree_to_df(employee.tree, "Age", "Education")
#'}
#'
#'
#' @export
tree_to_df = function(tree, ...) {
  vars = c(...)
  list1 = as.list(tree)[-1]
  list2 = rlist::list.flatten(list1, use.names = T)

  attributes = lapply(vars, function(x) {
    unlist(list2[grep(names(list2),
                      pattern = paste0("\\b",x,"\\b"), perl = T)],
           use.names = T)
  })
  names(attributes) = vars

  levels = lapply(strsplit(names(attributes[[1]]), split = "[.]"),
                  function(x) x[-c(length(x))])
  levels.df = as.data.frame(t(as.data.frame(levels)), stringsAsFactors = F)
  colnames(levels.df) = paste0("level_", seq(levels.df)+1)
  rownames(levels.df) = seq(nrow(levels.df))

  return(data.frame(level_1 = tree$name, levels.df, attributes, stringsAsFactors = F))
}


