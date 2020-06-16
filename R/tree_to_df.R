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
#' @importFrom data.tree isLeaf
#'
#' @export
tree_to_df = function(tree, ...) {
  vars = c(...)
  if (is.null(vars)) {
    levels.df = as.data.frame(t(tree$Get("path", filterFun = isLeaf)),
                              stringsAsFactors = F, row.names = F)
    colnames(levels.df) = paste0("level_", seq(levels.df))
    return(levels.df)
  }

  attributes = lapply(vars, function(x) {
    tree$Get(x, filterFun = isLeaf)
  })
  names(attributes) = vars

  levels.df = as.data.frame(t(tree$Get("path", filterFun = isLeaf)),
                            stringsAsFactors = F, row.names = F)
  colnames(levels.df) = paste0("level_", seq(levels.df))

  return(data.frame(levels.df, attributes))
}

