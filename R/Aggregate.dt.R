#' Aggregate attribute values throughout the entire tree recursively
#'
#' @description This function aggregates the selected attribute values of each node to its corresponding parent node using customised function, upto the root node.
#'
#'
#' @param tree the data tree object
#' @param ... the attribute variables to be aggregated for each node
#' @param aggfun the aggregation function to be applied to each node's attributes
#' @param traversal traversal order:
#' any of 'pre-order' (the default), 'post-order', 'in-order', 'level', 'ancestor', or a custom function
#'
#'
#' @examples
#' \dontrun{
#' # create a data tree object
#' data(employee)
#' employee.tree = createTree(employee, "Employee Information",
#'                            c("Department", "JobRole", "EmployeeNumber"))
#'
#' # aggregate the mean of monthly income for each node recursively
#' # to its corresponding parent node in post-order, upto the root node
#' Aggregate.dt(employee.tree, "MonthlyIncome",
#'              aggfun = function(x) mean(x, na.rm = TRUE),
#'              traversal = "post-order")
#'}
#'
#'
#' @export
#' @import data.tree
Aggregate.dt = function(tree, ..., aggfun,
                        traversal = c("pre-order", "post-order", "in-order",
                                      "level", "ancestor")){
  nodes = Traverse(tree, traversal = traversal)
  Do(nodes, function(node) {
    sapply(list(...), function(x)
      node[[x]] = Aggregate(node, aggFun = aggfun, attribute = x))
  })
}
