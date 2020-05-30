#' Print out the tree with selected attribute variables up to a certain level.
#'
#'
#' @param tree the data tree object to be printed
#' @param levels.shown a numeric value indicating which level print up to
#' @param attribute the selected attribute variable to be printed
#' @param ... other attribute variables to be passed on
#'
#'
#' @examples
#' \dontrun{
#' data(employee)
#' employee.tree = createTree(employee, "Employee Information",
#'                            c("Department", "JobRole", "EmployeeNumber"))
#'
#' # display the employee.tree with selected attribute variables up to the third level
#' display.dt(employee.tree, 3, "Age", "MonthlyIncome", "Attrition")
#'}
#'
#'
#' @import data.tree
#' @export
display.dt = function(tree,
                      levels.shown,
                      attribute, ...){
  if (levels.shown == 1) {
    root.nodes = Traverse(tree, filterFun = isRoot)
    root.df = do.call(data.frame, lapply(list('levelName', attribute, ...),
                                         function(x) Get(root.nodes, x)))
    colnames(root.df) <- c("levelName", attribute, ...)
    rownames(root.df) <- NULL
    print(root.df)
  }
  else if (levels.shown == tree[["height"]] - 1) {
    temp.nodes = Traverse(tree, filterFun = isNotLeaf)
    temp.df = do.call(data.frame, lapply(list('levelName', attribute, ...),
                                         function(x) Get(temp.nodes, x)))
    colnames(temp.df) <- c("levelName", attribute, ...)
    print(temp.df)
  }
  else if (levels.shown < tree[["height"]]) {
    temp.nodes = Traverse(tree, filterFun = function(x) x[["level"]] <= levels.shown)
    temp.df = do.call(data.frame, lapply(list('levelName', attribute, ...),
                                         function(x) Get(temp.nodes, x)))
    colnames(temp.df) <- c("levelName", attribute, ...)
    rownames(temp.df) <- NULL
    print(temp.df)
  }
  else print(tree, attribute, ...)
}



