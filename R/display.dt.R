#' Print out the tree with selected attribute variables up to a certain level.
#'
#'
#' @param tree the data tree object to be printed
#' @param ... attribute variables or arguments to be passed on
#' @param levels.shown a numeric value indicating which hierarchy level of the tree to print up to
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
#' @importFrom data.tree Get Traverse isRoot isNotLeaf
#'
#' @export
display.dt = function(tree, ..., levels.shown = tree[["height"]]){
  if (levels.shown == 1) {
    root.nodes = Traverse(tree, filterFun = isRoot)
    root.df = do.call(data.frame, lapply(list('levelName', ...),
                                         function(x) Get(root.nodes, x)))
    colnames(root.df) <- c("levelName", ...)
    rownames(root.df) <- NULL
    print(format(root.df, justify = "left"))
  }
  else if (levels.shown == tree[["height"]] - 1) {
    temp.nodes = Traverse(tree, filterFun = isNotLeaf)
    temp.df = do.call(data.frame, lapply(list('levelName', ...),
                                         function(x) Get(temp.nodes, x)))
    colnames(temp.df) <- c("levelName", ...)
    print(format(temp.df, justify = "left"))
  }
  else if (levels.shown < tree[["height"]]) {
    temp.nodes = Traverse(tree, filterFun = function(x) x[["level"]] <= levels.shown)
    temp.df = do.call(data.frame, lapply(list('levelName', ...),
                                         function(x) Get(temp.nodes, x)))
    colnames(temp.df) <- c("levelName", ...)
    rownames(temp.df) <- NULL
    print(format(temp.df, justify = "left"))
  }
  else print(tree, ...)
}


