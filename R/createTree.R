#' Create data tree object from a long format data frame
#'
#' @param df data frame in the long data format
#' @param root_name a character specifing the root name for the whole hierarchy tree
#' @param hierarchy_order a vector of hierarchy level names in ascending order
#'
#' @return An active-binding data tree object
#'
#'
#' @examples
#' \dontrun{
#' # load the employee data
#' data(employee)
#'
#' # create a data tree object from the employee dataset with a hierarchy order of:
#' # Employee Information -> Department -> JobRole -> EmployeeNumber
#' employee.tree = createTree(employee, "Employee Information",
#'            c("Department", "JobRole", "EmployeeNumber"))
#' }
#'
#'
#' @export
#' @import data.tree
createTree <- function(df, root_name, hierarchy_order){
  df[["pathString"]] = do.call(paste, c(root_name, df[hierarchy_order], list(sep = "/")))
  df.tree = as.Node(df, check = "no-warn")
  return(df.tree)
}
