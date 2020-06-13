#' Hierarchical data visualisation (spacing version)
#' to print the first 2 levels + the 3rd level of selected node expanded out
#'
#'
#' @param tree a data tree object
#' @param x.name a character value indicating x variable
#' @param y.name a character value indicating y variable
#' @param color a character value indicating which variable to use for colouring
#' @param node.shown node selected in the 2nd hierarchy level to be expanded out
#' @param index labels size and tree branches (lines) thickness
#' @param aspect plot aspect ratio
#' @param ... ggplot functions and layers to be passed on
#
#' @return a svg file will be saved in the current working directory and automatically opened in a web broswer
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
#'
#'
#' # x and y variables
#' spacing.plot_selected(employee.tree, x.name ="Age", y.name ="MonthlyIncome", index = 2,
#'                     node.shown = "Sales", geom_point(), geom_smooth())
#' # only singe variable
#' spacing.plot_selected(employee.tree, x.name ="MonthlyIncome", index = 2,
#'                       node.shown = "Human Resources", geom_density(),
#'                       ggtitle("Monthly Income density plot"))
#' }
#'
#' @import grid
#' @importFrom grDevices svg dev.off
#' @importFrom utils browseURL
#'
#' @export
spacing.plot_selected = function(tree, x.name, node.shown, ...,
                                 y.name = NULL, index = 1, aspect = 1, color = NULL) {

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  p = plots_prep(tree, x.name, y.name, color, 3, index, ..., aspect = aspect)
  thenode = p[[3]][[node.shown]]

  svg(paste0(tree[["name"]], "_", node.shown, ".svg"),
      width = 3*3*4,
      height = max(length(thenode), length(p[[2]]))*6)

  pushViewport(viewport(layout=grid.layout(1, 8,
                                           widths = unit(c(0.5, 1, rep_len(c(0.25, 0.75, 1), 6)),
                                                         "null"))))

  y3 = seq(1-1/length(thenode)/2, 1/length(thenode)/2, len=length(thenode))
  y2 = seq(1-1/length(p[[2]])/2, 1/length(p[[2]])/2, len=length(p[[2]]))
  z = grep(node.shown, names(p[[2]]))

  pushViewport(viewport(layout.pos.col=8,
                        layout=grid.layout(length(thenode), 1)))
  sapply(1:length(thenode), function(j) {
    print(thenode[[j]], newpage= F, vp = vplayout(j,1))
  })

  popViewport()

  pushViewport(viewport(layout.pos.col=7))
  sapply(1:length(thenode), function(j) {
    branches(index = index, vp = viewport(y = y3[j]), selected = T)
    labels(label = names(thenode)[[j]],
           current.level = 3, no.nodes = length(thenode),
           index = index, selected = T,
           vp = viewport(x = 0.01, y = y3[j]))
  })
  popViewport()

  vertsticks(y = sort(c(y2[z], y3)), segement.index = length(thenode) + 1,
             selected = T, index = index, vp = vplayout(1,6))

  pushViewport(viewport(layout.pos.col=6))
  branches(index = index, selected = T, vp = viewport(y = y2[z]))
  popViewport()

  pushViewport(viewport(layout.pos.col=5))
  sapply(1:length(p[[2]]), function(j)
    print(p[[2]][[j]], newpage=F, vp = viewport(y = y2[j])))
  popViewport()

  pushViewport(viewport(layout.pos.col=4))
  sapply(1:length(p[[2]]), function(j) {
    branches(index = index, selected = ifelse(j == z, T, F),
             vp = viewport(y = y2[j]))
    labels(index = index, label = names(p[[2]])[[j]], current.level = 2,
           no.nodes = length(thenode), selected = ifelse(j == z, T, F),
           vp = viewport(x = 0.01, y = y2[j]))
  })
  popViewport()

  y1 = vertsticks(y = y2, segement.index = length(p[[2]]),
                  index = index, vp = vplayout(1,3))

  pushViewport(viewport(layout.pos.col=3))
  branches(index = index, vp = viewport(y = y1))
  popViewport()

  pushViewport(viewport(layout.pos.col=2))
  print(p[[1]], newpage= F, vp = viewport(y = y1))
  popViewport()

  pushViewport(viewport(layout.pos.col=1))
  branches(index = index, vp = viewport(y = y1))
  labels(label = tree[["name"]], no.nodes = length(thenode),
         index = index,
         vp = viewport(x = 0.01, y = y1))
  popViewport()

  dev.off()
  browseURL(paste0(getwd(),"/",tree[["name"]],"_",node.shown,".svg"))
}




#' Hierarchical data visualisation (spacing version)
#' to print the all 3 levels of nodes
#'
#'
#'
#' @param tree a data tree object
#' @param x.name a character value indicating x variable
#' @param y.name a character value indicating y variable
#' @param color a character value indicating which variable to use for colouring
#' @param levels.shown a numeric value indicating which level plot up to
#' @param index labels size and tree branches (lines) thickness
#' @param aspect plot aspect ratio
#' @param ... ggplot functions and layers to be passed on
#
#' @return a svg file will be saved in the current working directory and automatically opened in a web broswer
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
#'
#'
#' # x and y variables
#' spacing.plot_all(employee.tree, x.name ="Age", y.name ="MonthlyIncome",
#'                  levels.shown = 1, index = 3, geom_point())
#'
#' # only singe variable
#' spacing.plot_all(employee.tree, x.name ="MonthlyIncome", color = "EducationField",
#'                  index = 2, levels.shown = 1, geom_density())
#'
#' }
#'
#'
#' @import grid
#' @importFrom grDevices svg dev.off
#' @importFrom utils browseURL
#'
#'
#' @export
spacing.plot_all = function(tree, x.name, ..., y.name = NULL, index = 1,
                            levels.shown = min(3, tree[["height"]]), aspect = 1, color = NULL) {

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  if (levels.shown == 1) {
    svg(paste0(tree[["name"]], ".svg"),
        width = 1*3*4, height = 5)

    p = plots_prep(tree, x.name, y.name, color, 1, index, ..., aspect = aspect)

    pushViewport(viewport(layout=grid.layout(1, 2,
                                             widths = unit(c(0.5, 1), "null"))))

    pushViewport(viewport(layout.pos.col=1))
    branches(index = index)
    labels(label = tree[["name"]],
           index = index, vp = viewport(x = 0.01))
    popViewport()

    print(p[[1]], newpage = F, vp = vplayout(1,2))
  }

  else if (levels.shown == 2) {
    p = plots_prep(tree, x.name, y.name, color, 2, index, ..., aspect = aspect)

    svg(paste0(tree[["name"]], ".svg"),
        width = 2*3*4, height = length(p[[2]])*5)

    pushViewport(viewport(layout=grid.layout(1, 5,
                                             widths = unit(c(0.5, 1, c(0.25, 0.75, 1)),
                                                           "null"))))
    y2 = seq(1-1/length(p[[2]])/2, 1/length(p[[2]])/2, len=length(p[[2]]))

    pushViewport(viewport(layout.pos.col=5))
    sapply(1:length(p[[2]]), function(j)
      print(p[[2]][[j]], newpage=F, vp = viewport(y = y2[j])))
    popViewport()

    pushViewport(viewport(layout.pos.col=4))
    sapply(1:length(p[[2]]), function(j) {
      branches(index = index, vp = viewport(y = y2[j]))
      labels(index = index, label = names(p[[2]])[[j]], current.level = 2,
             no.nodes = length(p[[2]]),
             vp = viewport(x = 0.01, y = y2[j]))
    })
    popViewport()

    y1 = vertsticks(y = y2, segement.index = length(p[[2]]),
                    index = index, vp = vplayout(1,3))


    pushViewport(viewport(layout.pos.col=3))
    branches(index = index, vp = viewport(y = y1))
    popViewport()

    pushViewport(viewport(layout.pos.col=2))
    print(p[[1]], newpage= F, vp = viewport(y = y1))
    popViewport()

    pushViewport(viewport(layout.pos.col=1))
    branches(index = index, vp = viewport(y = y1))
    labels(label = tree[["name"]], no.nodes = length(p[[2]]),
           index = index,
           vp = viewport(x = 0.01, y = y1))
    popViewport()

  }

  else {
    p = plots_prep(tree, x.name, y.name, color, levels.shown, index, ..., aspect = aspect)

    svg(paste0(tree[["name"]], ".svg"),
        width = 4*3*4,
        height = sum(sapply(p[[3]], length))*5)

    pushViewport(viewport(layout=grid.layout(1, 10,
                                             widths = unit(c(0.5, 1, rep_len(c(0.25, 0.75, 1), 6), 1),
                                                           "null"))))

    temp.unlist = unlist(p[[3]], recursive = F)
    y3 = seq(1-1/length(temp.unlist)/2, 1/length(temp.unlist)/2, len=length(temp.unlist))

    pushViewport(viewport(layout.pos.col=8,
                          layout=grid.layout(length(temp.unlist), 1)))
    sapply(1:length(temp.unlist), function(j) {
      print(temp.unlist[[j]], newpage= F, vp = vplayout(j,1))
    })

    popViewport()

    pushViewport(viewport(layout.pos.col=7))
    sapply(1:length(temp.unlist), function(j) {
      branches(index = index, vp = viewport(y = y3[j]))
      labels(label = unlist(sapply(p[[3]], names))[[j]],
             current.level = 3, no.nodes = length(temp.unlist),
             index = index,
             vp = viewport(x = 0.01, y = y3[j]))
    })
    popViewport()

    y2 = vertsticks(y = y3, segement.index = sapply(p[[3]], length),
                    index = index, vp = vplayout(1,6))

    pushViewport(viewport(layout.pos.col=6))
    sapply(1:length(p[[2]]), function(j)
      branches(index = index, vp = viewport(y = y2[j])))
    popViewport()

    pushViewport(viewport(layout.pos.col=5))
    sapply(1:length(p[[2]]), function(j)
      print(p[[2]][[j]], newpage=F, vp = viewport(y = y2[j])))
    popViewport()

    pushViewport(viewport(layout.pos.col=4))
    sapply(1:length(p[[2]]), function(j) {
      branches(index = index, vp = viewport(y = y2[j]))
      labels(index = index, label = names(p[[2]])[[j]], current.level = 2,
             no.nodes = length(temp.unlist),
             vp = viewport(x = 0.01, y = y2[j]))
    })
    popViewport()

    y1 = vertsticks(y = y2, segement.index = length(p[[2]]),
                    index = index, vp = vplayout(1,3))

    pushViewport(viewport(layout.pos.col=3))
    branches(index = index, vp = viewport(y = y1))
    popViewport()

    pushViewport(viewport(layout.pos.col=2))
    print(p[[1]], newpage= F, vp = viewport(y = y1))
    popViewport()

    pushViewport(viewport(layout.pos.col=1))
    branches(index = index, vp = viewport(y = y1))
    labels(label = tree[["name"]], no.nodes = length(temp.unlist),
           index = index,
           vp = viewport(x = 0.01, y = y1))
    popViewport()

  }
  dev.off()
  browseURL(paste0(getwd(),"/",tree[["name"]], ".svg"))
}


