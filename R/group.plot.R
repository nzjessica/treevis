#' Hierarchical data visualisation (space-saving version)
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
#' group.plot_selected(employee.tree, x.name ="Age", y.name ="MonthlyIncome", index = 2,
#'                     node.shown = "Sales", aspect = 3/5, geom_point(), geom_smooth(model = lm))
#'
#' # only singe variable
#' group.plot_selected(employee.tree, x.name ="MonthlyIncome", color = "EducationField", index = 2,
#'                     node.shown = "Human Resources", geom_density())
#' }
#'
#'
#' @import grid
#' @importFrom grDevices svg dev.off
#' @importFrom utils browseURL
#'
#'
#' @export
group.plot_selected = function(tree, x.name, node.shown, ...,
                               y.name = NULL, index = 1, aspect = NULL, color = NULL) {

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  p = plots_prep(tree, x.name, y.name, color, 3, index, ..., aspect = aspect)
  thenode = p[[3]][[node.shown]]

  svg(paste0(tree[["name"]], "_", node.shown, ".svg"),
      width = (3*2 + 1.5)*4,
      height = max(length(thenode), length(p[[2]]))*6)

  pushViewport(viewport(layout=grid.layout(1, 5,
                                           widths = unit(c(0.5, 1, 0.25, 0.25, 1), "null"))))

  z = grep(node.shown, names(p[[2]]))
  y = seq(1-(1/length(p[[2]])/2), 1/length(p[[2]])/2, by = -1/length(p[[2]]))
  total_no = max(length(p[[2]]), z-1+length(thenode))

  pushViewport(viewport(layout.pos.col=5,
                        layout=grid.layout(total_no, 1)))
  sapply(z:(z-1+length(thenode)), function(j) {
    print(thenode[j-z+1], newpage=F, vp = vplayout(j,1))
    grid.text(names(thenode)[j-z+1], x = 0.01, y = 1,
              gp=gpar(fontsize = (10 + 1)*(1+0.1*index),
                      cex = 1.5*(1+0.1*index), col = "red"),
              just = c("left","top"), vp = vplayout(j,1))
    grid.rect(vp = vplayout(j,1), gp = gpar(fill='transparent'))
  })

  grid.rect(vp = vplayout(z:(z-1+length(thenode)), 1),
            gp = gpar(fill='transparent', col = "red",
                      lwd = 3*(1+0.1*index)))

  popViewport()

  pushViewport(viewport(layout.pos.col=3:4,
                        layout=grid.layout(length(p[[2]]), 1)))
  branches(index = index, vp = vplayout(z,1), selected = T)

  popViewport()

  pushViewport(viewport(layout.pos.col=2,
                        layout=grid.layout(length(p[[2]]), 1)))
  sapply(1:length(p[[2]]), function(j) {
    print(p[[2]][j], newpage=F, vp = vplayout(j,1))
    grid.text(names(p[[2]])[j], x = 0.01, y = 1,
              gp=gpar(fontsize = (10 + 1)*(1+0.1*index),
                      cex = 1.5*(1+0.1*index)),
              just = c("left","top"), vp = vplayout(j,1))
    grid.rect(vp = vplayout(j,1), gp = gpar(fill='transparent'))
  })
  grid.text(names(p[[2]])[z], x = 0.01, y = 1,
            gp=gpar(fontsize = (10 + 1)*(1+0.1*index),
                    cex = 1.5*(1+0.1*index), col = "red"),
            just = c("left","top"), vp = vplayout(z,1))
  grid.rect(vp = vplayout(z, 1),
            gp = gpar(fill='transparent', col = "red",
                      lwd = 3*(1+0.1*index)))

  popViewport()

  pushViewport(viewport(layout.pos.col=1))
  branches(index = index)
  labels(label = tree[["name"]], index = index, vp = viewport(x = 0.01))
  popViewport()

  dev.off()
  browseURL(paste0(getwd(),"/",tree[["name"]],"_",node.shown,".svg"))
}



#' Hierarchical data visualisation (space-saving version)
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
#' group.plot_all(employee.tree, x.name ="Age", y.name ="MonthlyIncome",
#'                color = "EducationField", index = 2,
#'                levels.shown = 3, aspect = 3/4, geom_point(), geom_smooth())
#'
#' # only singe variable
#' group.plot_all(employee.tree, x.name ="MonthlyIncome",
#'                color = "Gender", index = 2,
#'                levels.shown = 1, geom_density())
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
group.plot_all = function(tree, x.name, ..., y.name = NULL,
                          index = 1, levels.shown = 3, aspect = NULL, color = NULL){

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  if (levels.shown == 1 | levels.shown == 2) {
    p = plots_prep(tree, x.name, y.name, color, 2, index, ..., aspect = aspect)

    svg(paste0(tree[["name"]], ".svg"),
        width = (1*2 + 1.5)*4,
        height = length(p[[2]])*5)

    pushViewport(viewport(layout=grid.layout(1, 2,
                                             widths = unit(c(0.5, 1), "null"))))

    pushViewport(viewport(layout.pos.col=2,
                          layout=grid.layout(length(p[[2]]), 1)))
    sapply(1:length(p[[2]]), function(j) {
      print(p[[2]][j], newpage=F, vp = vplayout(j,1))
      grid.text(names(p[[2]])[j], x = 0.01, y = 1,
                gp=gpar(fontsize = (10 + 1)*(1+0.1*index),
                        cex = 1.5*(1+0.1*index)),
                just = c("left","top"), vp = vplayout(j,1))
      grid.rect(vp = vplayout(j,1), gp = gpar(fill='transparent'))
    })
    popViewport()

    pushViewport(viewport(layout.pos.col=1))
    branches(index = index)
    labels(label = tree[["name"]],
           index = index, vp = viewport(x = 0.01))
    popViewport()

  }

  else {
    p = plots_prep(tree, x.name, y.name, color, 3, index, ..., aspect = aspect)

    svg(paste0(tree[["name"]], ".svg"),
        width = (3*2 + 1.5)*4,
        height = sum(sapply(p[[3]], length))*5)

    pushViewport(viewport(layout=grid.layout(1, 4,
                                             widths = unit(c(0.5, 1, 0.5, 1), "null"))))

    temp.unlist = unlist(p[[3]], recursive = F)

    total_no = length(temp.unlist)+length(p[[3]])-1
    no_nodes = sapply(p[[3]], length)
    index0 = cumsum(sapply(p[[3]], length)+1)-1
    index_used = seq(total_no)[!seq(total_no) %in% (index0+1)]

    y2 = no_nodes/total_no/2 + (1-index0/total_no)
    middle_index = ceiling(length(p[[2]])/2)
    y1 = (middle_index-1:length(p[[2]]))* 1/total_no+y2[middle_index]
    x = abs((middle_index-1:length(p[[2]])) * 0.05)

    pushViewport(viewport(layout.pos.col=4,
                          layout=grid.layout(total_no, 1)))
    sapply(index_used, function(j) {
      print(j)
      i = which(j == index_used)
      print(temp.unlist[i], newpage=F, vp = vplayout(j,1))
      grid.text(unlist(sapply(p[[3]], names))[i], x = 0.01, y = 1,
                gp=gpar(fontsize = (10 + 1/2)*(1+0.1*index),
                        cex = 1.5*(1+0.1*index)),
                just = c("left","top"), vp = vplayout(j,1))
      grid.rect(vp = vplayout(j,1), gp = gpar(fill='transparent'))
    })

    popViewport()

    pushViewport(viewport(layout.pos.col=3))
    sapply(1:length(p[[2]]), function(j) {
      branches.shifted(x0 = 0.5-x[j], index = index, vp = viewport(y = y2[j]))
    })

    grid.draw(segmentsGrob(x0 = 0.5-x, x1 = 0.5-x, y0 = y2,
                           y1 = y1, gp=gpar(lwd = 3*(1+0.1*index))))


    sapply(1:length(p[[2]]), function(j) {
      branches.shifted(x1 = 0.5-x[j], index = index, vp = viewport(y = y1[j]))
    })
    popViewport()

    a = seq(min(y1)-(abs(diff(y1))[1]/2), max(y1)+(abs(diff(y1))[1]/2), len=length(y1))
    pushViewport(viewport(layout.pos.col=2,
                          layout=grid.layout(3, 1,
                                             heights = unit(c((total_no-((a[1]/(1/total_no))+length(p[[2]])))/total_no,
                                                              length(p[[2]])/total_no,
                                                              (a[1]/(1/total_no))/total_no), "null"))))
    pushViewport(viewport(layout.pos.row = 2,
                          layout=grid.layout(length(p[[2]]), 1)))
    sapply(1:length(p[[2]]), function(j) {
      print(p[[2]][j], newpage=F, vp = vplayout(j,1))
      grid.text(names(p[[2]])[j], x = 0.01, y = 1,
                gp=gpar(fontsize = (10 + 1/2)*(1+0.1*index),
                        cex = 1.5*(1+0.1*index)),
                just = c("left","top"), vp = vplayout(j,1))
      grid.rect(vp = vplayout(j,1), gp = gpar(fill='transparent'))
    })
    popViewport()
    popViewport()

    pushViewport(viewport(layout.pos.col=1))
    branches(index = index, vp = viewport(y = y1[middle_index]))
    labels(label = tree[["name"]], index = index+1,
           vp = viewport(x = 0.01, y = y1[middle_index]))
    popViewport()

  }

  dev.off()
  browseURL(paste0(getwd(),"/",tree[["name"]], ".svg"))
}




