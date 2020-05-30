#' Expand all factors into multiple dummy variables whilst keeping all other variables in the df
#'
#'
#' @description Expand all factors (categorical variables) into multiple dummy variables whilst keeping all other variables in the df
#' (if the factor only contain two levels then the factor will be turned into one dummy variable with only 1/0 instead of unnecessary expanding.)
#'
#' @param dataframe a data frame containing categorical variables for expansion
#'
#' @return A data frame
#'
#' @examples
#' ### An example dataset
#' customers <- data.frame(
#'   id=c(10,20,30,40,50,60,70),
#'   gender=c('male','female','female','male','female', 'female', 'male'),
#'   mood=c('happy','sad','happy','sad','happy', "neutral", "neutral"),
#'   outcome=c(1,1,0,0,0,0,1))
#'
#' categorical.split(customers)
#'
#'
#'
#' @export
categorical.split = function(dataframe) {
  allvars = names(dataframe)
  for (x in allvars) {
    if (length(levels(dataframe[[x]])) == 2) {
      newname = paste0(x, "_", levels(dataframe[[x]])[1])
      dataframe[[newname]] = as.numeric(as.character(factor(dataframe[[x]], labels = 1:0)))
      dataframe[[x]] <- NULL
    }
    else
      if (length(levels(dataframe[[x]])) >= 2) {
        morecolumn = paste0(x, "_", levels(dataframe[[x]]))
        for (y in seq_along(morecolumn)) {
          columname = morecolumn[y]
          element = levels(dataframe[[x]])[y]
          logical.vector = dataframe[[x]] == element
          dataframe[[columname]] = ifelse(factor(logical.vector) == "TRUE", 1, 0)
        }
        dataframe[[x]] <- NULL
      }
  }
  return(dataframe)
}

