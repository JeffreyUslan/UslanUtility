#' Provids a table of variable classes
#'
#' @param dataset a dataset to evaluate
#' @examples
#' data(iris)
#' class_table(iris)
#'
#'


class_table=function(dataset){
  class_types=NULL
  class_table=list()
  for (i in 1:ncol(dataset)){
    class_hold=class(dataset[,i])
    class_match=class_hold %in% class_types
    if (!class_match){
      class_types=c(class_types,class_hold)
      class_table[[class_hold]]=names(dataset)[i]
    } else {
      class_table[[class_hold]]=c((class_table[[class_hold]]),names(dataset)[i])
    }
    
    
  }
  
  return(class_table)
}