##' Evaluate strength of correlation between HAB measure and other factor
#'
#' @param hab.data A data frame with columns "Date" (must be of class Date) and a measure of HAB (proxy measure of HAB, for example: cell count, chl-a concentration, phycocyanin concentration, etc.)
#' @param hab.name Name of the column for the measure of HAB
#' @param var.data A data frame with columns "Date" (must be of class Date) and some other factor to evaluate
#' @param var.name Name of the column for the parameter or factor
#' @return The correlation (using non-parametric Spearman method) between the HAB measure and another variable
#' @examples
#' @import dplyr
#' @export

habcorr <- function(hab.data,hab.name=NULL,var.data,var.name=NULL){
  #Format data frames given the user inputs
  if(is.null(hab.name)){
    if("Value" %in% colnames(hab.data)){
      hab.data <- hab.data[,c("Date","Value")]
      hab.name <- "Measure of HAB"
    }
      print("Please provide name of HAB measure")
  }else{
    hab.data <- hab.data[,c("Date",hab.name)]

  }

  if(is.null(var.name)){
    if("Value" %in% colnames(var.data)){
      var.data <- var.data[,c("Date","Value")]
      var.name <- "Other Parameter"
    }
    print("Please provide name of a parameter to evaluate")
  }else{
    var.data <- var.data[,c("Date",var.name)]

  }

  names(hab.data) <- c("Date","HABvalue")
  names(var.data) <- c("Date","Varvalue")

  #Join HABs data frame and other variable data frame
  all.data <- dplyr::left_join(hab.data,var.data,by='Date')
  #Remove any incomplete cases
  all.data <- na.omit(all.data)
  #Calculate correlation between the two parameters
  result <- cor(all.data$HABvalue,all.data$Varvalue,method="spearman")
  #Output correlation
  if(!is.null(result)){
    return(result)
  }
  #Plot scatterplot
  plot(data=all.data,x=HABvalue,y=Varvalue,type = "p",xlab=hab.name,ylab=var.name)
}
