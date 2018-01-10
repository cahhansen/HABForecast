#' Create a HAB panel model (econometric model across cross-sections). Cross-sections could be different bodies of water (for a more regional approach) or locations within the same body of water (for a more localized approach).
#'
#' @param df A data frame
#' @param hab.name Name of the column for the measure of HAB (proxy measure of HAB, for example: cell count, chl-a concentration, phycocyanin concentration, etc.)
#' @param vars A vector of parameters (independent variables) to use in building the panel model
#' @param panel.unit Name of the column which specifies the cross-sectional unit (for example: location ID)
#' @return Panel model and summary of panel model
#' @examples
#' @import plm
#' @export

habpanelmod <- function(df,hab.name,vars,panel.unit){
  #Remove any incomplete cases
  df <- na.omit(df)
  #Format for panel analysis
  df.format <- pdata.frame(x=df,index=panel.unit)
  #Create panel model
  mod <- plm::Within(df.format)
  #Return results
  return(mod)
}

