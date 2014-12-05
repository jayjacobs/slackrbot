#'Returns a random crowdstrike codename
#'
#' Given a country of origin for the threat actor (optional)
#' This will generate a random crowdstrike actor name
#' and return the string
#' @param country of origin
#' 
crowdstrike <- function(inCountry) {
  if( length(inCountry) > 1) {
    return("Crowdstrike nickname for a named country")
  }
  else {
    return("Random crowdstrike nickanme")
  }
}