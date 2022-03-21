#' @title Computing composite reliability index from a multilevel confirmatory factor analysis model
#' @param fit = multilevel CFA model fitted with lavaan
#' @param level = level of interest (either 1 or 2)
#' @param items = numeric string indicating the items of interest (e.g., 1:3 for items 1, 2 and 3)
#' @param item.labels = character string indicating the names of the items of interest
MCFArel <- function(fit,level,items,item.labels){ require(lavaan)
  if(level==1){ 
    sl <- standardizedsolution(fit)[1:(nrow(standardizedSolution(fit))/2),] # pars within
  } else if(level==2){ 
    sl <- standardizedsolution(fit)[(nrow(standardizedSolution(fit))/2):nrow(standardizedsolution(fit)),] # pars between
  } else { stop("Error: level can be either 1 or 2") }
  sl <- sl$est.std[sl$op == "=~"][items] # standardized loadings of the selected items
  names(sl) <- item.labels # item names
  re <- 1 - sl^2 # residual variances of items
  
  omega <- sum(sl)^2 / (sum(sl)^2 + sum(re)) # composite reliability index
  return(round(omega,2))}