#' @title Fit indices from mono- and multi-level confirmatory factor analysis models
#' @param model = CFA model(s) fitted with lavaan (multiple models sholud be added as a list)
#' @param from_summary = logical value indicating whether fit indices should be taken from model summary (default: FALSE) as some fit indices are only available from the model summary
#' @param type = character value indicating whether the CFA model is "multilevel" (default) or "monolevel"
#' @param models.names = character vector indicating the name(s) of the model(s) to be printed
#' @param fits = fit indices to be printed (see ?lavaan)
fit.ind <- function(models=NA,from_summary=FALSE,type="multilevel",models.names=NA,
                    fits=c("npar","chisq","df","pvalue","rmsea","cfi","srmr_within","srmr_between")){ require(lavaan); require(MuMIn)
  
  # removing level-specific fit indices when model is "monolevel"
  if(type=="monolevel"){
    fits <- gsub("srmr_within","srmr",fits)
    fits <- fits[fits!="srmr_between"] }
  if(from_summary==FALSE){
    # returning dataframe of models fit indices when more than one model is considered
    if(length(models)>1){
      fit.indices <- fitmeasures(models[[1]])[fits]
      for(i in 2:length(models)){
        fit.indices <- rbind(fit.indices,fitmeasures(models[[i]])[fits]) }
      if(!is.na(models.names[1])){
        row.names(fit.indices) <- models.names }
      return(as.data.frame(fit.indices))
    } else { return(fitmeasures(models)[fits]) }
    
  } else { # in some cases the fit indices are available only from the model's summary 
    quiet <- function(fit) { # this was written by Alicia FRANCO MARTÍNEZ on the lavaan Google group
      sink(tempfile())
      on.exit(sink()) 
      invisible(summary(fit, standardized = TRUE, fit.measures=TRUE))
    } 
    sum <- quiet(model)
    fit.indices <- sum$FIT[fits]
    return(fit.indices)}}