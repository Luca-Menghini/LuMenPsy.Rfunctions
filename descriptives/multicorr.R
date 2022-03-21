#' @title Correlation matrix of multilevel datasets
#' @param long = long-form data.frame including all time-varying variables with one row per observation
#' @param wide = wide-form data.frame including all time-invariant variables with one row per subject
#' @param cluster = character string indicating the name of the column with the subject identifiers
#' @param lv1 = character vector indicating the column names of the time-varying variables in the long dataset
#' @param lv2 = character vector indicating the column names of the time-invariant variables in the wide dataset
multicorr <- function(long,wide,lv1,lv2,cluster="ID"){ require(Hmisc); require(Rmisc)
  
  colnames(long)[which(colnames(long)==cluster)] <- "ID"
  
  # individual means (lv2) of lv1 variables
  for(VAR in lv1){
    wide <- cbind(wide,newVar=summarySE(long,VAR,"ID",na.rm=TRUE)[,3])
    colnames(wide)[which(colnames(wide)=="newVar")] <- paste(VAR,".cm",sep="") }
  
  # joining individual means (lv2) to the long dataset
  long <- plyr::join(long,
                     wide[,c(which(colnames(wide)==cluster),
                             which(colnames(wide)==paste(lv1[1],
                                                         ".cm",sep="")):which(colnames(wide)==paste(lv1[length(lv1)],
                                                                                                    ".cm",sep="")))],
                     type="left",by="ID")
  
  # mean-centered (lv1) values
  for(VAR in lv1){
    long$newVar <- long[,VAR] - long[,paste(VAR,".cm",sep="")]
    colnames(long)[which(colnames(long)=="newVar")] <- paste(VAR,".dm",sep="") }
  
  # between-subjects correlations (all variables)
  out.b <- rcorr(as.matrix(wide[,c(paste(lv1,".cm",sep=""),lv2)]), type = "pearson")
  rb <- round(out.b$r,2)
  rb[lower.tri(rb)] <- NA
  
  # within-participant correlations (HRV and ESM deviations from individual mean)
  out.w <- rcorr(as.matrix(long[,paste(lv1,".dm",sep="")]), type = "pearson")
  rw <- round(out.w$r,2)
  rw[upper.tri(rw)] <- NA
  
  # filling rb empty cells
  rb[1:length(lv1),1:length(lv1)][lower.tri(rb[1:length(lv1),1:length(lv1)])] <- rw[lower.tri(rw)]
  
  rownames(rb) <- gsub(".cm","",rownames(rb))
  
  return(t(rb))}