#' @title Descriptive statistics (mean and SD) and intraclass correlation of multilevel datasets
#' @param long = long-form data.frame including all time-varying variables with one row per observation
#' @param wide = wide-form data.frame including all time-invariant variables with one row per subject
#' @param cluster = character string indicating the name of the column with the subject identifiers
#' @param lv1 = character vector indicating the column names of the time-varying variables in the long dataset
#' @param lv2 = character vector indicating the column names of the time-invariant variables in the wide dataset
#' @param group = character string indicating the name of the column of the grouping variable (defult: NA for no group specification)
#' @param group.labels = character vector indicating the name of the group variable levels (default: NA)
multidesc <- function(long=NA,wide=NA,cluster="ID",lv1=NA,lv2=NA){ require(Rmisc); require(lme4)
  
  # HRV and ESM data
  out <- data.frame(Measure=lv1[1],
                    N=summarySE(long,lv1[1],na.rm=TRUE)[,2],
                    Mean=paste(round(summarySE(long,lv1[1],na.rm=TRUE)[,3],2)," (",
                               round(summarySE(long,lv1[1],na.rm=TRUE)[,4],2),")",sep=""))
  for(i in 2:length(lv1)){
    out <- rbind(out,
                 data.frame(Measure=lv1[i],
                            N=summarySE(long,lv1[i],na.rm=TRUE)[,2],
                            Mean=paste(round(summarySE(long,lv1[i],na.rm=TRUE)[,3],2)," (",
                                       round(summarySE(long,lv1[i],na.rm=TRUE)[,4],2),")",sep="")))}
  
  # PrelQS data
  if(!is.na(lv2[1])){
    for(i in 1:length(lv2)){
      out <- rbind(out,
                   data.frame(Measure=lv2[i],
                              N=summarySE(wide,lv2[i],na.rm=TRUE)[,2],
                              Mean=paste(round(summarySE(wide,lv2[i],na.rm=TRUE)[,3],2)," (",
                                         round(summarySE(wide,lv2[i],na.rm=TRUE)[,4],2),")",sep="")))}}
  
  # ICC
  out$ICC <- NA
  for(i in 1:length(lv1)){
    m <- lmer(formula=gsub("var",lv1[i],gsub("ID",cluster,"var~(1|ID)")),data=long) # VAR_between / (VAR_between + VAR_within)
    out[out$Measure==lv1[i],"ICC"] <- round(as.data.frame(VarCorr(m))[1,4]/
                                              (as.data.frame(VarCorr(m))[1,4]+as.data.frame(VarCorr(m))[2,4]),2)}
  rownames(out) <- gsub(".cm","",rownames(out))
  return(out)}
