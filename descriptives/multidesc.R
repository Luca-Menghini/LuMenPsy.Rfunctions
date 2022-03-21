#' @title Descriptive statistics (mean and SD) and intraclass correlation of multilevel datasets
#' @param long = long-form data.frame including all time-varying variables with one row per observation
#' @param wide = wide-form data.frame including all time-invariant variables with one row per subject
#' @param cluster = character string indicating the name of the column with the subject identifiers
#' @param lv1 = character vector indicating the column names of the time-varying variables in the long dataset
#' @param lv2 = character vector indicating the column names of the time-invariant variables in the wide dataset
#' @param group = character string indicating the name of the column of the grouping variable (defult: NA for no group specification)
#' @param group.labels = character vector indicating the name of the group variable levels (default: NA)
multidesc <- function(long,wide,cluster="S.ID",lv1,lv2,group=NA,group.labels=NA){ require(Rmisc); require(lme4)
  
  out <- data.frame(Measure=lv1[1],
                    N=summarySE(long,lv1[1],na.rm=TRUE)[,2],
                    Mean=paste(round(summarySE(long,lv1[1],na.rm=TRUE)[,3],2)," (",
                               round(summarySE(long,lv1[1],na.rm=TRUE)[,4],2),")",sep=""))
  if(!is.na(group)){
    colnames(long)[which(colnames(long)==group)] <- colnames(wide)[which(colnames(wide)==group)] <- "group"
    long$group <- as.factor(as.character(long$group))
    wide$group <- as.factor(as.character(wide$group))
    groups <- levels(long$group)
    out$Mean1 = paste(round(summarySE(long[long$group==groups[1],],lv1[1],na.rm=TRUE)[,3],2)," (",
                      round(summarySE(long[long$group==groups[1],],lv1[1],na.rm=TRUE)[,4],2),")",sep="")
    out$Mean2 = paste(round(summarySE(long[long$group==groups[2],],lv1[1],na.rm=TRUE)[,3],2)," (",
                      round(summarySE(long[long$group==groups[2],],lv1[1],na.rm=TRUE)[,4],2),")",sep="") }
  
  for(i in 2:length(lv1)){
    if(!is.na(group)){
      out <- rbind(out,
                   data.frame(Measure=lv1[i],
                              N=summarySE(long,lv1[i],na.rm=TRUE)[,2],
                              Mean=paste(round(summarySE(long,lv1[i],na.rm=TRUE)[,3],2)," (",
                                         round(summarySE(long,lv1[i],na.rm=TRUE)[,4],2),")",sep=""),
                              Mean1=paste(round(summarySE(long[long$group==groups[1],],lv1[i],na.rm=TRUE)[,3],2)," (",
                                          round(summarySE(long[long$group==groups[1],],lv1[i],na.rm=TRUE)[,4],2),")",sep=""),
                              Mean2=paste(round(summarySE(long[long$group==groups[2],],lv1[i],na.rm=TRUE)[,3],2)," (",
                                          round(summarySE(long[long$group==groups[2],],lv1[i],na.rm=TRUE)[,4],2),")",sep="")))
    } else {
      out <- rbind(out,
                   data.frame(Measure=lv1[i],
                              N=summarySE(long,lv1[i],na.rm=TRUE)[,2],
                              Mean=paste(round(summarySE(long,lv1[i],na.rm=TRUE)[,3],2)," (",
                                         round(summarySE(long,lv1[i],na.rm=TRUE)[,4],2),")",sep=""))) }}
  
  # demos data
  for(i in 1:length(lv2)){
    if(!is.na(group)){
      out <- rbind(out,
                   data.frame(Measure=lv2[i],
                              N=summarySE(wide,lv2[i],na.rm=TRUE)[,2],
                              Mean=paste(round(summarySE(wide,lv2[i],na.rm=TRUE)[,3],2)," (",
                                         round(summarySE(wide,lv2[i],na.rm=TRUE)[,4],2),")",sep=""),
                              Mean1=paste(round(summarySE(wide[wide$group==groups[1],],lv2[i],na.rm=TRUE)[,3],2)," (",
                                          round(summarySE(wide[wide$group==groups[1],],lv2[i],na.rm=TRUE)[,4],2),")",sep=""),
                              Mean2=paste(round(summarySE(wide[wide$group==groups[2],],lv2[i],na.rm=TRUE)[,3],2)," (",
                                          round(summarySE(wide[wide$group==groups[2],],lv2[i],na.rm=TRUE)[,4],2),")",sep="")))
    } else {
      out <- rbind(out,
                   data.frame(Measure=lv2[i],
                              N=summarySE(wide,lv2[i],na.rm=TRUE)[,2],
                              Mean=paste(round(summarySE(wide,lv2[i],na.rm=TRUE)[,3],2)," (",
                                         round(summarySE(wide,lv2[i],na.rm=TRUE)[,4],2),")",sep=""))) }}
  
  # ICC
  out$ICC <- NA
  for(i in 1:length(lv1)){
    m <- lmer(formula=gsub("var",lv1[i],gsub("ID",cluster,"var~(1|ID)")),data=long) # VAR_between / (VAR_between + VAR_within)
    out[out$Measure==lv1[i],"ICC"] <- round(as.data.frame(lme4::VarCorr(m))[1,4]/
                                              (as.data.frame(lme4::VarCorr(m))[1,4]+as.data.frame(lme4::VarCorr(m))[2,4]),2)}
  rownames(out) <- gsub(".cm","",rownames(out))
  if(!is.na(group)){ if(!is.na(group.labels)){
    colnames(out)[c(which(colnames(out)=="Mean1"),which(colnames(out)=="Mean2"))] <- group.labels 
  } else{ colnames(out)[c(which(colnames(out)=="Mean1"),which(colnames(out)=="Mean2"))] <-  levels(long$group)  }}
  return(out)}