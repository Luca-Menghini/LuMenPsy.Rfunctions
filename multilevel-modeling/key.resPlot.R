key.resPlot <- function(res=NA,robCheck=NA,levels=c(s.archit,s.timing,paste(s.variab,"c",sep=""),s.auton)){ 
  suppressMessages(suppressWarnings(require(ggplot2)))
  
  # setting color scale
  res[res$t.196==TRUE & res$sig.LRT==TRUE & res$higher.Aw==TRUE,"Value"] <- 10 # 10 = substantial effect
  res[res$t.196==TRUE & ((res$sig.LRT==FALSE & res$higher.Aw==TRUE) | 
                           res$sig.LRT==TRUE & res$higher.Aw==FALSE),"Value"] <- 7.5 # 7.5 = substantial but LRT n.s. OR lower Aw
  res[res$t.196==TRUE & res$sig.LRT==FALSE & res$higher.Aw==FALSE,"Value"] <- 5 # 5 = substantial but both LRT n.s. AND lower Aw
  res[res$t.196==FALSE & ((res$sig.LRT==TRUE & res$higher.Aw==TRUE) | (res$sig.LRT==TRUE & res$higher.Aw==FALSE) |
                            res$sig.LRT==FALSE & res$higher.Aw==TRUE),"Value"] <- 2.5 # 2.5 = unsubstantial and LRT n.s. OR lower Aw
  res[res$t.196==FALSE & res$sig.LRT==FALSE & res$higher.Aw==FALSE,"Value"] <- 1 # 1 = no evidence at all
  res$Measure <- factor(res$measure,levels=levels) # Measures as factor
  
  # plotting
  if(is.na(robCheck)){ # single set of analyses
    ggplot(res,aes(x=Measure,y=as.factor(1),fill=Value)) + geom_tile() + geom_text(aes(label=round(t,2))) +
      scale_fill_gradient2(low="red",high="lightgreen",mid="yellow",midpoint=5,limit = c(1,10), space = "Lab") + 
      labs(x="",y="") + theme(legend.position = "none",axis.text.y = element_blank(),axis.text.x=element_text(angle=45))
  } else { require(reshape2) # multiple robustness checks
    colnames(res)[which(colnames(res)==robCheck)] <- "robCheck"
    ggplot(res,aes(x=Measure, y=as.factor(robCheck), fill=Value)) + geom_tile() + geom_text(aes(label=round(t,2))) +
      scale_fill_gradient2(low="red",high="lightgreen",mid="yellow",midpoint=5,limit = c(1,10), space = "Lab") + 
      labs(x="",y="") + theme(legend.position = "none",axis.text.x=element_text(angle=45)) }}