#' @title Visualizing changes in multilevel CFA estimates by removing participants one-to-one (influential cases analysis)
#' @param data = data.frame used by the model.
#' @param cluster = Character. Variable name in the data frame defining the cluster in a two-level dataset.
#' @param parameter = Character. "var" for estimated variances, "load" for loadings.
#' @param st = Character indicating the standardization level of loadings: "st.all" or "st.lv".
#' @param n.items = Integer. Number of observed variables considered by the model.
#' @param item.labels = Character vector indicating the labels of the considered observed variables.
#' @param m = Character string specifying the model using the lavaan synthax.

sample.fluct <- function(data=NA,cluster="ID",parameter="var",st="st.all",
                         n.items=3,item.labels=c("stress","worry","mood"),
                         m = 'level: 1
                              Distress_w =~ stress + worry + mood
                              level: 2
                              Distress_b =~ stress + worry + mood'){ require(lavaan); require(tcltk)
  
  # function to estiamte and store estimated parameters
  estTable <- function(data=data,m=m,cluster=cluster){
    
    # model fitting
    m.res <- cfa(model=m,data=data,cluster=cluster,std.lv=TRUE)
    
    # parameters
    if(parameter=="var"){
      p <- parameterestimates(m.res)
      lv1 <- p[p$op=="~~" & p$est!=1 & p$level==1,"est"]
      lv2 <- p[p$op=="~~" & p$est!=1 & p$level==2,"est"]
    } else if(parameter=="load"){
      if(st=="st.all"){
        p <- standardizedsolution(m.res)
        lv1 <- p[p$op=="=~","est.std"][1:n.items]
        lv2 <- p[p$op=="=~","est.std"][(n.items+1):(n.items+n.items)]
      } else if(st=="st.lv"){
        p <- standardizedsolution(m.res,type="std.lv")
        lv1 <- p[p$op=="=~","est.std"][1:n.items]
        lv2 <- p[p$op=="=~","est.std"][(n.items+1):(n.items+n.items)]
      } else{
        lv1 <- p[p$op=="=~" & p$est!=1 & p$level==1,"est"]
        lv2 <- p[p$op=="=~" & p$est!=1 & p$level==2,"est"] }}
    
    # dataframe storing results
    parameters <- data.frame(ID=rep("all",n.items),X=item.labels,lv1=lv1,lv2=lv2,neg.var=rep(1,n.items))
    
    # marking neg.var as "0" when no negative values are highlighted
    if(!any(diag(lavInspect(m.res,"est")[["within"]][["theta"]]) < 0) & 
       !any(diag(lavInspect(m.res,"est")[["ID"]][["theta"]]) < 0)){ 
      parameters[1:n.items,"neg.var"] <- 0 }
    
    return(parameters) }
  
  # extracting baseline parameters
  parameters <- estTable(data=data,m=m,cluster=cluster)
  
  # replicate parameters estimation by excluding any participant one-by-one
  IDs <- levels(as.factor(as.character(data$ID)))
  pb <- tkProgressBar("Modeling", "Data modeling",0, 100, 50) # progress bar
  for(ID in IDs){ 
    info <- sprintf("%d%% done", round(which(IDs==ID)/length(IDs)*100))
    setTkProgressBar(pb, round(which(IDs==ID)/length(IDs)*100), sprintf("Data modeling", info), info)
    parameters <- rbind(parameters,estTable(data=data[data$ID!=ID,],m=m,cluster=cluster))
    parameters[(nrow(parameters)-n.items+1):nrow(parameters),"ID"] <- as.character(ID)}
  close(pb)
  parameters <- parameters[,c("ID","X","lv1","lv2","neg.var")]
  return(parameters)}