#' @title Separating multiple-choice checklist variable into multiple columns
#' @param data = data.frame with the checklist column.
#' @param varName = Character string indicating the name of the checklist variable.
#' @param sep = Character string indicating the category separator (default: comma ",")
#' @param nOptions = Number of response categories in the checklist variable
#' @param labels = Character vector of length = nOptions including the labels of the checklist categories 
#' @param new.labels = Optional argument to change category labels (defult: NA) 
#' @param return.data = Logical value indicating whether the recoded dataset should be returned (default: FALSE) 
#' @param printInfo = Logical value indicating whether the No. and % of cases in each category shold be printed (default: TRUE) 
checklist_sep <- function(data=NULL,varName=NULL,sep=",",nOptions,labels,new.labels=NA,return.data=FALSE,printInfo=TRUE){
    
    # sanity checks
    if(length(labels)!=nOptions){ stop(message="the number of labels should be equal to nOptions") }
    if(!is.na(new.labels[1]) & length(new.labels)!=nOptions){ stop(message="the number of new.labels should be equal to nOptions") }
    
    # creating one column for each category
    checklist <- data.frame(checklist_var=as.character(data[,varName]))
    for(i in 1:nOptions){ checklist$new.col <- NA
    colnames(checklist)[i+1] <- labels[i] }
    
    # populating dataset by assigning 0 (not selected) or 1 (selected) when the checklist variable isn't missing
    for(i in 1:nrow(checklist)){
        if(!is.na(checklist[i,1])){ checklist[i,2:ncol(checklist)] <- 0
        selected <- unlist(strsplit(checklist[i,1],sep))
        for(k in 1:length(selected)){ checklist[i,selected[k]] <- 1 }}}
    
    # updating category labels when new.labels has been set
    if(!is.na(new.labels[1])){ for(i in 2:ncol(checklist)){ colnames(checklist)[i] <- new.labels[i-1] }
        labels <- new.labels }
    
    # adding category columns to the original dataset
    data <- cbind(data[,1:which(colnames(data)==varName)],checklist[,2:ncol(checklist)],
                  data[,(which(colnames(data)==varName)+1):ncol(data)])
    checklist[,2:(nOptions+1)] <- lapply(checklist[,2:(nOptions+1)],as.factor) # converting as factor
    
    # printing No. and % of cases in each category
    if(printInfo==TRUE){ cat("\n\nNumber and % of non-missing cases for each category:\n")
        for(SC in labels){ cat("- ",SC,": ",summary(checklist[,SC])[2],", ",
                               round(100*summary(checklist[,SC])[2]/nrow(checklist[!is.na(checklist$checklist_var),]),2),
                               "%\n",sep="")}}
    
    # returning data
    if(return.data==TRUE){return(data)}}