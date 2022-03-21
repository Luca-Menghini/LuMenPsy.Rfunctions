itemsICC <- function(data,items,IDvar,output="text",digits=2){ require(lme4)
  data <- na.omit(data[,c(IDvar,items)])
  res <- data.frame(item=NA,icc=NA)
  for(i in 1:length(items)){
    m <- lmer(formula=gsub("ID",IDvar,gsub("d1",items[i],"d1~(1|ID)")),data=data) # VAR_between / (VAR_between + VAR_within)
    out <- round(as.data.frame(lme4::VarCorr(m))[1,4]/(as.data.frame(lme4::VarCorr(m))[1,4]+as.data.frame(lme4::VarCorr(m))[2,4]),
                 digits)
    
    # textual output or data.frame
    if(output=="text"){cat(items[i],"ICC =",out,"\n")
    }else{ res <- rbind(res,cbind(item=items[i],icc=out)) }} 
  if(output!="text"){return(res) }}