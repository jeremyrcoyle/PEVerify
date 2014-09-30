#lag a vector
lagvec=function(lag,vec){
  c(rep(vec[1],lag),vec)[1:length(vec)]
}

#fit a gam to evaluate
lagresid_generic=function(lagvec,data){
  #data$lagvar=lagvec
  r2=gam(resid~s(lagvec,bs="cs",by=factor(occupied)),data=data)
  mean(resid(r2)^2)
}

bestlag=function(variable,lagseq,data){
  lagvecs=lapply(lagseq,lagvec,data[,variable])
  mses=sapply(lagvecs,lagresid_generic,data)
  bestlag=which.min(mses)
  message(sprintf("Best lag for %s is %d",variable,lagseq[bestlag]))
  lagseq[bestlag]
}


#find the best lag for the weather variables
bestlags=function(data){
  tolag=c("temp","dewpt","rh","wind","poccupied")
  tolag=intersect(tolag,names(data))
  
  lagseq=seq(from=0,to=96,by=4)
  
  r=lm(power~timeofday_fac*weekday_fac,data)
  data$resid=data$power-predict(r,newdata=data)
  
  sapply(tolag,bestlag,lagseq,data)
}

#function to actually get the best lags
get_lags=function(data,bestlags){
  
  bestlagvecs=sapply(1:length(bestlags),function(i){lagvec(bestlags[i],data[,names(bestlags)[i]])})
  colnames(bestlagvecs)=paste("bestlag",names(bestlags),sep="_")
  
  bestlagvecs
}