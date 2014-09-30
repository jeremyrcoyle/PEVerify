#kmeans clustering to divide weekly data into occupied and nonoccupied times
est_occupancy=function(data){
  obsdata=data[!is.na(data$power),]
  
  #fit two clusters to the raw power data
  kmfit=kmeans(obsdata$power,2,iter.max=100,nstart=10)
  obsdata$occupied=as.numeric(kmfit$cluster==which.max(kmfit$centers))
  
  agg=aggregate(list(poccupied=obsdata$occupied,avgpower=obsdata$power),obsdata[,c("fifteenmin","weekday")],mean)
  agg2=aggregate(list(sdpower=obsdata$power),obsdata[,c("fifteenmin","weekday")],sd)
  agg=merge(agg,agg2)

  #fit three clusters to average and standard deviation of each hour
  kmfit2=kmeans(agg[,c("avgpower","sdpower")],3,iter.max=100,nstart=10)
  agg$occupied=(kmfit2$cluster!=which.min(kmfit2$centers[,"avgpower"]))
  
  return(agg)
}