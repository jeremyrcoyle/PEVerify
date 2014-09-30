#file="inst/extdata/data/power/19112//Main_meter1.csv"
#file="inst/extdata/data/AutoMV_1/Main_1.csv"
#data=read_input("inst/extdata/data/power/19112//Main_meter1.csv")
read_input=function(file,fillMissingRows=T){
  

  #read file once to find where the meta_data and data sections are
  initial_read=readLines(file,warn=F)
  
  #regexps to match headers
  meta_header="^[^#]*buildingID.*"
  
  #leaving this here in case they change the data spec again
  localtime=T
  if(localtime==T){
    data_header="^[^#]*time.LOCAL.*"
  }  else {
    data_header="^[^#]*time.UTC.*"
  }
  
  #find rows corresponding to start of metadata and actual data
  meta_start=grep(meta_header,initial_read)
  data_start=grep(data_header,initial_read)
  
  if(length(meta_start)!=1){
    stop(sprintf("cannot find metadata - looking for '%s'\n",meta_header))
  }
  
  if(length(data_start)!=1){
    stop(sprintf("cannot find mdata - looking for '%s'\n",data_header))
  }
  
  #read in metadata
  metadata=read.csv(file,skip=meta_start-1,nrows=1)
  metadata=metadata[,!is.na(metadata)]
  
  #read in actual data
  data=read.csv(file,skip=data_start-1,stringsAsFactors=F)
  
  #data$time=as.POSIXct(strptime(data$time.LOCAL,"%Y-%m-%d %H:%M:00"))
  data$time=as.POSIXct(strptime(data$time.LOCAL,"%m/%d/%y %H:%M"))
  
  #merge in any missing times
  if(fillMissingRows){
    expectedtimes=data.frame(time=seq(from=min(data$time,na.rm=T),to=max(data$time,na.rm=T),by="15 min"))
    data=merge(expectedtimes,data,all.x=T)
  }
  
  #create various time indicators
  data$hour=as.numeric(strftime(data$time,"%H"))
  data$minute=as.numeric(strftime(data$time,"%M"))
  data$date=strftime(data$time,"%Y-%m-%d")
  data$timeofday=data$hour+data$minute/60
  
  data$fifteenmin=round(data$timeofday*4)/4
  data$weekday=as.numeric(strftime(data$time,"%u"))
  
  #factorize indicators
  data$hour_fac=factor(data$hour)
  data$timeofday_fac=factor(data$timeofday)
  data$fifteenmin=factor(data$fifteenmin)
  data$weekday_fac=factor(data$weekday)
  
  #get holidays provided by LBNL
  #data(holidays)
  #data$holiday=as.numeric(data$date%in%holidays$date)
  
  #use a more limited list of holidays that also includes holiday names
  data(betterholidays)
  data=merge(data,betterholidays,all.x=T)
  data$holiday[is.na(data$holiday)]="none"
  
  data$isholiday=as.numeric(data$holiday!="none")
  data$workday=(!data$isholiday)&data$weekday<=5
  
  #change names for less typing
  lbnlnames=c("wbelectricity.kWh","coolingelectricity.kWh","heatingelectricity.kWh", 
   "ventilationelectricity.kWh","lightingelectricity.kWh","dboat.F", 
   "rh.Percent","dptemp.F","windspeed.MPH","occupancy.Percent", 
   "occupancy.Persons", "occupancy.SF")
  shortnames=c("power", "coolingpower", "heatingpower", "ventpower", "lightpower", 
               "temp", "rh", "dewpt", "wind", "perocc", "numocc", "sqftocc")
  curnames=names(data)
  matches=match(curnames,lbnlnames)
  names(data)[which(!is.na(matches))]=shortnames[matches[which(!is.na(matches))]]
  
  #impute missing power values
  #data$imp_wbelectricity.kWh=snaive_impute(data$wbelectricity.kWh)
  
  return(list(data=data,metadata=metadata))
}



