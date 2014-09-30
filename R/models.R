##############################################################################
# models.R
# functions that apply predictive models to data
# take training and test data
# fit model on training data, and generate predictions for testing data
#
##############################################################################

#libraries
#library(forecast)


#library(origami)
#load("inst//extdata//data//testdata.rdata")
#train_data=training(data,somefolds[[1]])
#val_data=validation(data,somefolds[[1]])

#############################################
#Mean model - marignal mean
meanmodel=function(train_data,val_data){
  pred=rep(mean(train_data$power,na.rm=T),nrow(val_data))
  return(pred)
}

#############################################
#Mean Week model - mean for each hour of the week
mwmodel=function(train_data,val_data){
  r=lm(power~hour_fac*weekday_fac,train_data)
  pred=predict(r,val_data)
  return(pred)
}

#############################################
#mean week with fifteen minute bins
mw15model=function(train_data,val_data){
  r=lm(power~timeofday_fac*weekday_fac,train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#############################################
#workday model - 15 minute interval means for workdays and nonworkdays
workdaymodel=function(train_data,val_data){
  r=lm(power~timeofday_fac*workday,train_data)
  pred=predict(r,val_data)
  return(pred)
}


#############################################
#earth spline model
earthmodel=function(train_data,val_data){
  train_data$temp=na.approx(train_data$temp,na.rm=F)  
  train_data$power=na.approx(train_data$power,na.rm=F)  
  
  r=earth(power~(timeofday+temp)*weekday_fac,train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#############################################
#earth spline equivalent of lbnlmodel
earthlbnlmodel=function(train_data,val_data){
  train_data$temp=na.approx(train_data$temp,na.rm=F)  
  train_data$power=na.approx(train_data$power,na.rm=F)
  
  r=earth(power~timeofday*weekday_fac+occupied*temp,train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#############################################
#gam equivalent of lbnlmodel
gamlbnlmodel=function(train_data,val_data){
  r=gam(power~timeofday_fac*weekday_fac+s(bestlag_temp,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

gamworkdaylbnlmodel=function(train_data,val_data){
  
  r=gam(power~timeofday_fac*workday+s(bestlag_temp,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#rh
gamlbnlrhmodel=function(train_data,val_data){
  r=gam(power~timeofday_fac*weekday_fac+s(bestlag_rh,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

gamworkdaylbnlrhmodel=function(train_data,val_data){
  
  r=gam(power~timeofday_fac*workday+s(bestlag_rh,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#dewpt
gamlbnldewptmodel=function(train_data,val_data){
  r=gam(power~timeofday_fac*weekday_fac+s(bestlag_dewpt,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

gamworkdaylbnldewptmodel=function(train_data,val_data){
  
  r=gam(power~timeofday_fac*workday+s(bestlag_dewpt,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#wind
gamlbnlwindmodel=function(train_data,val_data){
  r=gam(power~timeofday_fac*weekday_fac+s(bestlag_wind,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

gamworkdaylbnlwindmodel=function(train_data,val_data){
  
  r=gam(power~timeofday_fac*workday+s(bestlag_wind,bs="cs",by=factor(occupied)),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#poccupied
gampoccmodel=function(train_data,val_data){
  r=gam(power~timeofday_fac*weekday_fac+s(poccupied,bs="cs"),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

gamworkdaylbnlpoccmodel=function(train_data,val_data){
  
  r=gam(power~timeofday_fac*workday+s(poccupied,bs="cs"),data=train_data)
  pred=predict(r,val_data)
  
  return(pred)
}

#############################################
#broken model
#generates an error - to test that the algorithm will work even if individual models fail
brokenmodel=function(train_data,val_data){
  stop("broken model is broken")
}

#############################################
#DTT model - Mean Week model plus two piecewise linear temperature terms

add_dtt_indicators=function(data){
  data$Deg50step=(as.numeric(data$temp<50)*(abs(50-data$temp)))
  data$Deg50=(50-data$temp)
  data$Deg65step=(as.numeric(data$temp>65)*(abs(data$temp-65)))
  data$Deg65=(data$temp-65)  
  
  return(data)
}

dttmodel=function(train_data,val_data){
  train_data=add_dtt_indicators(train_data)
  val_data=add_dtt_indicators(val_data)
  r = lm(power~hour_fac*weekday_fac+Deg50step+Deg65step,train_data)
  
  pred=predict(r,val_data)
  return(pred)
}

#############################################
#LBNL model - Mean Week model plus 6 piecewise linear temperature terms
define_temp_indicators=function(temp){
  #print(temp[(!is.finite(temp))])
  MAX = max(temp,na.rm=T)
  MIN =min(temp,na.rm=T)
  B_vec = seq(from=MIN,to=MAX, length=7)
  B_vec[1]=0
  B_vec
}

add_temp_vectors = function(data,B_vec){
  temp=data$temp
  T=mapply(function(lower,upper,temp){
    ifelse(temp<lower,0,ifelse(temp>upper,upper-lower,temp-lower))
  },B_vec[-length(B_vec)],B_vec[-1],MoreArgs=list(temp=temp))
  colnames(T)=sprintf("T%d",1:6)
  data=cbind(data,cbind(data$occupied*T,unocc=(!data$occupied)*data$temp))
  
  return(data)
}

lbnlmodel=function(train_data,val_data){
  
  train_data$temp=na.approx(train_data$temp,na.rm=F)  
  B_vec = define_temp_indicators(train_data$temp[which(train_data$occupied==1)]) 
  
  train_data=add_temp_vectors(train_data,B_vec)
  val_data=add_temp_vectors(val_data,B_vec)
  
  r=lm(power~hour_fac*weekday_fac+T1+T2+T3+T4+T5+T6+unocc,train_data)
  
  pred=predict(r,newdata=val_data)
  return(pred)
}

lbnl15model=function(train_data,val_data){
  
  train_data$temp=na.approx(train_data$temp,na.rm=F)  
  B_vec = define_temp_indicators(train_data$temp[which(train_data$occupied==1)]) 
  
  train_data=add_temp_vectors(train_data,B_vec)
  val_data=add_temp_vectors(val_data,B_vec)
  
  r=lm(power~timeofday_fac*weekday_fac+T1+T2+T3+T4+T5+T6+unocc,train_data)
  
  pred=predict(r,newdata=val_data)
  return(pred)
}

# ####################################
# #function factory to generate lagged gam models
# laggedgamFactory=function(lagVariable){
#   
#   force(lagVariable)
#   lagVariable="bestlag_temp"
#   gamlbnl=function(train_data,val_data){
#     form=formula(sprintf("power~timeofday_fac*weekday_fac+s(%s,bs='cs',by=factor(occupied))",lagVariable))
#     r=gam(form,data=train_data)
#     pred=predict(r,val_data)
#     
#     return(pred)
#   }
# 
#   gamworkday=function(train_data,val_data){
#     form=formula(sprintf("power~timeofday_fac*workday+s(%s,bs='cs',by=factor(occupied))",lagVariable))
#     r=gam(form,data=train_data)
#     pred=predict(r,val_data)
#     
#     return(pred)
#   }
#   
#   c(gamlbnl,gamworkday)
#   
# }
# 
# all_var_lagfactory=function(factor)
# {
#   
#   lagvars=c("bestlag_temp", "bestlag_dewpt", "bestlag_rh", "bestlag_wind", 
#   "bestlag_poccupied")
#   fun_names=c("gamlbnl","gamworkday")
#   fun_defs = unlist(lapply(lagvars,laggedgamFactory))
#   
#   #Set global environment here. 
#   names = paste(rep(fun_names,length(lagvars)),rep(lagvars,each=2),sep="_")
#   LEN = length(names)
#   foreach(i=1:LEN)%do%
#     assign(names[[i]],fun_defs[[i]],env=.GlobalEnv)  
#   names
# }
# 
# all_var_lagfactory()