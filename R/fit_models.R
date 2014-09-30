#fit model, return preds, returning predictions from a mean model if there's an error
safe_getpreds=function(model,train_data,val_data,meanpred){
    #cat(model,"\n")
    
    #fallback to a mean model
    pred=meanpred
    
    #attempt to fit the better model
    try({
      pred=do.call(model,list(train_data,val_data))
    })
    
    return(pred)
}

#generate predictions for a particular fold 
cv_models=function(fold,data,models){
  message(sprintf("\tFold: %d",fold_index()))
  
  train_data=training(data)
  val_data=validation(data)
  
  val_data$fold=fold_index()
  val_data$horizon=1:nrow(val_data)
  #fit models  
  meanpred=meanmodel(train_data,val_data)
  results=sapply(models,safe_getpreds,train_data,val_data,meanpred)
  
  #return results
  names(results)=models  
  results=cbind(val_data,results)  
  
  return(list(results=results))
}


#function to load data, fit the model, and return output file
#trainfile="inst/extdata/data/sampledata/ModelInput6T.csv"
#predfile="inst/extdata/data/sampledata/ModelInput6P.csv"
#outfile="inst/extdata/data/sampledata/PEModelOutput6.csv"
#trainfile="inst/extdata/data/power/19112/meter1T.csv"
#predfile="inst/extdata/data/power/19112/meter1P.csv"
PE_model=function(trainfile,predfile,outfile,parallel=T,nfolds=200,debug=F){

  #load input files
  train=read_input(trainfile)
  pred=read_input(predfile,fillMissingRows=F)
  
  #generate, merge in occupancy
  #note: ideally occupancy determination would be included in cross-validation
  #this is necessary for accurate performance assessment
  #for now it's done for the full training dataset as a time-saving measure
  message(sprintf("Clustering to estimate occupancy"))
  traindata=train$data
  occ=est_occupancy(traindata)
  traindata=merge(traindata,occ)
  traindata=traindata[order(traindata$time),]
  
  #generate, merge in best lagged predictors
  #same disclaimer as above applies
  message(sprintf("Estimating best lag for some predictors"))
  bestlags=bestlags(traindata)
  bestlagvecs=get_lags(traindata,bestlags)
  traindata=cbind(traindata,bestlagvecs)
  
  #generate training and testing splits
  ntimes=nrow(traindata)
  train_size=round(ntimes*0.5)
  val_size=round(ntimes*0.45)
  folds=make_folds(ntimes,fold_fun=folds_rolling_window,window_size=train_size,validation_size=val_size)
  #folds=make_folds(ntimes)
  
  #randomly subsample folds for speed
  if(nfolds<length(folds)){
    folds=sample(folds,nfolds)
    #reindex folds
    for(i in 1:length(folds)){
      folds[[i]]@v=as.integer(i)
    }
  }
  
  #list of models to run
  models=c(c("mwmodel","mw15model","lbnlmodel","lbnl15model",
             "workdaymodel","meanmodel","earthmodel","earthlbnlmodel"),
           c("gamlbnlmodel", "gamworkdaylbnlmodel",  
             "gampoccmodel", "gamworkdaylbnlpoccmodel"))
  
  #add humidity models if RH is available
  if("bestlag_rh" %in% names(traindata)){
    models=c(models,"gamlbnlrhmodel", "gamworkdaylbnlrhmodel")
  }
  
  #generate matrix of cross-validated predictions and actual values
  #change some_folds to folds when running for real
  message(sprintf("Fitting models on %d cross-validation folds",length(folds)))
  cvresults=cross_validate(cv_models,folds,traindata,models,.parallel=parallel) 
  cvmat=cvresults$results
  
  #fit linear regression to find best combination
  message(sprintf("Fitting best combination",length(folds)))
  goodrows=apply(cvmat[,c("power",models)],1,function(x)all(!is.na(x)))
  combmod=nnls(as.matrix(cvmat[which(goodrows),models]),cvmat$power[which(goodrows)])
  #coefs=coef(combmod)
  coefs=coef(combmod)/sum(coef(combmod))
  
  cvpred=as.matrix(cvmat[,models])%*%coefs
  
  #combmod=lm(power~.-1,cvmat[,c("power",models)])
  
  #refit models on full training data, and predict on testing data
  message("Refitting models to full training set")
  preddata=pred$data
  preddata=merge(preddata,occ)
  preddata=preddata[order(preddata$time),]
  
  bestlagvecs=get_lags(preddata,bestlags)
  preddata=cbind(preddata,bestlagvecs)
  
  meanpred=meanmodel(traindata,preddata)
  predmat=sapply(models,safe_getpreds,traindata,preddata,meanpred)
  
  combpred=predmat%*%coefs
  
  #debug files
  if(debug){
    testout=cbind(preddata,data.frame(predmat,combmodel=combpred))
    write.csv(testout,"inst//extdata//data//testoutput.csv",row.names=F)
    
    coefout=data.frame(model=models,coef=coefs)
    write.csv(coefout,"inst//extdata//data//testcoefs.csv",row.names=F)
  }
  #write output
  outdata=data.frame(time.LOCAL=preddata$time.LOCAL,wbelectricity.kWh=combpred)
  
  message("Writing Output")
  write_output(outfile,pred$metadata,outdata)
  
}