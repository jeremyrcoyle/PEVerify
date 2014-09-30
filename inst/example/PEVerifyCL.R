options(echo = FALSE)
suppressMessages(library(PEVerify))



args=c("inst/extdata/data/sampledata/ModelInput6T.csv",
       "inst/extdata/data/sampledata/ModelInput6P.csv",
       "Nothing",
       "inst/extdata/data/sampledata/PEModelOutput6b.csv",
       "parallel"
)

args=commandArgs(trailingOnly=T)

if(length(args)<4){
  message("usage: R --no-save < PEVerifyCL.R --args trainingFile predictionFile holidayFile outputFile [parallel]")
  q(save="no",status=-1)
} else {
  #parse args
  trainingFile=args[1]  
  predictionFile=args[2]
  holidayFile=args[3] #we use our own holiday file so we ignore this
  outputFile=args[4]
  parallel=!is.na(args[5])&&(args[5]=="parallel")
  
  #check for files
  if(!file.exists(trainingFile)){
    stop(sprintf("trainingFile not found: %s",trainingFile))
  }
  
  if(!file.exists(predictionFile)){
    stop(sprintf("predictionFile not found: %s",predictionFile))
  }
  
  #allow overwrite of output for now
  #if(file.exists(outputFile)){
  #  stop(sprintf("outputFile already exists: %s",outputFile))
  #}  
}

if(parallel){
  #Multicore setup for Linux. 
  #Slightly more work is needed to parallelize on other systems
  #Contact jeremyrcoyle@gmail.com if there's interest
  suppressMessages(library(doMC))
  ncores=6
  registerDoMC(ncores)
  options(mc.cores=ncores)
}

nfolds=96
PE_model(trainingFile,predictionFile,outputFile,parallel,nfolds,debug=F)
