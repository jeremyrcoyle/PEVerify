write_output=function(outfile,metadata,data){
  #write to file
  con=file(outfile,"wt")
  writeLines("#metadata",con)
  write.csv(metadata,con,row.names=F,quote=F)
  writeLines("#Ouptut from Persistent Efficency Model",con)
  write.csv(data,con,row.names=F,quote=F)
  close(con)
}