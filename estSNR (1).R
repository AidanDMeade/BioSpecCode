estSNR<-function(signal,stderr1,stderr2){
  # source("C:/Users/adrian.maguire/Google Drive/R code/Rubber.R")
  smowind=15
  poly=5
  signalout=c()
  tempspec=signal
  tempspec=Rubber(tempspec)
  signalESNR=c(0)
  smo=matrix(0,dim(tempspec)[1],dim(tempspec)[2])
  for(i in 1:dim(tempspec)[2]){
    smo[,i]=savgol(tempspec[,i],smowind,poly,0)
  }
  difference=tempspec-smo
  for(i in 1:dim(tempspec)[2]){
    eN=quantile(abs(difference[,i]),c(.506))
    signalESNR[i]=max(smo[,i])/eN
  }
  temp=1
  temp2=c(0)
  while(isTRUE(length(temp)==0)==FALSE){
    if(temp2==0){
      temp=which(signalESNR>c(mean(signalESNR)+stderr1*std(signalESNR)))}
    if(isTRUE(length(temp)==0)==FALSE){
      signalESNR=signalESNR[-temp]
      signalout=c(signalout,temp)
      signalout=unique(signalout)
      temp=which(signalESNR>c(mean(signalESNR)+stderr2*std(signalESNR)))
      print(isTRUE(length(temp)==0))
      temp2=1
    }
  }
  signalESNRdb=10*log(signalESNR,base=10)
  return(list(SNRE=signalESNR,SNREdb=signalESNRdb,outliers=signalout))
}
  