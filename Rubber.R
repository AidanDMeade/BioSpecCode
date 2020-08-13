Rubber <- function(data)
{
  library(pracma)
  # performs rubberband baseline
  if(length(dim(data))!=0){
  x=dim(data)[1]
  y=dim(data)[2]
  }else{
    y=length(data)
    x=1
    data=matrix(data,x,y)
  }
  datacorrected=matrix(0,x,y)
  for(i in 1:y)
  {
    spectrum=data[,i]
    baseline=Rubberband(spectrum)
    datacorrected[,i]<-data[,i]-baseline
  }
  return(datacorrected)
}

Rubberband <- function(spec)
{
  nf=length(spec)
  l = seq(spec[1], spec[length(spec)], length.out=nf);
  xflat <- spec-l;
  idx=which(xflat == min(xflat), arr.ind = TRUE)# Find minima
  if (min(xflat) < 0) #If the minima are less than zero
  {
    # Perform banding of negative region
    line=c(Rubberband(spec[1:idx]),Rubberband(spec[idx:length(spec)]),deparse.level = 0)
  }
  else{
    line = l# Else take original linear baseline
  }
  line=line[1:length(spec)]
  return(line)
}
