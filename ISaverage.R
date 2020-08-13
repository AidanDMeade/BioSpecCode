ISaverage<-function(Matrix_IP,no_spec){

########################################################################
  # This is a function for the intersample averaging of spectra in a matrix;
# Usage: [Matrix] = intersample_ave(Matrix_IP,no_spec)  
# Where: Matrix is the output matrix with averaged spectra
#        no_spec is the number of spectra to average
# By Colin Clarke and Aidan Meade FOCAS institute DIT 2008 in Matlab and 
# translated to R by Adrian Maguire
########################################################################
  x=dim(Matrix_IP)[1]
  y=dim(Matrix_IP)[2]
  
  #Calculating number of output averaged spectra
  output_no=round(x/no_spec);
  
  #Initialising output matrix and holding matrix
  Matrix=matrix(0,output_no,y);
  spectrum=matrix(0,output_no,y);
  
  for(j in 1:output_no){
  #Calculating size of input matrix
    x=dim(Matrix_IP)[1]
    y=dim(Matrix_IP)[2]
  #Randomly sorting spectra in the matrix
  p=sample(c(1:x));
  #     Matrix_IP=intrlv(Matrix_IP,p);
  Matrix_IP=Matrix_IP[p,]
  #This routine prevents overrun of the loop
  if(x<=no_spec){
  num=x
  }else{
    num=no_spec
  }
  #Now selecting the first no_spec (max) spectra and averaging
  k=c(1:num)
  spectrum[j,]=colMeans(Matrix_IP[k,])
  #Now removing these no_spec(max) spectra from the input set
  Matrix_IP=Matrix_IP[c(1:x),]
  #Assigning average spectrum to output matrix
  Matrix[j,]=spectrum[j,]
  }
  return(Matrix)
}