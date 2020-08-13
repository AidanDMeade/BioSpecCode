SpecCal_X<-function(x0,y0,y1,data){
  # default settings for window size and maximumshift
  windowsize=25
  maximumshift=10
  # function to calculate how much to interpolate and create a model to interpolate new data
  model=alignspec(x0,y0,y1,windowsize,maximumshift)
  # applying model to new data
  newdata=correctdata(model,data)
  ref=cbind(y1,y1)
  newref=correctdata(model,ref)
  
  x=c(5:c(dim(data)[1]-5))
  data=data[x,]
  model$s$x=model$s$x[x]
  model$s$x1=model$s$x1[x]
  
  output=list()
  output$model=model
  output$newdata=newdata
  output$newref=newref
  return(output)
}

alignspec<-function(x0,y0,y1,windowsize,maximumshift){
  # options
  options.interpolate='linear'
  options.order=2
  
  #check sizes
  if (windowsize/2-floor(windowsize/2)!=0.5){
    print(paste('Input (windowsize) must be odd, changing from windowsize = ',
                as.character(windowsize),' to ',as.character(windowsize+1),'.'))
    windowsize = windowsize+1
  }
  if(maximumshift>windowsize/2){
    maximumshift_new = floor((floor(windowsize/2)-1)/2)*2+1;
    print(paste('Input (maximumshift) must be < 1/2 windowsizedow width, changing from maximumshift = ',
                as.numeric(maximumshift),' to ',as.numeric(maximumshift_new),'.'))
    maximumshift = maximumshift_new;
  }
  
  w2  = c(windowsize-1)/2;
  m   = seq(w2+maximumshift+1,length(x0)-w2-maximumshift,by=1);
  z1  = matrix(0,length(m),1);
  ij1 = seq(c(-w2-maximumshift),c(w2+maximumshift));
  j2  = seq(-maximumshift,maximumshift);
  j1  = 0;
  
  for (i in 1:length(m)){
    
    ii=m[i]
    #coarse scale
    j1 = j1+1;
    ij = seq(ii-w2,ii+w2);
    ik = t(t(ij))%*%matrix(1,1,2*maximumshift+1)+ matrix(1,length(ij))%*%j2;
    rr = cor(cbind(y0[ij],matrix(y1[ik],dim(ik)[1],dim(ik)[2])))
    i0=which(rr[1,c(2:dim(rr)[1])]==max(rr[1,c(2:dim(rr)[1])]));
    rr = max(rr[1,c(2:dim(rr)[1])])
    
    #fine scale
    switch(tolower(options.interpolate),
           'none'={z1[j1] = x0[ii+j2[i0]]},
           'linear'={
             
             temp=j2[i0]
             i0=pracma:::hessian(alignspecfun,j2[i0],
                                 y0=y0[ij],x1=ij1,y1=y1[ik[1]:ik[dim(ik)[1],dim(ik)[2]]],w2=w2,
                                 algorithm=tolower(options.interpolate))
             if(i0==0){i0=temp}
             temp = approx(ij1,x0[ik[1]:ik[dim(ik)[1],dim(ik)[2]]],
                           i0,method=tolower(options.interpolate))
             z1[j1]=temp$y
           })
  }
  
  s=list()
  s$modeltype = 'alignspectra';
  s$x  = t(x0);
  s$x1 = NULL;
  s$y0 = t(y0);
  s$interpolate = tolower(options.interpolate);
  
  y    = x0[m]
  p    = mldivide(matrix(z1,dim(z1)[1],options.order+1)^do.call(rbind,
                                                                replicate( length(z1),seq(options.order,0,by=-1), simplify=FALSE)),y)
  
  z1   = x0
  s$x1 = t(matrix(z1,length(z1)[1],options.order+1)^do.call(rbind,
                                                            replicate( length(z1),seq(options.order,0,by=-1), simplify=FALSE))%*%p)
  
  
  y_corr = axiscalize(s,y1);
  temp=which(is.na(y_corr$y)==1,arr.ind = TRUE)
  # x0=x0[-temp]
  # y0=y0[-temp]
  # y_corr$y=y_corr$y[-temp]
  # s$x1=s$x1[-temp]
  # s$x=s$x[-temp]
  
  output=list()
  output$y_corr=y_corr
  output$s=s
  output$NAs=temp
  return(output)
  
}

alignspecfun<-function(z,y0,x1,y1,w2,algorithm){
  if (z-floor(z)==0.5){z = z+0.001}
  rr  = spline(x1,y1,xout=c(round(z-w2):round(z+w2)),method="fmm");
  rr  = cor(cbind(y0,rr$y));
  rr=rr[2]
  res = 1-rr
  return(res)
}

axiscalize = function(s,y1){
  y     = spline(s$x1,y1,xout=s$x,method="fmm")
  return(y)
}

correctdata = function(model,data){
  # data=data[-model$NAs,]
  # data[model$NAs,]=0
  # x=c(5:c(dim(data)[1]-5))
  # data=data[x,]
  # model$s$x1=model$s$x1[x]
  # model$s$x=model$s$x[x]
  yold=data
  y=matrix(0,dim(yold)[1],dim(yold)[2])
  for(i in 1:dim(yold)[2]){
    temp = spline(model$s$x1,yold[,i],xout=model$s$x,method="fmm")
    y[,i]=temp$y
  }
  return(y)
}