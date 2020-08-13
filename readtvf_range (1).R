readtvf<-function(spec_name){
  # This function reads in .tvf and .tsf files from labspec 4 files and returns a list structure in R
  # the function is based on the function tvfreadone which was developed in Matlab. It was translated
  # into R by Adrian Maguire and was last updated on the 11 March 2016
  
  filename=matrix(spec_name);
  DataTypesCell=matrix(c('integer' , 'character' , 'integer' , 'integer', 'Rational' , 'double'),1)
  DataTypesCellsize=matrix(c(4 , 'integer' , 4 , 4, 'Rational' , 4),1)
  
  
  # Open file for reading
  info=file(filename, open = "rb", blocking = TRUE,
            encoding = getOption("encoding"), raw = TRUE)
  
  OffsetIFD=readBin(info, what='integer',n=2 ,size = 4) # IFD = Image File Directory = Offset to TAG's table
  status=seek(info, where = OffsetIFD[2], origin = "start")
  numberOfFields=readBin(info, what='integer',n=1 ,size = 2) # TAG's count = 33 for standard TVF files . See just below for a description of these fields :
  
  
  # Fields=list('Tag','Type','Count','Value','ValueOffset')
  Fields=list()
  # numberOfFields
  for (i in 1:numberOfFields)
  {
    Fields$Tag[i]=readBin(info, what='integer',n=1 ,size = 2)
    Fields$Type[i]=readBin(info, what='integer',n=1 ,size = 2)
    Fields$Count[i]=readBin(info, what='integer',n=1 ,size = 4)
    Fields$ValueOffset[i]=readBin(info, what='integer',n=1 ,size = 4)
    
    
    if(Fields$Count[i]==1 & (Fields$Type[i]==3 | Fields$Type[i]==4))
    {Fields$Value[i]=list(Fields$ValueOffset[i])
    }
    
    else
    {
      Currentposition=seek(info)
      status=seek(info, where = Fields$ValueOffset[i], origin = "start")
      if(Fields$Type[i]==5)
      {
        for(ii in 1:Fields$Count[i])
        {
          Fields$Value[i][ii]=readBin(info, what='integer',n=1 ,size = 4)/readBin(info, what='integer',n=1 ,size = 4)
        }
      }else if(Fields$Type[i]==134)
      {
        for(j in 1:Fields$Count[i])
        {
          tmp1=readBin(info, what='integer',n=1 ,size = 4)
          tmp2=readBin(info, what='integer',n=1 ,size = 4)
          if(j==1){
            Fields$Value[[i]]=list(Offset=tmp1,Size=tmp2)
          }else{Fields$Value[[i]]$Offset[j]=tmp1
          Fields$Value[[i]]$Size[j]=tmp2
          }
          
          if(is.null(Fields$Value[i]$Size[j])==FALSE){
          if(Fields$Value[[i]]$Size[[j]]!=0)
          {
            Currentposition2=seek(info,where=NA,origin="start")
            Fields$Value[[i]]$AxesValue[j]=readBin(info, what='integer',n=Fields$Value[i]$Size[j] ,size = 4)
            status=fseek(fid,CurrentPosition2);
          }}
        }
      }else if(Fields$Type[i]==130)
      {
        if(Fields$Tag[i]==-15536)
        {
          for(j in 1:Fields$Count[i])
          {
            tmp11=readBin(info, what='integer',n=1 ,size = 4)
            tmp22=readBin(info, what='integer',n=1 ,size = 4)
            if(j==1){
              Fields$Value[[i]]=list(Offset=tmp11,Size=tmp22)
            }else{Fields$Value[[i]]$Offset[j]=tmp11
            Fields$Value[[i]]$Size[j]=tmp22
            
            }
            if(Fields$Value[[i]]$Size[[j]]!=0)
            {
              Currentposition2=seek(info)
              status=seek(info,Fields$Vlaue[j]$Offset)
              tempo=readBin(info, what='character',n=Fields$Value[i]$Size,size=1)
              Fields$Value[i]$AxesLabels=tempo
              status=seek(info,Currentposition2)
            }
          }
        }else if (Fields$Tag[i]==-15532)
        {
          for(j in 1:Fields$Count[i])
          {
            Fields$Value[j]$Offset=readBin(info, what='integer',n=1 ,size = 4)
            Fields$Value[j]$Size=readBin(info, what='integer',n=1 ,size = 4)
            if(Fields$Value[i]$Size!=0)
            {
              Currentposition2=seek(info)
              Fields$Value[i]$Properties[j]=fread(info,n=Fields$Value[i]$Size[j],size=1);
              IndexZeros=which(Fields$Values[i]$Properties[j]==0);
              Fields$Value[i]$Name[j]=Fields$Value[i]$Properties[j][1:IndexZeros(1)-1];
              Fields$Value[i]$Type[j]=Fields$Value[i]$Properties[j][IndexZeros(1)+1];
              Fields$Value[i]$Value[j]=Fields$Value[i]$Properties[j][IndexZeros(1)+2:end];
              status=seek(info,Currentposition2)
            }
          }
          
        }
        else if(Fields$Tag[i]==-15526){
          for(j in 1:Fields$Count[i])
          {
            
            tmp11=readBin(info, what='integer',n=1 ,size = 4)
            tmp22=readBin(info, what='integer',n=1 ,size = 4)
            if(j==1){
              Fields$Value[[i]]=list(Offset=tmp11,Size=tmp22)
            }else{Fields$Value[[i]]$Offset[j]=tmp11
            Fields$Value[[i]]$Size[j]=tmp22
            }
            if(Fields$Value[[i]]$Size[j]!=0)
            {
              Currentposition2=seek(info)
              status=seek(info,Fields$Vlaue[j]$Offset[j])
              tempo=readBin(info, what='raw',n=Fields$Value[[i]]$Size[j],size=1)
              Fields$Value[[i]]$AxesLabels[j]=list(AxesLabels=tempo)
              status=seek(info,Currentposition2)
            }
          }
        }
      }else if (Fields$Tag[i]==-15525 | Fields$Tag[i]==-15524){
        
        tmp5=as.integer(readBin(info,what='raw',n=Fields$Count[i]))
        Fields$Value[i]=list(tmp5)
        
      }else{
        
        tmp=readBin(info,what=DataTypesCell[Fields$Type[i]],n=Fields$Count[i],size=DataTypesCellsize[Fields$Tag[i]])
        Fields$Value[i]=list(c(tmp))
        
        if(Fields$Type[2]==2)
        {
          Fields$Value[i]=Fields$Value[i]
        }
      }
      status=seek(info,Currentposition)
    }
  }
  
  
  # name of the possible axes types
  
  axes_type_name = c('Time' , 'Spectral type' , 'Z' , 'XY points' , 'X' , 'Y' , 'Intensity' , 'Pixel')
  TVFSpectra=list('Name','NbWavelengths','NbSpectra','Abscisses_Axes')
  TVFSpectra$Name = Fields$Value[[7]][1]
  TVFSpectra$NbWavelengths = Fields$Value[[2]][1]
  TVFSpectra$NbSpectra = Fields$Value[[3]][1]
  
  # load the abscisse axes
  
  ST_indice = which(Fields$Value[[29]]==as.integer(1))
  if(Fields$Value[[26]]$Size[ST_indice]!=TVFSpectra$NbWavelengths)
  {
    tryCatch({
      status=seek(info,where=Fields$Value[[26]]$Offset[ST_indice])
      TVFSpectra$Abscisses_Axes = as.integer(readBin(info,what='raw',n=4*Fields$Value[[26]]$Size[ST_indice],size=1))
      TVFSpectra$Abscisses_Axes = decompressWvAxe(TVFSpectra$Abscisses_Axes,
                                                  TVFSpectra$NbWavelengths,
                                                  Fields$Value[[24]][ST_indice],
                                                  Fields$Value[[25]][ST_indice])
      print('decompressed wavenumber axis')},
      error=function(cond) {
        min_wavelength = Fields$Value[[24]][ST_indice] ;
        max_wavelength = Fields$Value[[25]][ST_indice];
        TVFSpectra$Abscisses_Axes = seq(min_wavelength,max_wavelength,length.out = Fields$Value[[2]][1])
        
        print('alternative')
      },
      warning=function(cond) {
        min_wavelength = Fields$Value[[24]][ST_indice] ;
        max_wavelength = Fields$Value[[25]][ST_indice];
        TVFSpectra$Abscisses_Axes = seq(min_wavelength,max_wavelength,length.out = Fields$Value[[2]][1])
        print('Warning')
        return(NULL)
      },
      finally={    min_wavelength = Fields$Value[[24]][ST_indice] ;
      max_wavelength = Fields$Value[[25]][ST_indice];
      TVFSpectra$Abscisses_Axes = seq(min_wavelength,max_wavelength,length.out = Fields$Value[[2]][1])
      })
  }else{ # if no compression
    print('no compression')
    TVFSpectra$Abscisses_Axes = Fields$Value[26]$AxesValue[ST_indice]

    
  }
  rm(tmp,tmp1,tmp11,tmp2,tmp22,tmp5)
  
  #  load the spectra
  
  
  # SpectraTypeCell = {'int16' , 'uint8' , 'int32' , 'float32' , 'float64' , 'char'} ;
  status=seek(info,Fields$Value[8])
  if(Fields$Value[5]!=1) # if compression
  {
    nb_char = as.integer(readBin(info,what='raw',n=4,size=1))
    nb_char = (((nb_char[4]*2^8 + nb_char[3]) * 2^8 + nb_char[2]) * 2^8 + nb_char[1]) ;
    TVFSpectra$Data = as.integer(readBin(info,what='raw',nb_char,size=1))
    TVFSpectra$Data = decompressLZH(TVFSpectra$Data,Fields$Value[19],TVFSpectra$NbSpectra * TVFSpectra$NbWavelengths,Fields$Value[5],Fields$Value[22],Fields$Value[23]) ;
    TVFSpectra$Data = matrix(c(TVFSpectra$Data),Fields$Value[[2]],Fields$Value[[3]]) ;
    # plot(TVFSpectra$Abscisses_Axes,TVFSpectra$Data[,1],type = 'l',xlab = 'Wavenumber',ylab = 'Intensity')
    
  }else{# if no compression
    nb_char = TVFSpectra$NbSpectra * TVFSpectra$NbWavelengths ;
    TVFSpectra$Data = readBin(info,what='raw',nb_char,size=4) ;
    TVFSpectra$Data = reshape(TVFSpectra$Data,Fields$Value[2],Fields$Value[3]) ;
  }
  
  TVFSpectra$XResolution = Fields$Value[12] ;
  TVFSpectra$YResolution = Fields$Value[13] ;
  TVFSpectra$XPosition = Fields$Value[15] ;
  TVFSpectra$YPosition = Fields$Value[16] ;
  
  # # Add some fields relative to depth and spatial dimensions
# 
  # VV = find(Fields$Value[21] != 0) ;
  # 
  # for (i in 1 : length(VV)){
  # if(find(Fields$Value[[29]][VV(i)] == 2)
  # TVFSpectra.Depth = Fields{26}.Value{VV(i)}.AxesValue ;
  # TVFSpectra.NbDepth = Fields{26}.Value{VV(i)}.Size ;
  # }else if(Fields{29}.Value(VV(i))) == 4
  # if Fields{26}.Value{VV(i)}.Size ~= 0
  # TVFSpectra.X = Fields{26}.Value{VV(i)}.AxesValue ;
  # end
  # TVFSpectra.NbX = Fields{21}.Value(VV(i)) ;
  # elseif Fields{29}.Value(VV(i)) == 5 
  # if Fields{26}.Value{VV(i)}.Size ~= 0
  # TVFSpectra.Y = Fields{26}.Value{VV(i)}.AxesValue ;
  # end
  # TVFSpectra.NbY = Fields{21}.Value(VV(i)) ;
  # end
  # end
  
  # Add properties
  
  # for indice = 1 : Fields{30}.Count
  # TVFSpectra.Properties{indice}.Name = Fields{30}.Value{indice}.Name ;
  # TVFSpectra.Properties{indice}.Value = Fields{30}.Value{indice}.Value ;
  # end
  

  closeAllConnections()
  return(TVFSpectra)
}
#############################################################
solvelog2int<-function(x){
  # solving for F and E in the function X=F.2^E
  intpow=1
  const=0
  while(const==0 || const>1){
    intpow=intpow+1;
    const=x/(2^intpow)
  }
  
  return(list(const,intpow))
}
#############################################################
bitshiftRunit16<-function(getbuf,shift){
  x=as.integer(intToBits(getbuf))
  x=x[1:16]
  x=as.integer(c(x[c((1+shift):length(x))],matrix(0,shift,1)))
  tmp=0
  for(i in 1:length(x)){
    tmp=tmp+x[i]*2^(i-1)
  }
  return(tmp)
}
#############################################################
bitshiftunit16<-function(getbuf,shift){
  x=as.integer(intToBits(getbuf))
  x=x[1:16]
  x[1:16]=rev(x[1:16])
  x=as.integer(c(x[c((1+shift):length(x))],matrix(0,shift,1)))
  x[1:16]=rev(x[1:16])
  tmp=0
  for(i in 1:length(x)){
    tmp=tmp+x[i]*2^(i-1)
  }
  return(tmp)
}

#############################################################
pow<-function(x,pow){
  for(i in 1:(pow-1)){
    if(i==1){y=x
    tmp=y}
    y=y*x}
  return(x)
}
#############################################################
update<-function(global){ 
  cc=global$cc
  
  if(global$freq_[global$R+1] == global$MAX_FREQ){
    jj = 0 ;
    for(ii in 0 : (global$SizeTab-1)){   
      
      if(global$son_[ii+1] >= global$SizeTab){
        global$freq_[jj+1] = bitshiftRunit16((global$freq_[ii+1] + 1), 1) ;
        global$son_[jj+1] = global$son_[ii+1] ;
        jj = jj + 1 ;
        
      }        
    }
    
    ii = 0 ;
    for (jj in global$N_CHAR : (global$SizeTab-1)){
      
      kk = ii + 1 ;
      global$freq_[(jj+1)] = (global$freq_[(ii+1)] + global$freq_[(kk+1)]) ;
      
      f = global$freq_[(jj+1)] ;
      kk = jj - 1 ;
      p=0
      
      while(f < global$freq_[(kk+1)]){
        kk = kk - 1 ;
      }
      kk = kk + 1 ;
      ll = (bitshiftunit16((jj -kk),1)) ;
      
      if((kk+2)<(kk+2+ll-1)){
        global$freq_[(kk+2):(kk+2+ll-1)] = global$freq_[(kk+1) : (kk+1+ll-1)] ;
        global$son_[(kk+2):(kk+2+ll-1)] = global$son_[(kk+1) : (kk+1+ll-1)] ;
      }
      global$freq_[(kk+1)] = f ;
      global$son_[(kk+1)] = (ii) ;
      ii = ii + 2 ;
      
    }
    
    for (ii in 0 : (global$SizeTab-1)){ 
      kk = global$son_[ii+1] ;
      if (kk >= global$SizeTab){

        global$prnt_[kk+1] = (ii) ;
        
      }else{

        global$prnt_[kk+2] = (ii) ;
        global$prnt_[kk+1] = (ii) ;
        
      }
    }
  }
  
  cc = global$prnt_[cc + global$SizeTab + 1] ;
  flag_one = 1 ;
  

  while ((flag_one == 1) || (cc != 0)){
    global$freq_[cc+1] = global$freq_[cc+1] + 1 ;
    
    kk = global$freq_[cc+1] ;
    
    ll = cc + 1 ;
    
    
    if(kk > global$freq_[ll + 1]){
      
      while(kk > global$freq_[ll + 1]){
        ll = ll + 1 ;
      }
      ll = ll - 1 ;
      global$freq_[cc+1] = global$freq_[ll+1] ;
      global$freq_[ll+1] = (kk) ;
      
      ii = global$son_[cc+1] ;
      global$prnt_[ii+1] =(ll) ;
      
      if(ii < global$SizeTab){
        global$prnt_[ii + 2] = (ll) ;
      }
      jj = global$son_[ll+1] ;
      global$son_[ll+1] = (ii) ;
      global$prnt_[jj+1] = (cc) ;
      if(jj < global$SizeTab){
        global$prnt_[jj+2] = (cc) ;
      }
      global$son_[cc+1] = (jj) ;
      
      cc = ll ;
      
    }
    cc = global$prnt_[cc+1] ;
    flag_one = 0 ; 
  }
  
  return(global)
}
#############################################################
getByte<-function(global){
  
  while(global$getlen <= 8){
    
    global = getb_encoded(global) ;
    ii=global$ii
    if (ii < 0){ 
      ii = 0 ;
    }
    global$getbuf = (bitwOr((global$getbuf),(bitwShiftL((ii),8-global$getlen)))) ;
    global$getlen = global$getlen + 8 ;
  }
  
  ii = global$getbuf ;
  global$getbuf = (bitshiftunit16((global$getbuf),8)) ;
  global$getlen = global$getlen - 8 ;
  
  global$ii = (bitwShiftR(ii,8)) ;
  
  return(global)
}
#############################################################
decodePosition<-function(global){
  global = getByte(global) ;
  ii=global$ii
  cc = (bitshiftunit16((global$d_code[ii+1]),6)) ;
  jj = global$d_len[ii+1] ;
  jj = jj - 2 ;
  while(jj){
    jj = jj - 1 ;
    
    global=getBit(global)
    ii = (bitwShiftL(ii , 1))+global$ii;
  }
  
  global$ab = bitwOr((cc), bitwAnd((ii) , as.integer(as.hexmode('3f')))) ;
  return(global)
}

#############################################################
put_char<-function(global,x){
  global$d_data[global$adress] = x;
  global$adress = global$adress + 1 ;
  return(global)
}
#############################################################
putb_decoded<-function(global){ 
  bb=global$cc
  if(global$pos >= global$obj_size)
  {
    global$aa = -1 ;
    return
  }
  
  if (global$floatType!=1){
    bb = global$lastbyte +  bb ;
    global$lastbyte =  bb ;
    put_char(global,bb) ;
    global$pos = global$pos + 1 ;
    
  }else{
    if(bitwAnd(global$pos,1)==1){
      bb=as.double(bitwAnd(global$lastbyte,as.integer(as.hexmode('ff'))) + bitshiftunit16(bb,8))/global$fc + global$fm[[1]]
      global=put_char(global,bb) ;
      
    }else{
      
      global$lastbyte =  bb ;
      
    }
    global$pos = global$pos + 1 ;
  }
  
  global$aa = 0 ;
  
  return(global)
}
##############################################################
getb_encoded<-function(global){
  
  if (global$cpos >= global$codsize)
  {
    global$ii = -1 
    return(global)
  }
  if(global$cfbuf >= global$BufSize){read_size = global$BufSize ;
  if ((global$codsize-global$cpos) < read_size){
    read_size = global$codsize-global$cpos ;
  }
  
  global$fbuf = global$data[matrix(c(global$current_position:(global$current_position + read_size-1)),1)] 
  global$current_position = global$current_position + read_size ;
  
  global$cfbuf = 0 ;
  }
  
  global$cpos = global$cpos + 1 ;
  global$ii = global$fbuf[global$cfbuf+1] ;
  global$cfbuf = global$cfbuf + 1 ;
  
  return(global)
}
###############################################################
getBit<-function(global){
  while(global$getlen <=8){
    global=getb_encoded(global)
    ii=global$ii
    if (ii < 0){
      ii = 0 ;}
    global$getbuf = bitwOr(global$getbuf,bitwShiftL(as.integer(ii),(8 - global$getlen)))
    global$getlen = global$getlen + 8 ;
    
  }
  
  ii = global$getbuf ;
  global$getbuf = bitshiftunit16(global$getbuf,1);
  
  global$getlen = global$getlen - 1 ;
  global$ii = bitwShiftR(as.integer(ii),15) ;
  
  return(global)
}
###############################################################
decodeChar<-function(global){
  global$cc = global$son_[global$R+1] ;
  while(global$cc<global$SizeTab){
    
    tmp=global$cc
    global=getBit(global)
    global$cc=tmp+global$ii
    global$cc = global$son_[global$cc+1]  
    
    
  }
  
  global$cc = global$cc - global$SizeTab ;
  global=update(global) ;
  
  return(global)
}
###############################################################
startHuff<-function(global)
{
  for(ii in 0 : global$N_CHAR-1){
    global$freq_[ii+1] = 1 ;
    global$son_[ii+1] = ii+global$SizeTab ;
    global$prnt_[ii+1+global$SizeTab] = ii ;
  }
  
  ii = 0 ; jj = global$N_CHAR ;
  while(jj <= global$R){
    global$freq_[jj+1] = global$freq_[ii+1] + global$freq_[ii+2] ;
    global$son_[jj+1] = ii ;
    global$prnt_[ii+2] = jj ;
    global$prnt_[ii+1] = jj ;
    ii = ii + 2 ;
    jj = jj + 1 ;
  }
  global$ii=ii
  global$freq_[global$SizeTab+1] = as.integer(as.hexmode(('FFFF'))) 
  global$prnt_[global$R+1] = 0 ;
  
  return(global)
}

###############################################################

decodeLZH<-function(global){
  
  if(global$obj_size <= 0)
  {
    global$result = -1
    return(global) 
  }
  global$text_buf = matrix(0,1,global$N+global$LaBuf) ;
  global$freq_ = matrix(0,1,global$SizeTab+2) ;
  global$son_ = matrix(0,1,global$SizeTab+1) ;
  global$prnt_ = matrix(0,1,global$SizeTab+global$N_CHAR+1) ;
  global$lson_ = matrix(0,1,global$N+2) ;
  global$rson_ = matrix(0,1,global$N+258) ;
  dad_ = matrix(0,1,global$N+2) ;
  global$getbuf = 0 ; global$getlen = 0 ;
  
  global=startHuff(global)
  
  rr = global$N-global$LaBuf ;
  global$text_buf=matrix(0,1,global$N-global$LaBuf+1)
  
  count = 0 ;
  
  while (count < global$obj_size)
  {
    
    global = decodeChar(global)
    
    
    if (global$cc < 256){
      global= putb_decoded(global)
      if (global$aa< 0)
      {
        break ;
      }
      global$text_buf[rr+1] = global$cc ;
      rr = rr + 1 ;
      rr = (bitwAnd(rr,global$N-1)) ;
      count = count + 1 ;
    }else{
      global = decodePosition(global) ;
      ab=global$ab
      ac = rr - ab - 1 ;
      ad = global$N-1 ;
      if(ac < 0){
        ii = ac + ad +1 
        
        
      }else{
        ii = bitwAnd((ac),(global$N-1))
        
        
      }
      jj = global$cc - 255 + global$THR ;
      for (kk in 0 : (jj-1)){
        
        global$cc = global$text_buf[(bitwAnd((ii+kk),(global$N-1))) + 1];
        global=putb_decoded(global)
        if (global$cc < 0){
          break ;
        }
        global$text_buf[rr+1] = global$cc ;
        rr = rr + 1 ;
        rr = (bitwAnd((rr),global$N-1)) ;
        count = count + 1 ;
        
      }
    }
    
  }
  
  
  if(count<global$obj_size){
    global$result = -1 ;
  }else{
    global$result = 0 ;
  }
  
  return(global)
}
#############


decompressLZH<-function(compress_data,data_type,data_size,decompress_method,min_intensity,max_intensity)
{
  global=list()
  global$N = 4096 ; # Size of string buffer
  global$LaBuf= 60 ; # Size of look-ahead buffer
  # # data_type = 0   =>   Int16 type
  # # data_type = 1   =>   Int8 type
  # # data_type = 2   =>   Int32 type
  # # data_type = 3   =>   Float type
  # # data_type = 4   =>   Double type
  # # data_type = 5   =>   Char type
  # # data_type = 6   =>   Int type
  # # data_type = 7   =>   Arg type
  # # data_type = 8   =>   Empty type
  
  
  
  
  # ###################################
  # # declaration of global variables #
  # ###################################
  
  
  
  
  global$current_position = 1 ; # Position of the reader pointer in the input data "compress_data"
  global$data = compress_data ; # Copy of input dataglobal$d_data = matrix(0,1,data_size); # Copy of output data "decompress_data"
  global$d_data = matrix(0,1,data_size); # Copy of output data "decompress_data"
  global$adress = 1 ; # Position of the copying pointer in the output data
  
  
  # ###################################
  # # declaration of global constants #
  # ###################################
  
  # LZSS Parameters
  
  
  NIL = global$N ; # End of tree's node
  global$THR = 2 ; # Threshold
  
  # Huffman coding parameters
  
  global$N_CHAR = 256-global$THR+global$LaBuf; # Character code (= 0..global$N_CHAR-1)
  global$SizeTab= global$N_CHAR*2-1 ; # Size of table
  global$R = global$SizeTab-1 ; # root position
  global$MAX_FREQ = 32768 ; # update when frequency reaches to this
  
  # Tables for encoding/decoding upper 6 bits of sliding dictionary pointer
  
  # Encoder table
  code=tolower(c( '00', '00', '00', '00', '00', '00', '00', '00', '00',
                  '00', '00', '00', '00', '00', '00', '00', '00', '00', '00', '00', '00',
                  '00', '00', '00', '00', '00', '00', '00', '00', '00', '00', '00', '01',
                  '01', '01', '01', '01', '01', '01', '01', '01', '01', '01', '01', '01',
                  '01', '01', '01', '02', '02', '02', '02', '02', '02', '02', '02', '02',
                  '02', '02', '02', '02', '02', '02', '02', '03', '03', '03', '03', '03',
                  '03', '03', '03', '03', '03', '03', '03', '03', '03', '03', '03', '04',
                  '04', '04', '04', '04', '04', '04', '04', '05', '05', '05', '05', '05',
                  '05', '05', '05', '06', '06', '06', '06', '06', '06', '06', '06', '07',
                  '07', '07', '07', '07', '07', '07', '07', '08', '08', '08', '08', '08',
                  '08', '08', '08', '09', '09', '09', '09', '09', '09', '09', '09', '0A',
                  '0A', '0A', '0A', '0A', '0A', '0A', '0A', '0B', '0B', '0B', '0B', '0B',
                  '0B', '0B', '0B', '0C', '0C', '0C', '0C', '0D', '0D', '0D', '0D', '0E',
                  '0E', '0E', '0E', '0F', '0F', '0F', '0F', '10', '10', '10', '10', '11',
                  '11', '11', '11', '12', '12', '12', '12', '13', '13', '13', '13', '14',
                  '14', '14', '14', '15', '15', '15', '15', '16', '16', '16', '16', '17',
                  '17', '17', '17', '18', '18', '19', '19', '1A', '1A', '1B', '1B', '1C',
                  '1C', '1D', '1D', '1E', '1E', '1F', '1F', '20', '20', '21', '21', '22',
                  '22', '23', '23', '24', '24', '25', '25', '26', '26', '27', '27', '28',
                  '28', '29', '29', '2A', '2A', '2B', '2B', '2C', '2C', '2D', '2D', '2E',
                  '2E', '2F', '2F', '30', '31', '32', '33', '34', '35', '36', '37', '38',
                  '39', '3A', '3B', '3C', '3D', '3E', '3F' ))
  
  coderange=c(0:255)
  ascii = data.frame(coderange,  as.raw(coderange) )
  colnames( ascii) <- c("dec","hex") 
  ascii$hex2=as.character(ascii$hex)
  ascii
  
  dec=matrix(0,1,length(code))
  for(i in 1:256)# length(code)
  {
    tmp=which((code %in% ascii$hex2[i]))
    dec[tmp]=ascii$dec[i]
  }
  global$d_code=dec
  
  
  # Decoder table
  global$d_len = c( 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03,
                    03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 03, 04, 04,
                    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04,
                    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04,
                    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 05, 05, 05, 05, 05, 05, 05, 05,
                    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
                    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
                    05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05,
                    05, 05, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
                    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06,
                    06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 06, 07, 07, 07, 07,
                    07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07,
                    07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07, 07,
                    07, 07, 07, 07, 07, 07, 07, 07, 08, 08, 08, 08, 08, 08, 08, 08, 08, 08,
                    08, 08, 08, 08, 08, 08 )
  
  
  #########################################
  # Declaration of other global variables #
  #########################################
  
  global$BufSize = 512 ; # Buffer size
  global$pos = 0 ;
  global$cpos = 0 ;
  global$lastbyte = 0 ;
  global$fbuf = matrix(0,1,global$BufSize) ;
  global$cfbuf = global$BufSize ;
  global$codsize = length(compress_data) ;
  global$obj_size = data_size ;
  
  
  #############################################
  # Determination of the decompression method #
  #############################################
  
  decompress_method = as.hexmode(decompress_method[[1]][1])
  switch(decompress_method,
         '1005'={global$LaBuf= 60},
         '1006'={global$LaBuf= 14},
         {    error=function(cond) {return(NA)}}
  )
  
  
  
  # ###############################################################
  # # Fixation of algorithm parameters depending on the data type #
  # ###############################################################
  
  if ((data_type != 1) && (data_type != 5)) # data_type ~= int8 & data_type ~= char
  {X = 65534/(max_intensity[[1]][1] - min_intensity[[1]][1]);
  global$fm = min_intensity ;
  FF = unlist(solvelog2int(X)) ;
  xp=(FF[2])
  global$fc=2.^(xp-1)
  # global$fc = pow2(1,xp-1) ;
  global$obj_size = global$obj_size * 2 ;
  global$floatType = 1 ;
  }else{
    global$floatType = 0 ;
  }
  
  # #########################
  # # Decompression of data #
  # #########################
  
  global = decodeLZH(global) 
  
  switch(global$result,
         '-1'={error=function(cond) {print('Read error LZH')}},
         '-2'={error=function(cond) {print('Memory allocation error')}}
  )
  decompress_data=global$d_data
  return(decompress_data)
}

decompressWvAxe<-function(compress_axe,axe_size,fmin,fmax){
  
  # # decompressWvAxe is used to decompress wavelength axe from a TVF Labspec
  # # file.
  # #
  # # decompress_axe = decompressWvAxe(compress_axe,axe_size,fmin,fmax)
  # # input :  compress_axe = compressed wavelength axe extracted from the TVF Labspec file
  # #                       (see tvfreadone.m for more details)
  # #          axe_size     = size of the final decompressed wavelength axe
  # #          fmin         = minimal wavelength
  # #          fmax         = maximal wavelength
  # # output : decompress_axe = final decompressed wavelength axe
  
  
  FreqResolution= 200000 ;
  II = which(compress_axe > 128) ;
  
  compress_axe[II] = compress_axe[II]-256;
  
  masi = compress_axe ;
  tsize = masi[1] ;
  
  step = floor(FreqResolution/axe_size) ;
  
  masi = compress_axe[5:length(compress_axe)] ;
  masb = masi ;
  
  fmax=(fmax-fmin)/step/(axe_size-1);
  
  cur = 0 ;
  
  for( i in  0 : c(axe_size - 1) ){
    if (tsize > 1 ){
      cur = cur + masi[i+1] ;
    }else{
      cur = cur + masb[i+1] ;
    }
    compress_axe[i+1]=fmax*cur+fmin;
    
    cur = cur + step ;
  }
  
  
  decompress_axe = compress_axe[1:axe_size] ;
  
  return(decompress_axe)
}
########################################################

