readfsm<-function(filename){
library(abind)

# function [data, xAxis, yAxis, zAxis, misc] = fsmload(filename)
# # Reads in IR image data from PerkinElmer block structured files.
# # This version is compatible with '1994' standard FSM files.
# # FSM files have 3 constant data intervals.
# #
# # [data, xAxis, yAxis, zAxis, misc] = fsmload(filename):
#   #   data:  3D array, [Y, X, Z]
# #   xAxis: vector for horizontal axis (e.g. micrometers)
# #   yAxis: vector for vertical axis (e.g. micrometers)
# #   zAxis: vector for wavenumber axis (e.g. cm-1)
# #   misc: miscellanous information in name,value pairs
# 
# # Copyright (C)2007 PerkinElmer Life and Analytical Sciences
# # Stephen Westlake, Seer Green
# #
# # History
# # 2007-04-24 SW     Initial version

# Block IDs
DS4C3IData            = toString(5100);   # 4D DS: header info
DS4C3IPts             = toString(5101);   # 4D DS: point coordinates (CvCoOrdArray)
DS4C3IPtsCont         = toString(5102);   # 4D DS: point coordinates (continuator for large CvCoOrdArray)
DS4C3ITemp2DData      = toString(5103);   # 4D DS: temp block for storing part of component 2d dataset
DS4C3I2DData          = toString(5104);   # 4D DS: release sw block for storing ALL of component 2d data
DS4C3IFloatPts        = toString(5105);   # 4D DS: point coordinates (FloatArray)
DS4C3IFloatPtsCont    = toString(5106);   # 4D DS: point coordinates (continuator for large FloatArray)
DS4C3IHistory         = toString(5107);   # 4D DS: history record


# Open file for reading
fid=file(filename, open = "rb", blocking = TRUE,
          encoding = getOption("encoding"), raw = TRUE)

# Fixed file header of signature and description
sig=rawToChar(readBin(fid, what='raw',n=4,size = 1))

 if(identical(sig, 'PEPE')==FALSE){
 stop('This is not a PerkinElmer block structured file.');
 return
 }
description = rawToChar(readBin(fid, what='raw',n=40,size = 1))

# Initialize a variable so we can tell if we have read it.
xLen = c(0);
seek(fid, where = NA, origin = "current")

specnumber=1
# The rest of the file is a list of blocks
while((lng=length(readBin(fid, what='int',n=1 ,size = 1)))>0){
seek(fid, where = -lng, origin = "current")
blockID = readBin(fid, what='int',n=1 ,size = 2);
blockSize = readBin(fid, what='int',n=1 ,size = 4);
# # feof does not go true until after the read has failed.
# if( feof(fid)){
# break
# }

blockID=as.character(blockID)
switch(blockID,'5100'={
len = readBin(fid, what='int',n=1 ,size = 2);
alias_n=rawToChar(readBin(fid, what='raw',n=len))
# print('sw1')
# scan(quiet = TRUE)

               
               xDelta = readBin(fid, what='double',n=1,);
               yDelta = readBin(fid, what='double',n=1);
               zDelta = readBin(fid, what='double',n=1);
               readBin(fid, what='double',n=1);      # zstart
               readBin(fid, what='double',n=1);      # zend    
               readBin(fid, what='double',n=1);      # min data value
               readBin(fid, what='double',n=1);      # max data value
               x0 = readBin(fid, what='double',n=1);
               y0 = readBin(fid, what='double',n=1);
               z0 = readBin(fid, what='double',n=1);
               xLen = readBin(fid, what='integer',n=1,size=4);
               yLen = readBin(fid, what='integer',n=1,size=4);
               zLen = readBin(fid, what='integer',n=1,size=4);            
               
                len = readBin(fid, what='integer',n=1,size=2);
                xLabel = rawToChar(readBin(fid, what='raw',n=len));
                len = readBin(fid, what='integer',n=1,size=2);
                yLabel = rawToChar(readBin(fid, what='raw',n=len));
                len = readBin(fid, what='integer',n=1,size=2);
                zLabel = rawToChar(readBin(fid, what='raw',n=len));
                len = readBin(fid, what='integer',n=1,size=2);
                wLabel = rawToChar(readBin(fid, what='raw',n=len));
                
                # matlab row 1 is the top, i.e. fsm row N.
                y = yLen;
                x = 1;
 },
'5105'={
#   print('sw2')
#   scan(quiet = TRUE)
  # the next spectrum
                if(specnumber==1){data=array(0,c(xLen,yLen,zLen))}
  data[x,y,]=readBin(fid, n=zLen, what='double',size=4)
  specnumber=specnumber+1
                # set up the index for the next point
                x = x + 1;
                if(x > xLen){
                x = 1;
                y = y - 1;
                }
},
{
#   print('sw3')
#   scan(quiet = TRUE)
      # unknown block, just seek past it
    seek(fid, where = blockSize, origin = "current")}
)
}
closeAllConnections();
                
                if(xLen == 0){
                stop('The file does not contain spectral image data.');
                return
                }
                
                # Expand the axes specifications into vectors
                # Y axis is reversed to match the image
                xEnd = x0 + (xLen - 1) * xDelta;
                yEnd = y0 + (yLen - 1) * yDelta;
                zEnd = z0 + (zLen - 1) * zDelta;
                xAxis = seq(x0, xEnd, xDelta);
                yAxis = seq(yEnd, y0, -yDelta);
                zAxis = seq(z0, zEnd,zDelta);
                
                # Return the other details as name,value pairs
                misc=matrix(0,1,5)
                misc[1] = xLabel;
                misc[2] = yLabel;
                misc[3] = zLabel;
                misc[4] = wLabel;
                misc[5] = alias_n;
                colnames(misc)=c('xlabel','ylabel','zlabel','wlabel','alias')
                ans=list('data','xaxis','yaxis','zaxis','misc')
                ans$data=data
                ans$xAxis=xAxis
                ans$yAxis=yAxis
                ans$zAxis=zAxis
                ans$misc=misc
                return(ans)
}
                