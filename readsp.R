readsp=function(filename){
  options(error=traceback)
# Reads in spectra from PerkinElmer block structured files.
# This version supports 'Spectrum' SP files.
# Note that earlier 'Data Manager' formats are not supported.
#
# [data, xAxis, misc] = spload(filename):
#   data:  1D array of doubles
#   xAxis: vector for abscissa (e.g. Wavenumbers).
#   misc: miscellanous information in name,value pairs

# Copyright (C)2007 PerkinElmer Life and Analytical Sciences
# Stephen Westlake, Seer Green
#
# History
# 2007-04-24 SW     Initial version

# Block IDs
DSet2DC1DIBlock               =  '120';
HistoryRecordBlock            =  '121';
InstrHdrHistoryRecordBlock    =  '122';
InstrumentHeaderBlock         =  '123';
IRInstrumentHeaderBlock       =  '124';
UVInstrumentHeaderBlock       =  '125';
FLInstrumentHeaderBlock       =  '126';
# Data member IDs
DataSetDataTypeMember              =  '-29839';
DataSetAbscissaRangeMember         =  '-29838';
DataSetOrdinateRangeMember         =  '-29837';
DataSetIntervalMember              =  '-29836';
DataSetNumPointsMember             =  '-29835';
DataSetSamplingMethodMember        =  '-29834';
DataSetXAxisLabelMember            =  '-29833';
DataSetYAxisLabelMember            =  '-29832';
DataSetXAxisUnitTypeMember         =  '-29831';
DataSetYAxisUnitTypeMember         =  '-29830';
DataSetFileTypeMember              =  '-29829';
DataSetDataMember                  =  '-29828';
DataSetNameMember                  =  '-29827';
DataSetChecksumMember              =  '-29826';
DataSetHistoryRecordMember         =  '-29825';
DataSetInvalidRegionMember         =  '-29824';
DataSetAliasMember                 =  '-29823';
DataSetVXIRAccyHdrMember           =  '-29822';
DataSetVXIRQualHdrMember           =  '-29821';
DataSetEventMarkersMember          =  '-29820';
# Type code IDs
ShortType               = '29999';
UShortType              = '29998';
IntType                 = '29997';
UIntType                = '29996';
LongType                = '29995';
BoolType                = '29988';
CharType                = '29987';
CvCoOrdPointType        = '29986';
StdFontType             = '29985';
CvCoOrdDimensionType    = '29984';
CvCoOrdRectangleType    = '29983';
RGBColorType            = '29982';
CvCoOrdRangeType        = '29981';
DoubleType              = '29980';
CvCoOrdType             = '29979';
ULongType               = '29978';
PeakType                = '29977';
CoOrdType               = '29976';
RangeType               = '29975';
CvCoOrdArrayType        = '29974';
EnumType                = '29973';
LogFontType             = '29972';

 
 # Open file for reading
 fid=file(filename, open = "rb", blocking = TRUE,
          encoding = getOption("encoding"), raw = TRUE)


# Fixed file header of signature and description
signature=rawToChar(readBin(fid, what='raw',n=4,size = 1))
 if(identical(signature, 'PEPE')==FALSE){
   stop('This is not a PerkinElmer block structured file.');
   return
 }
 description = rawToChar(readBin(fid, what='raw',n=40,size = 1))

# Initialize a variable so we can tell if we have read it.
xLen = c(0);
seek(fid, where = NA, origin = "current")

# The rest of the file is a list of blocks
while((lng=length(readBin(fid, what='int',n=1 ,size = 1)))>0){
  seek(fid, where = -lng, origin = "current")
  blockID = readBin(fid, what='int',n=1 ,size = 2)
  blockSize = readBin(fid, what='int',n=1 ,size = 4)


blockID=as.character(blockID)

# scan(quiet=TRUE)
switch(blockID,
       
'120'={},
# Wrapper block.  Read nothing.

'-29838'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
#_ASSERTE(CvCoOrdRangeType == nInnerCode);
x0 = readBin(fid, what='double',n=1 )
xEnd = readBin(fid, what='double',n=1 )
},

'-29836'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
xDelta = readBin(fid, what='double',n=1 )
},

'-29835'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
xLen = readBin(fid, what='int',n=1 ,size = 4)
},

'-29833'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
len = readBin(fid, what='int',n=1 ,size = 2)
xLabel = rawToChar(readBin(fid, what='raw',n=len))
},
                
'-29832'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
len = readBin(fid, what='int',n=1 ,size = 2)
yLabel = rawToChar(readBin(fid, what='raw',n=len))
},

'-29823'={
  innerCode = readBin(fid, what='int',n=1 ,size = 2)
  len = readBin(fid, what='int',n=1 ,size = 2)
  alias = rawToChar(readBin(fid, what='raw',n=len))
  },
               
'-29827'={
innerCode = readBin(fid, what='int',n=1 ,size = 2)
len = readBin(fid, what='int',n=1 ,size = 2)
originalName = rawToChar(readBin(fid, what='raw',n=len))

},  


'-29828'={
  innerCode = readBin(fid, what='int',n=1 ,size = 2)
  len = readBin(fid, what='int',n=1 ,size = 4)
# innerCode should be CvCoOrdArrayType
# len should be xLen * 8
if(xLen== 0){
xLen = len / 8;
}
data = readBin(fid, what='double',n=xLen )
},

# unknown block, just seek past it
{
seek(fid,  where = blockSize, origin="current")}
)}
closeAllConnections();

if(xLen == 0){
stop('The file does not contain spectral data.');
return
}

# Expand the axes specifications into vectors
xAxis = seq(x0, xEnd,xDelta)

# Return the other details as name,value pairs
misc=list('xLabel','ylabel','alias','original name')
misc$xLabel=c('xLabel', xLabel)
misc$yLabel=c('yLabel', yLabel)
misc$alisa= c('alias', alias)
misc$originalname=c('originalname', originalName)

ans=list('data','xaxis','misc')
ans$data=data
ans$xAxis=xAxis
ans$misc=misc
return(ans)}
