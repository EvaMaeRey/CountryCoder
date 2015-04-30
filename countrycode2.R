setwd("C:\\Users\\ereynol4.UOFI\\00Current\\0MyCountryCoder\\")

library(stringr)
CountyNamer=read.csv("MyCountryNamer.csv")
head(CountyNamer)

FavoriteCountryName=function(Vector, PrintMatches=TRUE){  
  
  
  OutVector=rep(NA,length(Vector))  
  for (i in 1:length(Vector))  {
    
    OutVector[i]=as.character(CountyNamer$CountryReturnShort[which.min(
      adist(tolower(Vector[i]), CountyNamer$CountryInput))])
    
  }
  
  ####
  UniqueInputs=unique(Vector)
  Out=rep(NA,length(UniqueInputs))  
  
  for (i in 1:length(UniqueInputs))  {
    
    Out[i]=as.character(CountyNamer$CountryReturnShort[which.min(
      adist(tolower(UniqueInputs[i]), CountyNamer$CountryInput))])
    
  }
  Unique.Inputs.Truncated=substr(UniqueInputs,1,30)
  print("The unique input and output where input and output do not match are:")
  if(PrintMatches==TRUE){
  print(data.frame(Unique.Inputs.Truncated,Out)[str_trim(tolower(UniqueInputs))!=tolower(Out),])}
  return(OutVector)
}