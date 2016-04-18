library(mapproj)
compressincome<-function(incomedata){
  ZIPCODE<-names(table(unique(incomedata[,1])))
  Averageincome<-NULL
  for (index in 1:length(ZIPCODE)){
    workar<-incomedata[,1]==ZIPCODE[index]
    numberofpeople<-sum(incomedata[workar,]$N1)
    totalincome<-sum(incomedata[workar,]$A00100)
    Averageincome<-c(Averageincome,totalincome/numberofpeople)
  }
  return(data.frame(ZIPCODE,Averageincome))
}

findafforablity<-function(q,ga){
  fav<-merge(ga,q,by='ZIPCODE')
  fav$affordabl=fav$income/fav$coffecient
  names(fav)[1] <- "zip"
  return(fav)
}

getincomeincreaseforstate<-function(statename){
  irs11<-read.csv("11zpallagi.csv")
  irs12<-read.csv("12zpallagi.csv")
  irs13<-read.csv("13zpallagi.csv")
  CA11=irs11[irs11$STATE==statename,c("ZIPCODE","N1","A00100")]
  CA12=irs12[irs12$STATE==statename,c("zipcode","N1","A00100")]
  CA13=irs13[irs13$STATE==statename,c("zipcode","N1","A00100")]
  CAA11<-compressincome(CA11)
  CAA12<-compressincome(CA12)
  CAA13<-compressincome(CA13)
  names(CAA12)[2] <- "IRS12"
  names(CAA13)[2] <- "IRS13"
  da<-merge(CAA11,CAA12, by='ZIPCODE')
  da<-merge(da,CAA13, by='ZIPCODE')
  ga<-findcoffience(da)
  names(ga)[1] <- "ZIPCODE"
  names(ga)[2] <- "income increase"
  return(ga)
}



findcoffience<-function(timeseries){
  #assume zip is the first entery
  x<-2:length(timeseries)
  coffecient<-NULL
  for (index in 1:length(timeseries[,1])){
    a<-lm(unlist(timeseries[index,x])~x)
    coffecient<-c(coffecient,a$coefficients[2])
  }
  
  return(data.frame(timeseries$ZIPCODE,coffecient))
}

getreadytoplot<-function(zipcode,state,fav){
  zipca<-zipcode[zipcode$state==state,]
  qq<-mapproject(zipca$longitude,zipca$latitude,projection = "cylindrical")
  zipca$x<-qq$x
  zipca$y<-qq$y
  cain<-merge(fav,zipca,by="zip")
  cain$col="Black"
  cain[cain$affordabl>mean(cain$affordabl),]$col="blue"
  cain[cain$affordabl<mean(cain$affordabl)-sd(cain$affordabl),]$col="red"
  return(cain)
  
}

ziphousing1<-read.csv("Zip_MedianListingPrice_Sfr.csv")
names(ziphousing1)[names(ziphousing1)=="RegionName"] <- "ZIPCODE"
zip2<-ziphousing1[,c(1,7:80)]
housingrateofincrease<-findcoffience(zip2)
names(housingrateofincrease)[1] <- "ZIPCODE"
incomeca<-getincomeincreaseforstate("CA")
caaffordability<-findafforablity(incomeca,housingrateofincrease)
caplotdata<-getreadytoplot(zipcode,"CA",caaffordability)
plot(caplotdata$x,caplotdata$y,col=caplotdata$col,xlab="Longitude",ylab="Latitude")

incomeny<-getincomeincreaseforstate("NY")
nyaffordability<-findafforablity(incomeny,housingrateofincrease)
nyplotdata<-getreadytoplot(zipcode,"NY",nyaffordability)
plot(nyplotdata$x,nyplotdata$y,col=nyplotdata$col,xlab="Longitude",ylab="Latitude")





