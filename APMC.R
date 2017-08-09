rm(list = ls())
library(xlsx)
library(zoo)
library(forecast)

## Importing Data
monthly_data<-read.csv(paste0(getwd(),"/Monthly_data_cmo.csv"))
MSP_Data<-read.csv(paste0(getwd(),"/CMO_MSP_Mandi.csv"))

#### Data Cleaning
## ordering data based on comodities
MSP_Data<-MSP_Data[order(MSP_Data$commodity),]
#### We find the number of commodities for which MSP is given
## Checking unique commodities for which MSP is given
unique(MSP_Data$commodity)
## converting all to lower case
MSP_Data$commodity<-tolower(MSP_Data$commodity)
unique(MSP_Data$commodity)
## we have in all 32 commodities but sugarcane is given as with a hyphen in one of the rows
## removing that
MSP_Data$commodity[which(MSP_Data$commodity == "sugar-cane")]<-"sugarcane"
MSP_Comodity<-unique(MSP_Data$commodity)

### Sorting the Data
monthly_data<-monthly_data[order(monthly_data$Commodity),]
### Getting unique commodities
unique(monthly_data$Commodity)
### We see that many values are differentiated because of case of work
monthly_data$Commodity<-tolower(monthly_data$Commodity)
Monthly_comodity<-unique(monthly_data$Commodity)
index<-1
### Getting common comodities
common_comodities<-c()
for(i in Monthly_comodity){
  for(j in MSP_Comodity){
    if(i == j){
      common_comodities[index]<-i
      index<-index+1
    }
  }
}

### Getting data with common comodities
final_monthly_data<-c()
for(i in common_comodities){
  final_monthly_data<-rbind(final_monthly_data,monthly_data[which(monthly_data$Commodity == i),])  
}
#### Merging the data
final_monthly_data$MSP<-NA
for(i in  common_comodities){
    for (j in unique(final_monthly_data$Year)){
        final_monthly_data[which(final_monthly_data$Commodity == i & final_monthly_data$Year == j),12]<-MSP_Data[which(MSP_Data$commodity == i & MSP_Data$year == j),4]       
      }
}
## Tesing hypothests H0: Modal price = MSP
## we calc the avarage modal price and SD for each comodity in a mandi in a specific year
index<-1
mean_values<-c()
final_monthly_data$annual_mean<-NA
final_monthly_data$annual_SD<-NA
for(j in unique(final_monthly_data$Commodity)){
  temp<-final_monthly_data[which(final_monthly_data$Commodity == j),]
  for (i in unique(temp$Year)){
    temp<-temp[which(temp$Year == i),]
    for (k in unique(temp$APMC)){
      temp<-temp[which(as.character(temp$APMC) == k),]
      final_monthly_data[which(final_monthly_data$Year == i & final_monthly_data$Commodity == j &  final_monthly_data$APMC == k),13]<-mean(temp$modal_price)
      final_monthly_data[which(final_monthly_data$Year == i &  final_monthly_data$Commodity == j & final_monthly_data$APMC == k),14]<-sd(temp$modal_price)
      }
  }
}


### This is done to draw out seasonal components in the data.
#temp$date<-as.Date(as.character(temp$date) , format = "%Y-%mm")
temp<-monthly_data[order(temp$date),]
temp<-temp[order(temp$Commodity),]
temp<-temp[order(temp$APMC),]
plot(x= temp$date[which(temp$Commodity =="Paddy-Unhusked")], y= temp$modal_price[which(temp$Commodity =="Paddy-Unhusked")])
temp_paddy<-temp[which(temp$Commodity =="Paddy-Unhusked"),]
temp_paddy_Aamgaon<-temp[which(temp$APMC == "Aamgaon" & temp$Commodity =="Paddy-Unhusked"),]
plot.ts(temp_paddy_Aamgaon$modal_price)
acf(temp_paddy_Aamgaon$modal_price)
pacf(temp_paddy_Aamgaon$modal_price)
auto.arima(temp_paddy_Aamgaon$modal_price)

temp$Commodity<-tolower(temp$Commodity)
unique(temp$Commodity)



for(i in unique(temp$Commodity)){
  for(j in unique(temp$date)){
        
      
  }
}




which(is.na(temp$modal_price))
for(i in unique(temp$Commodity)){
  temp_1<-temp[which(temp$Commodity == i),]
  for(j in unique(temp_1$APMC)){
    temp_2<-temp[which(temp$APMC == j),8]
    boxplot(temp_2, main = paste(i,j))
  }
}
for(i in unique(temp$Commodity)){
  temp_1<-temp[which(temp$Commodity == i),]
  for(j in unique(temp_1$APMC)){
    temp_2<-temp[which(temp$APMC == j),]
    pacf(temp_2$modal_price, main = paste(i,j))
      }
}

