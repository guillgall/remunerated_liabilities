library(openxlsx)
library(reshape2)


#set directory
setwd("~/Documents/GitHub/exchange_rates_balance_sheet/Data")

files <- list.files(pattern='[.]xlsx')

monetary_data <- read.xlsx("Base_monetaria.xlsx")
monetary_data <- monetary_data[,1:2]
colnames(monetary_data)[2] <- "date"

i <- "Base_monetaria.xlsx"

for (i in files){
  temp <- read.xlsx(get("i"))
  temp <- temp[,2:3]
  colnames(temp)[1:2] <- c("date", print(i))
  
  monetary_data <- merge(monetary_data, temp, all=TRUE, by="date")
}

monetary_data <- monetary_data[-c(1:2),-2]

monetary_data <- cbind(monetary_data,
                       colsplit(monetary_data[,1], "(?<=\\p{L}) (?=[\\d+$])", c("month", "year")))


monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Enero", "01", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Febrero", "02", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Marzo", "03", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Abril", "04", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Mayo", "05", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Junio", "06", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Julio", "07", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Agosto", "08", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Septiembre", "09", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Octubre", "10", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Noviembre", "11", monetary_data[,grep("month", colnames(monetary_data))])
monetary_data[,grep("month", colnames(monetary_data))] <- gsub("Diciembre", "12", monetary_data[,grep("month", colnames(monetary_data))])

#

#monetary_data <- monetary_data[order(monetary_data$year, 
#                                     monetary_data$month),]

monetary_data <- monetary_data[,-1]

colnames(monetary_data)[1:6] <- c("m0", 
                                  "informal_er",
                                  "notas.bcra", 
                                  "reserves", 
                                  "badlar", 
                                  "exchange") 

#save file
write.csv(monetary_data, file = "monetary_data.csv")
