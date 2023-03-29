library(zoo)

#set directory
setwd("~/Documents/GitHub/exchange_rates_balance_sheet/Data")

monetary_data <- read.csv("monetary_data.csv", stringsAsFactors=FALSE)

monetary_data <- monetary_data[,-1]

#date var
monetary_data$date <- paste(monetary_data$year,
                            monetary_data$month,
                            sep="-")

monetary_data$date2 <- as.Date(as.yearmon(paste(monetary_data$date)))
                              
monetary_data <- monetary_data[order(monetary_data$date2),]

# Addition of FRED ER for Argentina
# arg ER data is missing for 2009.1-.8 then will replace the entire series from FRED
library(quantmod)
getSymbols('ARGCCUSMA02STM',src='FRED')
er_arg <- ARGCCUSMA02STM
colnames(er_arg) <- "exchange_fred"
er_arg <- as.data.frame(er_arg)
er_arg$date2 <- as.Date(rownames(er_arg))
sapply(er_arg, class)

er_arg <- subset(er_arg, date2>="2002-01-01" & date2<="2019-12-01")

monetary_data <- merge(monetary_data, er_arg, by=c("date2"))

monetary_data$exchange <- monetary_data$exchange_fred

monetary_data <- monetary_data[,-11]


#create data
#convertion exchange rate: using monetary base only
monetary_data$convertion1 <- monetary_data$m0/monetary_data$reserves
#convertion exchange rate: using all CB liabilities
monetary_data$convertion2 <- (monetary_data$m0+monetary_data$notas.bcra)/monetary_data$reserves
#notas.bcra in dollars
monetary_data$notas.bcra.dollars <- monetary_data$notas.bcra/monetary_data$exchange
#monetary base in dollars
monetary_data$m0.dollars <- monetary_data$m0/monetary_data$exchange

monetary_data$m0.frac <- monetary_data$m0.dollars/(monetary_data$m0.dollars +
                                                     monetary_data$notas.bcra.dollars)

monetary_data$notas.bcra.frac <- monetary_data$notas.bcra.dollars/(monetary_data$m0.dollars +
                                                           monetary_data$notas.bcra.dollars)

#save file
write.csv(monetary_data, file = "arg_cer.csv")

