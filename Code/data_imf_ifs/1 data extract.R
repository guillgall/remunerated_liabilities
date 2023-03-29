library(ggplot2)

#set directory
setwd("~/Desktop")

cb <- read.csv("ifs_bc_monthly.csv", stringsAsFactors=FALSE)

#rearrange data

cb2 <- as.data.frame(matrix(ncol=17, nrow=1))

colnames(cb2) <- c("asset1.claims_on_nonresid",
                   "asset2.claims_on_otherdepos",
                   "asset3.netclaims_on_centgovt",
                   "a.liabilities_to_nonresid",
                   "b.monetary_base",
                   "c.other_liabilities",
                   "d.deposits",
                   "e.loans",
                   "f.derivatives",
                   "g.shares",
                   "h.other_items",
                   "reserves",
                   "exchange",
                   "broad_money",
                   "interest_rate",
                   "period",
                   "country")

countries <- unique(cb$Country.Name)

i <- "Chile"


for (i in countries){
  
  temp <- subset(cb, Country.Name==i)
  
  #in case there is a repeated row:
  temp <- temp[-which(duplicated(temp$Indicator.Code)), ]
  
  #in case a country has less than the 9 variables
  
  if (nrow(temp)==15){
    #transpose
    tempt <- as.data.frame(t(temp))
    
    colnames(tempt) <- c("asset1.claims_on_nonresid",
                         "asset2.claims_on_otherdepos",
                         "asset3.netclaims_on_centgovt",
                         "a.liabilities_to_nonresid",
                         "b.monetary_base",
                         "c.other_liabilities",
                         "d.deposits",
                         "e.loans",
                         "f.derivatives",
                         "g.shares",
                         "h.other_items",
                         "reserves",
                         "exchange",
                         "broad_money",
                         "interest_rate")
    
    tempt <- tempt[-c(1:6),]
    
    
    sapply(tempt, class)
    
    #to numeric
    
    for (k in 1:(ncol(tempt))){
      tempt[,k] <- as.numeric(as.character(tempt[,k]))
    }
    
    tempt$period <- as.character(seq(as.Date("2002/1/1"), by = "month", length.out = nrow(tempt)))
    
    tempt$country <- i
    
    cb2 <- rbind(cb2, tempt)
    
    print(paste(i, "Full Data", sep=": ")) 
  }
  
  print(paste(i, "Not Full Data", sep=": ")) 
  
}

cb2$period <- as.Date(cb2$period)

#remunerated liabilities
cb2$remunerated_liabilities <- cb2$c.other_liabilities +
                               cb2$d.deposits +
                               cb2$e.loans + 
                               cb2$f.derivatives

#convertion ER
cb2$cer_base <- cb2$b.monetary_base/cb2$reserves

cb2$cer_full <- (cb2$b.monetary_base+cb2$remunerated_liabilities)/cb2$reserves

cb2 <- cb2[-1,]

write.csv(cb2, file = "~/Documents/GitHub/QT/Data/Output/cer.csv")

##QUARTERLY
cb <- read.csv("ifs_bc_quarterly.csv", stringsAsFactors=FALSE)

#rearrange data

cb2 <- as.data.frame(matrix(ncol=19, nrow=1))

colnames(cb2) <- c("asset1.claims_on_nonresid",
                   "asset2.claims_on_otherdepos",
                   "asset3.netclaims_on_centgovt",
                   "a.liabilities_to_nonresid",
                   "b.monetary_base",
                   "c.other_liabilities",
                   "d.deposits",
                   "e.loans",
                   "f.derivatives",
                   "g.shares",
                   "h.other_items",
                   "reserves",
                   "exchange",
                   "broad_money",
                   "interest_rate",
                   "gdp",
                   "gdp_deflator",
                   "period",
                   "country")

countries <- unique(cb$Country.Name)

i <- "Chile"


for (i in countries){
  
  temp <- subset(cb, Country.Name==i)
  
  #in case there is a repeated row:
  temp <- temp[-which(duplicated(temp$Indicator.Code)), ]
  
  #in case a country has less than the 11 variables
  
  if (nrow(temp)==17){
    #transpose
    tempt <- as.data.frame(t(temp))
    
    colnames(tempt) <- c("asset1.claims_on_nonresid",
                         "asset2.claims_on_otherdepos",
                         "asset3.netclaims_on_centgovt",
                         "a.liabilities_to_nonresid",
                         "b.monetary_base",
                         "c.other_liabilities",
                         "d.deposits",
                         "e.loans",
                         "f.derivatives",
                         "g.shares",
                         "h.other_items",
                         "reserves",
                         "exchange",
                         "broad_money",
                         "interest_rate",
                         "gdp",
                         "gdp_deflator")
    
    tempt <- tempt[-c(1:6),]
    
    
    sapply(tempt, class)
    
    #to numeric
    
    for (k in 1:(ncol(tempt))){
      tempt[,k] <- as.numeric(as.character(tempt[,k]))
    }
    
    tempt$period <- as.character(seq(as.Date("2002/1/1"), by = "quarter", length.out = nrow(tempt)))
    
    tempt$country <- i
    
    cb2 <- rbind(cb2, tempt)
    
    print(paste(i, "Full Data", sep=": ")) 
  }
  
  print(paste(i, "Not Full Data", sep=": ")) 
  
}

cb2$period <- as.Date(cb2$period)

#remunerated liabilities
cb2$remunerated_liabilities <- cb2$c.other_liabilities +
                               cb2$d.deposits +
                               cb2$e.loans + 
                               cb2$f.derivatives

#convertion ER
cb2$cer_base <- cb2$b.monetary_base/cb2$reserves

cb2$cer_full <- (cb2$b.monetary_base+cb2$remunerated_liabilities)/cb2$reserves

cb2 <- cb2[-1,]

write.csv(cb2, file = "~/Documents/GitHub/QT/Data/Output/cer_quarterly.csv")

##ANNUAL
cb <- read.csv("ifs_bc_annual.csv", stringsAsFactors=FALSE)

#rearrange data

cb2 <- as.data.frame(matrix(ncol=19, nrow=1))

colnames(cb2) <- c("asset1.claims_on_nonresid",
                   "asset2.claims_on_otherdepos",
                   "asset3.netclaims_on_centgovt",
                   "a.liabilities_to_nonresid",
                   "b.monetary_base",
                   "c.other_liabilities",
                   "d.deposits",
                   "e.loans",
                   "f.derivatives",
                   "g.shares",
                   "h.other_items",
                   "reserves",
                   "exchange",
                   "broad_money",
                   "interest_rate",
                   "gdp",
                   "gdp_deflator",
                   "period",
                   "country")

countries <- unique(cb$Country.Name)

i <- "Chile"


for (i in countries){
  
  temp <- subset(cb, Country.Name==i)
  
  #in case there is a repeated row:
  temp <- temp[-which(duplicated(temp$Indicator.Code)), ]
  
  #in case a country has less than the 11 variables
  
  if (nrow(temp)==17){
    #transpose
    tempt <- as.data.frame(t(temp))
    
    colnames(tempt) <- c("asset1.claims_on_nonresid",
                         "asset2.claims_on_otherdepos",
                         "asset3.netclaims_on_centgovt",
                         "a.liabilities_to_nonresid",
                         "b.monetary_base",
                         "c.other_liabilities",
                         "d.deposits",
                         "e.loans",
                         "f.derivatives",
                         "g.shares",
                         "h.other_items",
                         "reserves",
                         "exchange",
                         "broad_money",
                         "interest_rate",
                         "gdp",
                         "gdp_deflator")
    
    tempt <- tempt[-c(1:6),]
    
    
    sapply(tempt, class)
    
    #to numeric
    
    for (k in 1:(ncol(tempt))){
      tempt[,k] <- as.numeric(as.character(tempt[,k]))
    }
    
    tempt$period <- as.character(seq(as.Date("2002/1/1"), by = "year", length.out = nrow(tempt)))
    
    tempt$country <- i
    
    cb2 <- rbind(cb2, tempt)
    
    print(paste(i, "Full Data", sep=": ")) 
  }
  
  print(paste(i, "Not Full Data", sep=": ")) 
  
}

cb2$period <- as.Date(cb2$period)

#remunerated liabilities
cb2$remunerated_liabilities <- cb2$c.other_liabilities +
                               cb2$d.deposits +
                               cb2$e.loans + 
                               cb2$f.derivatives

#convertion ER
cb2$cer_base <- cb2$b.monetary_base/cb2$reserves

cb2$cer_full <- (cb2$b.monetary_base+cb2$remunerated_liabilities)/cb2$reserves

cb2 <- cb2[-1,]

write.csv(cb2, file = "~/Documents/GitHub/QT/Data/Output/cer_annual.csv")




