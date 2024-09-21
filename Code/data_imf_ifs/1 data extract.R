library(tidyverse)
library(ggplot2)

#set directory
setwd("~/Desktop/Data/IMF IFS")

cb <- read.csv("ifs_bc_monthly.csv", stringsAsFactors=FALSE)

#rearrange data

cb2 <- as.data.frame(matrix(ncol=18, nrow=1))

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
                   "cb_foreign_debt",
                   "period",
                   "country")

countries <- unique(cb$Country.Name)

i <- "Chile"


for (i in countries){
  
  temp <- subset(cb, Country.Name==i)
  
  # Replace empty entries with NA
  temp[temp == ""] <- NA
  
  # Count the number of missing values in each row and store in a new column
  temp$missing_count <- rowSums(is.na(temp))
  
  #remove rows with mostly missing data
  temp <- subset(temp, missing_count<200)
  
  #in case there is a repeated row:
  temp <- temp[-which(duplicated(temp$Indicator.Code)), ]
  
  #if country has all 16 variables
  
  if (nrow(temp)==16){
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
                         "cb_foreign_debt")
    
    tempt <- tempt[-c(1:6,nrow(tempt)),]
    
    
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

cb2$financial_claims <- cb2$asset2.claims_on_otherdepos-cb2$g.shares-cb2$h.other_items
cb2$domestic_credit <- cb2$asset3.netclaims_on_centgovt

#convertion ER
cb2$cer_base <- cb2$b.monetary_base/cb2$reserves

cb2$cer_full <- (cb2$b.monetary_base+cb2$remunerated_liabilities)/cb2$reserves

cb2$cer_complete <- (cb2$b.monetary_base+cb2$remunerated_liabilities-
                       cb2$financial_claims-cb2$domestic_credit)/(cb2$reserves-cb2$cb_foreign_debt)

cb2$cer_base_wfd <- cb2$b.monetary_base/(cb2$reserves-cb2$cb_foreign_debt)

cb2$cer_full_wfd <- (cb2$b.monetary_base+cb2$remunerated_liabilities)/(cb2$reserves-cb2$cb_foreign_debt)

cb2 <- cb2[-1,]

#SUBSET YEARS
cb2 <- subset(cb2, period>"2003-12-01")

write.csv(cb2, file = "~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output/cer_complete.csv")

####
#EXCTRACT GDP ANNUAL FROM QUARTERLY DATA (SOME COUNTRIES LACK ANNUAL GDP DATA, AND MONTHLY IS NA)

cb <- read.csv("ifs_bc_quarterly.csv", stringsAsFactors=FALSE)

cb2 <- cb[grep("Gross Domestic Product, Nominal, Unadjusted, Domestic Currency", cb$Indicator.Name),]

cb2 <- cb2[,c(2,7:ncol(cb2))]

#WIDE TO LONG
library(tidyr)

# Assuming cb2 is your original dataframe
cb_long <- cb2 %>%
  gather(key = "period", value = "gdp", -Country.Name) %>%
  rename(country = Country.Name) %>%
  mutate(period = gsub("X", "", period)) %>%
  separate(period, into = c("year", "quarter"), sep = "Q") %>%
  mutate(period = paste(year, quarter, sep = "-"),
         period = as.Date(paste0(period, "-01"), format = "%Y-%m-%d")) %>%
  select(country, period, gdp)

# Assuming "cb_long" is already in the proper long format with "period" as character column
cb_long$year <- as.numeric(substr(cb_long$period, 1, 4))

cb_long$gdp <- as.numeric(cb_long$gdp)

#AGGREGATE AT ANNUAL FREQUENCY
# Assuming cb_long is your data frame from the previous step
annual_gdp <- cb_long %>%
  group_by(country, year) %>%
  summarize(mean_gdp = sum(gdp, na.rm = TRUE))

write.csv(annual_gdp, file = "~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output/gdp_annual.csv")

