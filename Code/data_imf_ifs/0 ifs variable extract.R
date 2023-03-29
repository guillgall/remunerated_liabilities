#set directory
setwd("~/Desktop")

ifs <- read.csv("IFS_10-23-2020 15-52-28-92_timeSeries.csv", stringsAsFactors=FALSE)

#see what are relevant variables
var_names <- as.data.frame(unique(ifs$Indicator.Name))
var_names <- as.data.frame(var_names[grep("Monetary", var_names[,1]),])
var_names <- as.data.frame(var_names[-grep("Euros", var_names[,1]),])
var_names <- as.data.frame(var_names[-grep("US Dollars", var_names[,1]),])
var_names <- as.data.frame(var_names[grep("Central Bank Survey", var_names[,1]),])
write.csv(var_names, file = "indicator_name.csv")

##VARIABLES IN FS 2020 paper
#1: Monetary, Central Bank Survey, Net Foreign Assets, Claims on Non-residents, Domestic Currency
#2: Monetary, Central Bank Survey, Claims on Other Depository Corporations, Domestic Currency
#3: Monetary, Central Bank Survey, Net Claims on Central Government, Claims on Central Government, Domestic Currency
#a: Monetary, Central Bank Survey, Net Foreign Assets, Liabilities To Non-residents, Domestic Currency
#b: Monetary, Central Bank Survey, Monetary Base, Domestic Currency
#c: Monetary, Central Bank Survey, Other Liabilities To Other Depository Corporations, Domestic Currency
#d: Monetary, Central Bank Survey, Deposits and Securities other than Shares Excluded from Monetary Base, Domestic Currency
#e: Monetary, Central Bank Survey, Loans, Domestic Currency
#f: Monetary, Central Bank Survey, Financial Derivatives, Domestic Currency
#g: Monetary, Central Bank Survey, Shares and other Equity, Domestic Currency
#h: Monetary, Central Bank Survey, Other Items (Net), Domestic Currency
##

##
#extract relevant variables: balance sheet 
bc <- ifs[c(grep("Monetary, Central Bank Survey, Net Foreign Assets, Claims on Non-residents, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Claims on Other Depository Corporations, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Net Claims on Central Government, Claims on Central Government, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Net Foreign Assets, Liabilities To Non-residents, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Monetary Base, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Other Liabilities To Other Depository Corporations, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Deposits and Securities other than Shares Excluded from Monetary Base, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Loans, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Financial Derivatives, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Shares and other Equity, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Other Items (Net), Domestic Currency", ifs$Indicator.Name, fixed = TRUE),
           #see https://stackoverflow.com/questions/7992436/r-grep-pattern-regex-with-brackets
           grep("Monetary, Central Bank Survey, Financial Derivatives, Domestic Currency", ifs$Indicator.Name),
           grep("International Reserves, Official Reserve Assets, US Dollars", ifs$Indicator.Name),
           grep("Exchange Rates, Domestic Currency per U.S. Dollar, Period Average, Rate", ifs$Indicator.Name),
           grep("Monetary, Broad Money, Domestic Currency", ifs$Indicator.Name),
           grep("Financial, Interest Rates, Deposit, Percent per annum", ifs$Indicator.Name),
           grep("Gross Domestic Product, Expenditure Approach, Nominal, Domestic Currency", ifs$Indicator.Name),
           grep("Gross Domestic Product, Expenditure Approach, Deflator, Index", ifs$Indicator.Name))
         ,]

variables <- unique(bc$Indicator.Name)

variables

#MONTHLY DATA
ifsl_mon <- bc[,c(1:5, grep("M", colnames(bc)))]

unique(ifsl_mon$Indicator.Name)

#keep only data > 2002

ifsl_mon <- ifsl_mon[,-c(6:grep("^X2001M12$", colnames(ifsl_mon)))]                 

#delete rows with NA  
ifsl_monb <- ifsl_mon[complete.cases(ifsl_mon[ ,14]),]

write.csv(ifsl_monb, file = "ifs_bc_monthly.csv")

#QUARTERLY DATA (INCLUDES GDP)

ifsl_qua <- bc[,c(1:5, grep("Q", colnames(bc)))]


#keep only data > 2002

ifsl_qua <- ifsl_qua[,-c(6:grep("^X2001Q4$", colnames(ifsl_qua)))]                 

#delete rows with NA  
ifsl_qua[ifsl_qua==""]<-NA

ifsl_quab <- ifsl_qua[complete.cases(ifsl_qua[ ,6]),]

write.csv(ifsl_quab, file = "ifs_bc_quarterly.csv")

#ANNUAL DATA 

toremove1 <-grep("M", colnames(bc))
toremove2 <-grep("Q", colnames(bc))

ifsl_an <- bc[,-c(toremove1, toremove2)]

#keep only data > 2002

ifsl_an <- ifsl_an[,-c(6:grep("^X2001$", colnames(ifsl_an)))]                 

#delete rows with NA  
ifsl_an[ifsl_an==""] <-NA
ifsl_anb <- ifsl_an[complete.cases(ifsl_an[ ,6]),]

write.csv(ifsl_anb, file = "ifs_bc_annual.csv")


#
countries <- unique(ifsl_monb$Country.Name)

countries




write.csv(bc, file = "ifs_bc.csv")