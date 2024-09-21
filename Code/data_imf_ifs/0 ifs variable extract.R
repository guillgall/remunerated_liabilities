#set directory
setwd("~/Desktop/Data/IMF IFS")

ifs <- read.csv("IFS_06-10-2024 16-18-13-41_timeSeries.csv", stringsAsFactors=FALSE)

#see what are relevant variables
var_names <- as.data.frame(unique(ifs$Indicator.Name))
var_names <- as.data.frame(var_names[grep("Monetary", var_names[,1]),])
var_names <- as.data.frame(var_names[-grep("Euros", var_names[,1]),])
var_names <- as.data.frame(var_names[-grep("US Dollars", var_names[,1]),])
var_names <- as.data.frame(var_names[grep("Central Bank Survey", var_names[,1]),])
write.csv(var_names, file = "indicator_name.csv")

#see what are relevant variables denominated in dollars
var_names <- as.data.frame(unique(ifs$Indicator.Name))
var_names <- as.data.frame(var_names[grep("Monetary", var_names[,1]),])
var_names <- as.data.frame(var_names[grep("US Dollars", var_names[,1]),])
var_names <- as.data.frame(var_names[grep("Central Bank Survey", var_names[,1]),])
write.csv(var_names, file = "indicator_name_dollars.csv")

#see names other variables
var_names <- as.data.frame(unique(ifs$Indicator.Name))
var_names <- as.data.frame(var_names[grep("Gross Domestic", var_names[,1]),])

#check
uru <- subset(ifs, Country.Name=="Uruguay")
uru <- uru[grep("Gross Domestic", uru$Indicator.Name),]


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
           grep("Monetary, Central Bank Survey, Net Claims on Central Government, Domestic Currency", ifs$Indicator.Name),
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
           grep("Gross Domestic Product, Nominal, Unadjusted, Domestic Currency", ifs$Indicator.Name),
           grep("Monetary, Central Bank Survey, Net Foreign Assets, Liabilities To Non-residents, Other Foreign Liabilities, International Liquidity, Domestic Currency", ifs$Indicator.Name),
          grep("Monetary, Central Bank Survey, Net Foreign Assets, Liabilities To Non-residents, Other Foreign Liabilities, International Liquidity, US Dollars", ifs$Indicator.Name))
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

ifsl_q <- bc[,c(1:5,grep("Q", colnames(bc)))]

#keep only data > 2002

ifsl_q <- ifsl_q[,-c(6:grep("^X2001Q4$", colnames(ifsl_q)))]                 

#delete rows with NA  
ifsl_q[ifsl_q==""] <-NA
ifsl_qb <- ifsl_q[complete.cases(ifsl_q[ ,80]),]

write.csv(ifsl_qb, file = "ifs_bc_quarterly.csv")


#check
uru <- subset(ifsl_qb, Country.Name=="Uruguay")
uru <- uru[grep("Gross Domestic", uru$Indicator.Name),]

#
countries <- unique(ifsl_monb$Country.Name)

countries

#write.csv(bc, file = "ifs_bc.csv")


