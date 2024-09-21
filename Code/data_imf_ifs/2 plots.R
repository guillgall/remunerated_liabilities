library(ggplot2)
library(ggrepel)
library(xtable)
library(tidyverse)
library(openxlsx)
library(zoo)

#set directory
setwd("~/Documents/GitHub/exchange_rates_balance_sheet/Data")

cb <- read.csv("./Output/cer_complete.csv", stringsAsFactors=FALSE, check.names = FALSE)

cb <- cb[,-1]

cb$period <- as.Date(cb$period)

colnames(cb)

check <- unique(cb$country)

cb <- subset(cb, period>"2005-12-01" & period<"2020-01-01")

###FIGURE 1
g <- subset(cb, country=="Chile" | country=="Uruguay" |
              country=="Brazil" | country=="Paraguay" | 
              country=="Mexico" | country=="Colombia" | 
              country=="Peru" | country=="Argentina") %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Peru", "Argentina")) %>%
  ggplot() + ylab("Local currency per US dollar") + xlab("Year") +
  geom_line(aes(x=period, y=cer_base, colour="CER Base"), size = 0.375) +
  geom_line(aes(x=period, y=cer_full, colour="CER Full"), size = 0.375) +
  geom_line(aes(x=period, y=exchange, colour="Exchange Rate"), size = 0.5) +
  scale_colour_manual(name="", values=c("CER Base"="red2", 
                                        "CER Full"="blue", 
                                        "Exchange Rate"="black")) +
  facet_wrap(vars(country), scales = "free_y") + theme_minimal(11) + theme(legend.position = "bottom")

g

#save plot
ggsave("./Output/conversion_latin_america.pdf",
       width = 8, height = 5)

#FOR APPENDIX: COMPELTE CER

g <- subset(cb, country=="Chile" | country=="Uruguay" |
              country=="Brazil" | country=="Paraguay" | 
              country=="Mexico" | country=="Colombia" | 
              country=="Peru" | country=="Argentina") %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Peru", "Argentina")) %>%
  ggplot() + ylab("Local currency per US dollar") + xlab("Year") +
  geom_line(aes(x=period, y=cer_base, colour="CER Base"), size = 0.375) +
  geom_line(aes(x=period, y=cer_full, colour="CER Full"), size = 0.375) +
  geom_line(aes(x=period, y=cer_base_wfd, colour="CER Base With Foreign Debt"), size = 0.375) +
  geom_line(aes(x=period, y=cer_full_wfd, colour="CER Full With Foreign Debt"), size = 0.375) +
  geom_line(aes(x=period, y=cer_complete, colour="Conversion Complete"), size = 0.375) +
  geom_line(aes(x=period, y=exchange, colour="Exchange Rate"), size = 0.5) +
  scale_colour_manual(name="", values=c("CER Base"="red2", 
                                        "CER Full"="blue", 
                                        "CER Base With Foreign Debt"="grey",
                                        "CER Full With Foreign Debt"="purple",  
                                        "Conversion Complete"="green",
                                        "Exchange Rate"="black")) +
  facet_wrap(vars(country), scales = "free_y") + theme_minimal(11) + theme(legend.position = "bottom")

g

#save plot
ggsave("./Output/conversion_latin_america_all_metrics.pdf",
       width = 8, height = 5)

##Argentina
#ADD BLACK MARKET ER
monetary_data <- read.csv("./Output/monetary_data.csv", stringsAsFactors=FALSE)

#date var
monetary_data$date <- paste(monetary_data$year,
                            monetary_data$month,
                            sep="-")

monetary_data$period <- as.Date(as.yearmon(paste(monetary_data$date)))

monetary_data <- monetary_data[,c("period", "informal_er")]

arg <- subset(cb, country=="Argentina")
arg <- merge(arg, monetary_data, by="period")

g <- subset(arg, period>"2016-12-01" & period<"2020-01-01") %>% 
  #mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Peru", "Bolivia", "Argentina", "Venezuela, Rep. Bolivariana de", "Costa Rica", "Guatemala")) %>%
  ggplot() + ylab("Local currency per US dollar") + xlab("Year") +
  geom_line(aes(x=period, y=cer_base, colour="CER Base"), size = 0.375) +
  geom_line(aes(x=period, y=cer_full, colour="CER Full"), size = 0.375) +
  geom_line(aes(x=period, y=exchange, colour="Formal ER"), size = 0.375) +
  geom_line(aes(x=period, y=informal_er, colour="Informal ER"), size = 0.5) +
  scale_colour_manual(name="", values=c("CER Base"="red2", 
                                        "CER Full"="blue", 
                                        "Formal ER"="black",
                                        "Informal ER"="green")) +
  theme_minimal(14) + theme(legend.position = "bottom")

g

#save plot
ggsave("./Output/argentina_conversion_exchange_rate_complete_1701_1912.pdf",
       width = 8, height = 5)


####
#FOR TABLES
#ADD GDP DATA

gdp <- read.csv("./Output/gdp_annual.csv")

gdp <- gdp[,-c(1)]


#aggregate annual 2010-2019

#cb2 <- subset(cb, period>"2010-01-01" & period<"2020-01-01")

cb$year <- as.numeric(substr(cb$period, 1, 4))

cb_annual <- aggregate(cb, list(cb$country, cb$year), na.rm = TRUE, mean)

colnames(cb_annual)[1] <- "country"

cb_annual <- cb_annual[,-c(2)]

cb_annual<- merge(cb_annual, gdp, by=c("country", "year"))

cb_annual$reserves_gdp <- cb_annual$asset1.claims_on_nonresid/cb_annual$mean_gdp
cb_annual$monetary_base_gdp <- cb_annual$b.monetary_base/cb_annual$mean_gdp
cb_annual$remunerated_liabilities_gdp <- cb_annual$remunerated_liabilities/cb_annual$mean_gdp
cb_annual$non_resid_liab_gdp <- cb_annual$a.liabilities_to_nonresid/cb_annual$mean_gdp
cb_annual$financial_claims_gdp <- cb_annual$financial_claims/cb_annual$mean_gdp
cb_annual$domestic_credit_gdp <- cb_annual$domestic_credit/cb_annual$mean_gdp

cb_annual <- cb_annual[order(-cb_annual[,"remunerated_liabilities_gdp"]),]

# Filter out rows with Inf values in the Reserves column
cb_annual <- cb_annual[!is.infinite(cb_annual$remunerated_liabilities_gdp), ]

colnames(cb_annual)[1] <- "Country"

colnames(cb_annual)[30:35] <- c("Reserves", "Mon. Base", "RDL", "Ext. Liab.", "Net Fin. Claims", "Net Dom. Credit")

data_sub <- subset(cb_annual, year==2017)

###TABLE 1

lat_am_sub <- subset(data_sub, Country=="Chile" | Country=="Uruguay" |
                       Country=="Brazil" | Country=="Paraguay" | 
                       Country=="Mexico" | Country=="Colombia" | 
                       Country=="Peru" | Country=="Argentina")


#lat_am_sub <- lat_am_sub[order(-lat_am_sub[,29]),]


print(xtable(lat_am_sub[,c(1,30:32)],
             caption = "Balance Sheet Components as a fraction of GDP, 2017 average",
             label = "tab:table_balance_sheet_latin_america_sub",
             align = "clccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size = "small",
      caption.placement = "top",
      file="./Output/table_balance_sheet_latin_america_sub.tex")

mean(lat_am_sub$RDL)

#TABLE APPENDIX: LATAM ALL COMPONENTS

print(xtable(lat_am_sub[,c(1,30:35)],
             caption = "Balance Sheet Components as a fraction of GDP, 2017 average",
             label = "tab:table_balance_sheet_latin_america_sub_all_components",
             align = "clcccccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size = "small",
      caption.placement = "top",
      file="./Output/table_balance_sheet_latin_america_sub_all_components.tex")

mean(lat_am_sub$`Ext. Liab.`)*100


#TABLE APPENDIX: ALL COUNTRIES

data_sub2 <- subset(data_sub, data_sub$RDL>.01)

print(xtable(data_sub2[,c(1,30:35)],
             caption = "Balance Sheet Components as a fraction of GDP, 2017",
             label = "tab:table_balance_sheet",
             align = "clcccccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size = "scriptsize",
      caption.placement = "top",
      file="./Output/table_balance_sheet.tex")


########


###FIGURE A3
lat_am_04_19 <- cb_annual

colnames(lat_am_04_19)[20] <- "Country"

lat_am_04_19 <- subset(lat_am_04_19, Country=="Chile" | Country=="Uruguay" |
                      Country=="Brazil" | Country=="Paraguay" | 
                      Country=="Mexico" | Country=="Colombia" | 
                      Country=="Peru" | Country=="Argentina")


#lat_am_04_19 <- lat_am_04_19[,c(1,2,31)]

sapply(lat_am_04_19, class)

lat_am_04_19 <- subset(lat_am_04_19, period<"2020-01-01")



g <- lat_am_04_19 %>% 
      mutate(
        # change ordering of data by higher rl/gdp (change only affects plot g)
        Country = fct_relevel(Country, "Uruguay", "Brazil", "Chile", "Paraguay", 
                              "Mexico", "Colombia", "Peru", "Argentina")
      ) %>%
      ggplot(aes(x=year, y=`RDL`, color=Country)) +
      geom_line(size = 0.8) + 
      xlab("year") + 
      ylab("RDL to GDP") + 
      theme_bw(14)
g

#save plot
ggsave("./Output/remunerated_domestic_liabilities_over_gdp_evolution.pdf",
       width = 8, height = 5)

g <- lat_am_04_19 %>% 
  mutate(
    # change ordering of data by higher rl/gdp (change only affects plot g)
    Country = fct_relevel(Country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")
  ) %>%
  ggplot(aes(x=year, y=`External Liabilities`, color=Country)) +
  geom_line(size = 0.8) + 
  xlab("year") + 
  ylab("External Liabilities to GDP") + 
  theme_bw(14)
g

#save plot
ggsave("./Output/external_liabilities_over_gdp_evolution.pdf",
       width = 8, height = 5)

g <- lat_am_04_19 %>% 
  mutate(
    # change ordering of data by higher rl/gdp (change only affects plot g)
    Country = fct_relevel(Country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")
  ) %>%
  ggplot(aes(x=year, y=`Reserves`, color=Country)) +
  geom_line(size = 0.8) + 
  xlab("year") + 
  ylab("Reserves to GDP") + 
  theme_bw(14)
g

#save plot
ggsave("./Output/reserves_over_gdp_evolution.pdf",
       width = 8, height = 5)


#####
#SCATTER PLOT
# Create the scatter plot
ggplot(subset(cb, period=="2018-01-01"), aes(x = exchange/cer_base_complete, y = exchange/cer_full_complete)) +
  geom_point() +
  labs(x = "cb$exchange", y = "cb$cer_full_complete") +
  ggtitle("Scatter Plot: cb$exchange vs. cb$cer_full_exchange")


#########OTHER PLOTS
##ADD OTHER COUNTRIES
g <- subset(cb, period<"2020-01-01" & 
              (country=="Chile" | country=="Uruguay" |
              country=="Brazil" | country=="Paraguay" | 
              country=="Mexico" | country=="Colombia" | 
              country=="Bolivia" | country=="Peru" | 
              country=="Canada" | country=="Malaysia" |
              country=="Korea, Rep. of" | country=="Jamaica")) %>% 
  #mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Peru", "Bolivia", "Argentina", "Venezuela, Rep. Bolivariana de", "Costa Rica", "Guatemala")) %>%
  ggplot() + ylab("local currency per US dollar") + xlab("year") +
  geom_line(aes(x=period, y=cer_base, colour="Conversion Base"), size = 0.375) +
  geom_line(aes(x=period, y=cer_domestic, colour="Conversion Domestic"), size = 0.375) +
  geom_line(aes(x=period, y=cer_external, colour="Conversion External"), size = 0.375) +
  geom_line(aes(x=period, y=cer_nonother, colour="Conversion Non Other"), size = 0.375) +
  geom_line(aes(x=period, y=cer_complete, colour="Conversion Complete"), size = 0.375) +
  geom_line(aes(x=period, y=exchange, colour="Exchange Rate"), size = 0.5) +
  scale_colour_manual(name="", values=c("Conversion Base"="red2", 
                                        "Conversion Domestic"="purple", 
                                        "Conversion External"="green", 
                                        "Conversion Non Other"="orange",
                                        "Conversion Complete"="royalblue",
                                        "Exchange Rate"="black")) +
  facet_wrap(vars(country), scales = "free_y") + theme_minimal(11) + theme(legend.position = "bottom")

g



