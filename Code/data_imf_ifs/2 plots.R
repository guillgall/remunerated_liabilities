library(ggplot2)
library(ggrepel)
library(xtable)
library(tidyverse)

#set directory
setwd("~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output")

cb <- read.csv("cer.csv", stringsAsFactors=FALSE, check.names = FALSE)
cb_q <- read.csv("cer_quarterly.csv", stringsAsFactors=FALSE, check.names = FALSE)
cb_a <- read.csv("cer_annual.csv", stringsAsFactors=FALSE, check.names = FALSE)

cb$period <- as.Date(cb$period)
cb_q$period <- as.Date(cb_q$period)
cb_a$period <- as.Date(cb_a$period)

#cross section

cb_a$reserves_gdp <- cb_a$asset1.claims_on_nonresid/cb_a$gdp
cb_a$other_claims_depo_gdp <- cb_a$asset2.claims_on_otherdepos/cb_a$gdp
cb_a$domestic_credit_gdp <- cb_a$asset3.netclaims_on_centgovt/cb_a$gdp
cb_a$monetary_base_gdp <- cb_a$b.monetary_base/cb_a$gdp
cb_a$remunerated_liabilities_gdp <- cb_a$remunerated_liabilities/cb_a$gdp

colnames(cb_a)[c(24:28)] <- c("Reserves", "Financial Claims", "Domestic Credit", 
                                      "Monetary Base", "Remunerated Liabilities")


########
###TABLE 1
cb_2018 <- subset(cb_a, period=="2018-01-01")

colnames(cb_2018)[20] <- "Country"

lat_am_18 <- subset(cb_2018, Country=="Chile" | Country=="Uruguay" |
                        Country=="Brazil" | Country=="Paraguay" | 
                        Country=="Mexico" | Country=="Colombia")


cb_2018 <- cb_2018[order(-cb_2018[,28]),]
lat_am_18 <- lat_am_18[order(-lat_am_18[,28]),]



print(xtable(lat_am_18[,c(20,24:28)],
             caption = "Balance Sheet Components as a fraction of GDP, 2018",
             label = "tab:table_balance_sheet_latin_america_2018",
             align = "clccccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size = "small",
      caption.placement = "top",
      file="table_balance_sheet_latin_america_2018.tex")

###FIGURE 2

g <- subset(cb[,-1], country=="Chile" | country=="Uruguay" |
              country=="Brazil" | country=="Paraguay" | 
              country=="Mexico" | country=="Colombia") %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + ylab("local currency per US dollar") + xlab("year") +
  geom_line(aes(x=period, y=cer_base, colour="Conversion Base"), size = 0.375) +
  geom_line(aes(x=period, y=cer_full, colour="Conversion Full"), size = 0.375) +
  geom_line(aes(x=period, y=exchange, colour="Exchange Rate"), size = 0.5) +
  scale_colour_manual(name="", values=c("Conversion Base"="red2", 
                                        "Conversion Full"="royalblue",
                                        "Exchange Rate"="black")) +
  facet_wrap(vars(country), scales = "free_y") + theme_minimal(11) + theme(legend.position = "bottom")

g


#save plot
ggsave("./conversion_latin_america.pdf",
       width = 8, height = 5)



###FIGURE A3
lat_am_04_19 <- cb_a

colnames(lat_am_04_19)[20] <- "Country"

lat_am_04_19 <- subset(lat_am_04_19, Country=="Chile" | Country=="Uruguay" |
                      Country=="Brazil" | Country=="Paraguay" | 
                      Country=="Mexico" | Country=="Colombia")


lat_am_04_19 <- lat_am_04_19[,c(19,20,28)]

sapply(lat_am_04_19, class)

lat_am_04_19 <- subset(lat_am_04_19, period<"2020-01-01")



g <- lat_am_04_19 %>% 
      mutate(
        # change ordering of data by higher rl/gdp (change only affects plot g)
        Country = fct_relevel(Country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")
      ) %>%
      ggplot(aes(x=period, y=`Remunerated Liabilities`, color=Country)) +
      geom_line(size = 0.8) + 
      xlab("year") + 
      ylab("Remunerated liablities to GDP") + 
      theme_bw(14)
g
#save plot
ggsave("./remunerated_liabilities_fraction_gdp_evolution.pdf",
       width = 8, height = 5)

###TABLE A6
#aggregate 2010-2019

data_10_19 <- aggregate(cb_a, list(cb_a$country), na.rm = TRUE, mean)

data_10_19$country <- data_10_19[,1]

g <- ggplot(data=data_10_19, aes(y=reserves_gdp, x=remunerated_liabilities_gdp)) 
g <- g +  geom_point() 
g <- g + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
g <- g + labs(y="Reserves", x = "Remunerated Liabilities")
g <- g + geom_text_repel(data=subset(data_10_19, reserves_gdp > .4 | remunerated_liabilities_gdp> .05), 
                   aes(remunerated_liabilities_gdp, reserves_gdp, label=country))
#g <- g + theme(legend.position="none")
g


data_10_19 <- data_10_19[order(-data_10_19[,29]),]

colnames(data_10_19)[1] <- "Country"
  

print(xtable(data_10_19[,c(1,25:29)],
             caption = "Balance Sheet Components as a fraction of GDP",
             label = "tab:table_balance_sheet",
             align = "clccccc"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      type="latex", 
      size = "scriptsize",
      caption.placement = "top",
      file="table_balance_sheet.tex")

#####

