library(ggplot2)
library(zoo)
library(xtable)

#set directory
setwd("~/Documents/GitHub/exchange_rates_balance_sheet/Data")

monetary_data <- read.csv("monetary_level.csv", stringsAsFactors=FALSE)

monetary_data$date2 <- as.Date(as.yearmon(paste(monetary_data$date2)))

#Conversion Exchange Rate
require('patchwork')

###FIGURE 2

g1 <- ggplot(data=subset(monetary_data, date2>"2016-01-01" & date2<"2019-07-01")) + 
        ylab(NULL) + xlab('year') +
        geom_line(aes(x=date2, y=convertion1,  colour="CER Base"), size = 0.375) + 
        geom_line(aes(x=date2, y=convertion2,  colour="CER Full"), size = 0.375) + 
        geom_line(aes(x=date2, y=exchange,  colour="Formal ER"), size = 0.5) + 
        geom_line(aes(x=date2, y=informal_er,  colour="Informal ER"), size = 0.375) + 
        scale_colour_manual(name="", values=c("CER Base"="red2", 
                                               "CER Full"="royalblue",
                                               "Formal ER"="black",
                                               "Informal ER"="green")) +
        theme_minimal(13) + theme(legend.position="none")
g1

ggsave("./Output/argentina_conversion_exchange_rate.pdf", width = 8, height=5)

g0 <- ggplot(data=subset(monetary_data, date2>="2004-01-01" & date2<"2019-12-01")) + 
  ylab("Pesos per dollar") + xlab('year') +
  geom_line(aes(x=date2, y=convertion1,  colour="CER Base"), size = 0.375) + 
  geom_line(aes(x=date2, y=convertion2,  colour="CER Full"), size = 0.375) + 
  geom_line(aes(x=date2, y=exchange,  colour="Formal ER"), size = 0.5) + 
  geom_line(aes(x=date2, y=informal_er,  colour="Informal ER"), size = 0.375) + 
  scale_colour_manual(name="", values=c("CER Base"="red2", 
                                        "CER Full"="royalblue",
                                        "Formal ER"="black",
                                        "Informal ER"="green")) +
  theme_minimal(13) + theme(legend.position="none")

g0

(g0 | g1) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave("./Output/argentina_conversion_exchange_rate_fullsample.pdf", width = 8, height=5)


