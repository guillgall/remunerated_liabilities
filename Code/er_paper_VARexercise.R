# Paper on Exchange Rate Dynamics and the Central Banks' Balance Sheet
# Replication code for VAR exercise based on eq (14) variables.
# Date (1st version): Aug 2021
# camilo.granados@utdallas.edu, guillermogallacher@gmail.com, janelle.mann@umanitoba.ca


## Iterations/updates:

# 1. Estimation of VAR by country (Chile, Brazil, Paraguay, Uruguay, Colombia) and Panel VAR.
# Causality tests, FEVD
# 2. Estimation with shorter sample (2004-2019) (Sep 2021)
#    Inclusion of Mexico
#    Inclusion of Argentina 
# 3. Inclusion of Argentina - Informal ER (Feb 2023)

# Contents:
# Unit root tests
# Variables transformation
# VAR Estimation
# IRF and Variance Decomposition (FEDV)

rm(list=ls())

# Packages
library(vars)
library(tseries)
library(tidyverse)
library(stargazer)
library(dplyr) 
library(countrycode)
library(patchwork)


# Reading data
#path <- "/Users/jocagraca-mbp15/Dropbox/Papers/ExchangeRateDynamics"  ; setwd(path) 
#path <- "/Users/camilogranados/Dropbox/Papers/ExchangeRateDynamics"  ; setwd(path) 

path <- "~/Documents/GitHub/exchange_rates_balance_sheet/R Code/VARs"; setwd(path)
cer_dat <- read.csv("cer.csv", header = T) 

# Exercise for Chile, Uruguay, Paraguay, Brazil, Argentina, Colombia
ctrylist <- c("Chile","Brazil","Paraguay","Uruguay","Colombia","Mexico","Argentina","Argentina_informal")#  #Argentina ERs not in sample --  done with another data file

cer_dat_latam <- cer_dat %>% filter(country %in% ctrylist) %>%
  dplyr::select(period,country,exchange,asset1.claims_on_nonresid,b.monetary_base,c.other_liabilities,d.deposits,e.loans,f.derivatives,cer_base,cer_full) %>%
  mutate(H = asset1.claims_on_nonresid) %>%
  mutate(RL = c.other_liabilities + d.deposits + e.loans + f.derivatives) %>%
  mutate(MB = b.monetary_base) %>%
  mutate(year = substr(as.character(period), start=1, stop=4)) %>%
  mutate(month = substr(as.character(period), start=6, stop=7)) %>%
  mutate_at(c("year","month"),as.numeric) 

# Remove incomplete data (end of sample)
cer_dat_latam <- cer_dat_latam %>% filter(period != "2020-09-01" & period != "2020-08-01")

# Iter 2: shorter sample 2004-2019
shorter_sample = 1
  if(shorter_sample == 1){
    cer_dat_latam <- cer_dat_latam %>% filter(year >= 2004 & year <= 2019)
  }
  
start_year <- min(unique(cer_dat_latam$year))

VARLevels <- c("Chile","Uruguay","Colombia")# True: levels, False: VAR in diffs.
#VARDiffs <- c("Chile"     "Brazil"    "Paraguay"  "Uruguay"   "Colombia"  "Mexico"    "Argentina")

##--Main Loop
#-Selection of data by country
#-Estimation of VAR by country

for(i in 1:length(ctrylist)){
  ctry <- ctrylist[i] 
  
  if(!(ctry %in% c("Argentina","Argentina_informal"))){
    cer_ctry <- cer_dat_latam %>% filter(country == ctry) %>% dplyr::select(exchange,H,MB,RL) %>% 
      ts(start=c(start_year,1),freq=12)
  } else if(ctry == "Argentina" | ctry == "Argentina_informal") {

    ## -- Argentina not in original database. Added with external data (on 9/25/22) --
    arg_dat <- read.csv("arg_cer.csv", header = T) 
    # variables mapping from database: reserves: H, notas.bcra: RL, m0: MB, exchange
    arg_dat <- arg_dat %>% dplyr::select(year,month,exchange,reserves,m0,notas.bcra) %>%
      rename(H = reserves, RL = notas.bcra, MB = m0) %>% 
      mutate_at(c("year","month", "MB"),as.numeric) 
    
    if(shorter_sample == 1){
      arg_dat <- arg_dat  %>% filter(year >= start_year & year <= 2019)
    }
    
    # arg ER data is missing for 2009.1-.8 then will replace the entire series from FRED
    library(quantmod)
    getSymbols('ARGCCUSMA02STM',src='FRED')
    er_arg <- ARGCCUSMA02STM
    last_year_fred <- index(er_arg) %>% last %>% substr(start=1,stop=4) %>% as.numeric
    dates <- as.data.frame(expand.grid(1960:last_year_fred,1:12)) %>% arrange(Var1,Var2) %>% rename(c("year" = "Var1", "quarter" = "Var2"))
    dates <- dates[1:length(er_arg),]
    er_arg <- cbind(dates,er_arg) %>% filter(year >= start_year & year <= 2019)
    arg_dat <- arg_dat %>% mutate(exchange = er_arg$ARGCCUSMA02STM)
    
    # To compare old and new ER replace last line with the next one and uncomment plot
    #arg_dat <- arg_dat %>% mutate(exchange2 = er_arg$ARGCCUSMA02STM)
    # arg_dat %>% ggplot() + 
    #   geom_line(aes(x = 1:192, y = exchange2, color = "new from FRED")) + geom_line(aes(x = 1:192, y = exchange)) + 
    #   theme_minimal(13) +  ggtitle("Exchange Rate in ARG") + theme(plot.title = element_text(hjust = 0.5)) + 
    #   ylab("") + xlab("")
     
     
    ## conversion to diff-logs and time series for VAR
    #start_year <- min(unique(arg_dat$year)) 
    
    #arg_dat_dlog <- arg_dat %>% mutate(H = log(H) - lag(log(H)), RL = log(RL) - lag(log(RL))) %>%
    #  mutate(MB = log(MB) - lag(log(MB)), exchange = log(exchange) - lag(log(exchange))) %>% 
    #  select(-c("year","month")) %>%
    #  ts(start = c(start_year,1),freq=12)
    
    #arg_dat_dlog <- arg_dat_dlog[-1,] %>% ts(start = c(start_year,2),freq=12) #remove first obs with NA due to diff-log
    cer_ctry <- arg_dat %>% dplyr::select(-c("year","month")) 
    
    if(ctry == "Argentina_informal"){
      argInf_dat <- read.csv('~/Documents/GitHub/exchange_rates_balance_sheet/Data/monetary_level.csv', header = T)
      argInf_dat <- argInf_dat %>% dplyr::select("year", "month", "informal_er") %>% filter(year >= start_year & year <= 2019)
      
    #replace ER in cer_arg with Informal ER (keep name as "exchange" for loop purposes)
    #arg_inf_er <-  ts(argInf_dat$informal_er,start=c(start_year,1),freq=12)  
    cer_ctry <- cer_ctry %>% mutate(exchange = argInf_dat$informal_er)
    }
    
    #transform in time series format
    cer_ctry <- cer_ctry %>% ts(start=c(start_year,1),freq=12)
    
    } 
   
 
  # Country prompt:
  cat("\n","\n",ctry,"\n","\n")
  
  # Unit root tests:
  variables <- c("exchange","H","MB","RL")
  cat('I(1) test:',"\n")
  for(j in variables)  cat(j,':',adf.test(cer_ctry[,j])$p.value, '\n')
  # all I(1). Series are transformed to log-diff
  cer_ctry_dlog <- diff(log(cer_ctry)) %>% ts(start=c(start_year,2),freq=12) 
  # Optimal lag length
  lag_ctry <- VARselect(cer_ctry_dlog) ; cat('\n')  ; print(lag_ctry$selection) #opt lag Ch: 1
  lag <- lag_ctry$selection["AIC(n)"]
  # Estimation
  var_ctry <- VAR(cer_ctry_dlog,p=lag,type="const")
  model <- "VAR in diff of logs"
  
  if(ctry %in% VARLevels){ #Overwrite VAR with model in levels for selected countries
    lag_ctry <- VARselect(log(cer_ctry)); lag <- lag_ctry$selection["AIC(n)"]
    var_ctry <- VAR(log(cer_ctry),p=lag,type="const")
    model <- "VAR in levels - logs"
  }
  
  summary(var_ctry)
  stargazer(var_ctry[["varresult"]],type='text')
  # takeaway: (Ch) for S all variables matter, including RL, while for others only AR terms matter. This is good, 
  # implies that eq. (11) makes sense and it's not a matter of all variables having cofounding determinants.
  
  # Testing models' stability:
  stable <- all(roots(var_ctry, modulus=T) <= 1) # good is all < 1
  
  # Granger Causality
  granger_er_ctry <- causality(var_ctry, cause="exchange", vcov. = vcovHC(var_ctry))
  granger_h_ctry <- causality(var_ctry, cause="H",         vcov. = vcovHC(var_ctry))
  granger_mb_ctry <- causality(var_ctry, cause="MB",       vcov. = vcovHC(var_ctry))
  granger_rl_ctry <- causality(var_ctry, cause="RL",       vcov. = vcovHC(var_ctry))
  # S does not granger cause the system. The rest of the system does granger-cause the other variables.
  
  # IRFs and FEVD
  #irf_ctry <- irf(var_ctry, n=24, cumulative=T, boot= F, run = 10000, ci= 0.95)
  #plot(irf_ctry)
  vd_ctry <- vars::fevd(var_ctry, n.ahead = 12)
  #plot(vd_ctry)
  
  # Diagnostic tests for VAR model 
  #Serial correlation
  arres_ctry <- serial.test(var_ctry, lags.pt =8, type = "PT.asymptotic") 
  arres_ctry$serial$p.value
  
  #Normality of residuals
  normal_ctry <- normality.test(var_ctry,multivariate.only=T)
  normal_ctry$jb.mul$JB$p.value
  #Structural breaks of residuals
  stab_var_ctry <- vars::stability(var_ctry, type = "OLS-CUSUM")
  #plot(stab_var_ctry)
  
  # -- Cointegration (multiv) tests
  lagLevel <- VARselect(log(cer_ctry)); lagLv <- lagLevel$selection["AIC(n)"]
  cotest1 <- ca.jo(log(cer_ctry), type='trace', ecdet = 'cons', K=lagLv) 
  cotest2 <- ca.jo(log(cer_ctry), type='eigen', ecdet = 'cons', K=lagLv) 
  summary(cotest1); summary(cotest2)
  
  cotest1tab <- cbind(cotest1@cval[,2], round(cotest1@teststat,2), cotest1@cval[,2] >= cotest1@teststat)
  colnames(cotest1tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test Results for',ctry, '\n', '\n') ; print(cotest1tab)
  
  # Saving results by country
  list_var <- list(var = var_ctry, 
                   lag = lag_ctry, 
                   stable = stable, 
                   granger_er = granger_er_ctry, 
                   granger_h = granger_h_ctry, 
                   granger_mb = granger_mb_ctry, 
                   granger_rl = granger_rl_ctry, 
                   vardecomp = vd_ctry, 
                   serial_test = arres_ctry$serial$p.value,
                   country = ctry,
                   model = model,
                   data_in_var = cer_ctry)
  
  if(ctry == "Argentina_informal") { ctryIso3 <- "ARGInf"
  } else {
    ctryIso3 <- countrycode(ctry,origin='country.name',destination='iso3c')
  }
  
  assign(paste("var",ctryIso3,sep=""),list_var)
}


## -- FEVD plots

# Format: x_y -> variance of x explained by y
# Creation of dataframe for FEVD plots
temp1 <- rbind(varURY$vardecomp$exchange*100, varBRA$vardecomp$exchange*100, varCHL$vardecomp$exchange*100, 
      varPRY$vardecomp$exchange*100, varMEX$vardecomp$exchange*100, varCOL$vardecomp$exchange*100, 
      varARG$vardecomp$exchange*100, varARGInf$vardecomp$exchange*100 )

temp2 <- t(cbind(t(varURY$vardecomp$H[,"H"]*100), t(varBRA$vardecomp$H[,"H"]*100), t(varCHL$vardecomp$H[,"H"]*100),
        t(varPRY$vardecomp$H[,"H"]*100), t(varMEX$vardecomp$H[,"H"]*100), t(varCOL$vardecomp$H[,"H"]*100),
        t(varARG$vardecomp$H[,"H"]*100),t(varARGInf$vardecomp$H[,"H"]*100)))

temp3 <- t(cbind(t(varURY$vardecomp$H[,"RL"]*100), t(varBRA$vardecomp$H[,"RL"]*100), t(varCHL$vardecomp$H[,"RL"]*100),
                 t(varPRY$vardecomp$H[,"RL"]*100), t(varMEX$vardecomp$H[,"RL"]*100), t(varCOL$vardecomp$H[,"RL"]*100),
                 t(varARG$vardecomp$H[,"RL"]*100), t(varARGInf$vardecomp$H[,"RL"]*100)))

temp4 <- t(cbind(t(varURY$vardecomp$MB[,"RL"]*100), t(varBRA$vardecomp$MB[,"RL"]*100), t(varCHL$vardecomp$MB[,"RL"]*100),
        t(varPRY$vardecomp$MB[,"RL"]*100), t(varMEX$vardecomp$MB[,"RL"]*100), t(varCOL$vardecomp$MB[,"RL"]*100),
        t(varARG$vardecomp$MB[,"RL"]*100), t(varARGInf$vardecomp$MB[,"RL"]*100)))

# add two new columns
temp5 <- t(cbind(t(varURY$vardecomp$RL[,"RL"]*100), t(varBRA$vardecomp$RL[,"RL"]*100), t(varCHL$vardecomp$RL[,"RL"]*100),
                 t(varPRY$vardecomp$RL[,"RL"]*100), t(varMEX$vardecomp$RL[,"RL"]*100), t(varCOL$vardecomp$RL[,"RL"]*100),
                 t(varARG$vardecomp$RL[,"RL"]*100), t(varARGInf$vardecomp$RL[,"RL"]*100)))

temp6 <- t(cbind(t(varURY$vardecomp$MB[,"MB"]*100), t(varBRA$vardecomp$MB[,"MB"]*100), t(varCHL$vardecomp$MB[,"MB"]*100),
                 t(varPRY$vardecomp$MB[,"MB"]*100), t(varMEX$vardecomp$MB[,"MB"]*100), t(varCOL$vardecomp$MB[,"MB"]*100),
                 t(varARG$vardecomp$MB[,"MB"]*100), t(varARGInf$vardecomp$MB[,"MB"]*100)))

temp <- cbind(temp1, temp2, temp3, temp4, temp5, temp6) %>% as.tibble()

colnames(temp) <- c("er_er","er_h","er_mb","er_rl","h_h","h_rl","mb_rl","rl_rl","mb_mb")
country_label <- rep(c("Uruguay","Brazil","Chile","Paraguay","Mexico","Colombia","Argentina","Argentina (informal)"), times = 1, each = 12)

data_fevd <- tibble(horizon = rep(1:12,8), country = country_label , temp)

# figure A1 in the draft (appendix)
g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina","Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = er_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RL",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%")
  ) + scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))
  
g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = h_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,5,10,15,20,25,30),
    labels = c("0%","5%","10%","15%","20%","25%","30%"),
    limits = c(0,30)
  ) + scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5))

g3 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = mb_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RL",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%"),
    limits = c(0,10)
  ) + scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Money Base") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"
    
library(patchwork)

layout <- 
  "AAABB#
   #CCC##"

g1 + g2  + g3 + plot_layout(design = layout) #& scale_color_manual(values=c("royalblue3","orangered3","purple4","lightseagreen","gold4","orange","gray35"))

ggsave("~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output/fevd_variables_by_rl.pdf", width = 8, height=5)


# Figure A2

g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = mb_mb, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by MB",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,100)
  ) + scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Monetary Base") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))

g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = rl_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RL",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,100)
  ) +
  scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Remunerated Liabilities") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))


(g1 | g2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output/fevd_mb_rl_by_themselves.pdf", width = 8, height=4)


# Figure 3

g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = er_er, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by ER",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(0,105)
  ) + scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))

g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia", "Argentina", "Argentina (informal)")) %>%
  ggplot() + aes(x = horizon, y = h_h, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by Reserves",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(0,105)
  ) +
  scale_x_continuous(
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))


(g1 | g2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("~/Documents/GitHub/exchange_rates_balance_sheet/Data/Output/fevd_er_h_by_themselves.pdf", width = 8, height=4)



# Individual comparable figures (generated with base R plotting packages)

plot(1:12,varCHL$vardecomp$exchange[,"RL"]*100,type='l',ylim=c(0,12),xlab="forecast horizon",ylab='% of variance explained by RL')
lines(1:12,varBRA$vardecomp$exchange[,"RL"]*100,col='red')
lines(1:12,varPRY$vardecomp$exchange[,"RL"]*100,col='green4')
lines(1:12,varURY$vardecomp$exchange[,"RL"]*100,col='blue')
lines(1:12,varCOL$vardecomp$exchange[,"RL"]*100,col='darkorange')
lines(1:12,varMEX$vardecomp$exchange[,"RL"]*100,col='brown4')
lines(1:12,varARG$vardecomp$exchange[,"RL"]*100,col='darkslategray3')
lines(1:12,varARGInf$vardecomp$exchange[,"RL"]*100,col='purple')
title("Forecast Error Variance Decomposition for the Exchange Rate")
legend("topright",c("CHL","BRA","PRY","URY","COL","MEX","ARG","ARGInf"),col=c("black","red","green4","blue","darkorange","brown4","darkslategray3","purple"), lty = 1,
       cex=.55,horiz=T)



plot(1:12,varCHL$vardecomp$H[,"RL"]*100,type='l',ylim=c(0,30),xlab="forecast horizon",ylab='% of variance explained by RL')
lines(1:12,varBRA$vardecomp$H[,"RL"]*100,col='red')
lines(1:12,varPRY$vardecomp$H[,"RL"]*100,col='green4')
lines(1:12,varURY$vardecomp$H[,"RL"]*100,col='blue')
lines(1:12,varCOL$vardecomp$H[,"RL"]*100,col='darkorange')
lines(1:12,varMEX$vardecomp$H[,"RL"]*100,col='brown4')
lines(1:12,varARG$vardecomp$H[,"RL"]*100,col='darkslategray3')
lines(1:12,varARGInf$vardecomp$H[,"RL"]*100,col='purple')
title("Forecast Error Variance Decomposition for the Reserves")
legend("topright",c("CHL","BRA","PRY","URY","COL","MEX","ARG","ARGInf"),col=c("black","red","green4","blue","darkorange","brown4","darkslategray3","purple"), lty = 1,
       cex=.55,horiz=T)

plot(1:12,varCHL$vardecomp$MB[,"RL"]*100,type='l',ylim=c(0,10),xlab="forecast horizon",ylab='% of variance explained by RL')
lines(1:12,varBRA$vardecomp$MB[,"RL"]*100,col='red')
lines(1:12,varPRY$vardecomp$MB[,"RL"]*100,col='green4')
lines(1:12,varURY$vardecomp$MB[,"RL"]*100,col='blue')
lines(1:12,varCOL$vardecomp$MB[,"RL"]*100,col='darkorange')
lines(1:12,varMEX$vardecomp$MB[,"RL"]*100,col='brown4')
lines(1:12,varARG$vardecomp$MB[,"RL"]*100,col='darkslategray3')
lines(1:12,varARGInf$vardecomp$MB[,"RL"]*100,col='purple')
title("Forecast Error Variance Decomposition for the Money Base")
legend("topright",c("CHL","BRA","PRY","URY","COL","MEX","ARG","ARGInf"),col=c("black","red","green4","blue","darkorange","brown4","darkslategray3","purple"), lty = 1,
       cex=.55,horiz=T)


# Issue/feature: ER is highly persistent and explained by itself. Thus, 5% explanation by RL could be even large
plot(1:12,varCHL$vardecomp$exchange[,"exchange"]*100,type='l',ylim=c(0,100),xlab="forecast horizon",ylab='% of variance explained by the Exchange Rate')
lines(1:12,varBRA$vardecomp$exchange[,"exchange"]*100,col='red')
lines(1:12,varPRY$vardecomp$exchange[,"exchange"]*100,col='green4')
lines(1:12,varURY$vardecomp$exchange[,"exchange"]*100,col='blue')
lines(1:12,varCOL$vardecomp$exchange[,"exchange"]*100,col='darkorange')
lines(1:12,varMEX$vardecomp$exchange[,"exchange"]*100,col='brown4')
lines(1:12,varARG$vardecomp$exchange[,"exchange"]*100,col='darkslategray3')
lines(1:12,varARGInf$vardecomp$exchange[,"exchange"]*100,col='purple')
title("Forecast Error Variance Decomposition for the Exchange Rate")
legend("bottomright",c("CHL","BRA","PRY","URY","COL","MEX","ARG","ARGInf"),col=c("black","red","green4","blue","darkorange","brown4","darkslategray3","purple"), lty = 1,
       cex=.55,horiz=T)

# To compare: % of H variance explained by H
plot(1:12,varCHL$vardecomp$H[,"H"]*100,type='l',ylim=c(0,100),xlab="forecast horizon",ylab='% of variance explained by the Reserves')
lines(1:12,varBRA$vardecomp$H[,"H"]*100,col='red')
lines(1:12,varPRY$vardecomp$H[,"H"]*100,col='green4')
lines(1:12,varURY$vardecomp$H[,"H"]*100,col='blue')
lines(1:12,varCOL$vardecomp$H[,"H"]*100,col='darkorange')
lines(1:12,varMEX$vardecomp$H[,"H"]*100,col='brown4')
lines(1:12,varARG$vardecomp$H[,"H"]*100,col='darkslategray3')
lines(1:12,varARG$vardecomp$H[,"H"]*100,col='purple')
title("Forecast Error Variance Decomposition for the Reserves")
legend("topright",c("CHL","BRA","PRY","URY","COL","MEX","ARG","ARGInf"),col=c("black","red","green4","blue","darkorange","brown4","darkslategray3","purple"), lty = 1,
       cex=.55,horiz=T)

## -- Granger tests (does RL cause the rest of the system)

# Granger
granger_er <- c(varCHL$granger_er$Granger$p.value, 
                varBRA$granger_er$Granger$p.value,
                varPRY$granger_er$Granger$p.value,
                varURY$granger_er$Granger$p.value,
                varCOL$granger_er$Granger$p.value,
                varMEX$granger_er$Granger$p.value,
                varARG$granger_er$Granger$p.value,
                varARGInf$granger_er$Granger$p.value) %>% round(3)

granger_h <- c(varCHL$granger_h$Granger$p.value, 
                varBRA$granger_h$Granger$p.value,
                varPRY$granger_h$Granger$p.value,
                varURY$granger_h$Granger$p.value,
                varCOL$granger_h$Granger$p.value,
                varMEX$granger_h$Granger$p.value,
                varARG$granger_h$Granger$p.value,
                varARGInf$granger_h$Granger$p.value) %>% round(3)

granger_mb <- c(varCHL$granger_mb$Granger$p.value, 
               varBRA$granger_mb$Granger$p.value,
               varPRY$granger_mb$Granger$p.value,
               varURY$granger_mb$Granger$p.value,
               varCOL$granger_mb$Granger$p.value,
               varMEX$granger_mb$Granger$p.value,
               varARG$granger_mb$Granger$p.value,
               varARGInf$granger_mb$Granger$p.value) %>% round(3)

granger_rl <- c(varCHL$granger_rl$Granger$p.value, 
               varBRA$granger_rl$Granger$p.value,
               varPRY$granger_rl$Granger$p.value,
               varURY$granger_rl$Granger$p.value,
               varCOL$granger_rl$Granger$p.value,
               varMEX$granger_rl$Granger$p.value,
               varARG$granger_rl$Granger$p.value,
               varARGInf$granger_rl$Granger$p.value) %>% round(3)

countListIso3 <- countrycode(ctrylist,origin="country.name",destinatio='iso3c')
  countListIso3[length(countListIso3)] <- "ARGInf" #Addition of Arg informal
tabGranger <- as.table(cbind(countListIso3, granger_er,granger_h,granger_mb,granger_rl))
colnames(tabGranger) <- c("Country","ER","H","MB","RL")

print(tabGranger)
# Instantaneous

instant_er <- c(varCHL$granger_er$Instant$p.value, 
                varBRA$granger_er$Instant$p.value,
                varPRY$granger_er$Instant$p.value,
                varURY$granger_er$Instant$p.value,
                varCOL$granger_er$Instant$p.value,
                varMEX$granger_er$Instant$p.value,
                varARG$granger_er$Instant$p.value,
                varARGInf$granger_er$Instant$p.value) %>% round(3)

instant_h <- c(varCHL$granger_h$Instant$p.value, 
               varBRA$granger_h$Instant$p.value,
               varPRY$granger_h$Instant$p.value,
               varURY$granger_h$Instant$p.value,
               varCOL$granger_h$Instant$p.value,
               varMEX$granger_h$Instant$p.value,
               varARG$granger_h$Instant$p.value,
               varARGInf$granger_h$Instant$p.value) %>% round(3)

instant_mb <- c(varCHL$granger_mb$Instant$p.value, 
                varBRA$granger_mb$Instant$p.value,
                varPRY$granger_mb$Instant$p.value,
                varURY$granger_mb$Instant$p.value,
                varCOL$granger_mb$Instant$p.value,
                varMEX$granger_mb$Instant$p.value,
                varARG$granger_mb$Instant$p.value,
                varARGInf$granger_mb$Instant$p.value) %>% round(3)

instant_rl <- c(varCHL$granger_rl$Instant$p.value, 
                varBRA$granger_rl$Instant$p.value,
                varPRY$granger_rl$Instant$p.value,
                varURY$granger_rl$Instant$p.value,
                varCOL$granger_rl$Instant$p.value,
                varMEX$granger_rl$Instant$p.value,
                varARG$granger_rl$Instant$p.value,
                varARGInf$granger_rl$Instant$p.value) %>% round(3)

countListIso3 <- countrycode(ctrylist,origin="country.name",destinatio='iso3c')
  countListIso3[length(countListIso3)] <- "ARGInf" #Addition of Arg informal
tabInstant <- as.table(cbind(countListIso3, instant_er,instant_h,instant_mb,instant_rl))
colnames(tabInstant) <- c("Country","ER","H","MB","RL")

print(tabInstant)

tabLags <- cbind(countListIso3, rbind(varCHL$lag$selection, varBRA$lag$selection,
      varPRY$lag$selection, varURY$lag$selection, varCOL$lag$selection, varMEX$lag$selection, 
      varARG$lag$selection, varARGInf$lag$selection)) %>% as.table()
colnames(tabLags)[1] <- "Country"

print(tabLags)


# Printing results (to paste in Latex)
library(xtable)
print(xtable(tabLags, type = "latex"),include.rownames = FALSE)

print(xtable(tabInstant, type = "latex"),include.rownames = FALSE)

print(xtable(tabGranger, type = "latex"),include.rownames = FALSE)

# -- Cointegration (multiv) tests (run manually with the last country in the loop)

# list of countries: "Chile", "Brazil", "Paraguay", "Uruguay", "Colombia", "Mexico", "Argentina", "Argentina_informal"
var <- varBRA #pick one of: varURY, varBRA, varCHL, varPRY, varMEX, varCOL, varARG, varARGInf

cer_ctry <- var$data_in_var  

lagLevel <- VARselect(log(cer_ctry)); lagLv <- lagLevel$selection["AIC(n)"]
cotest1 <- ca.jo(log(cer_ctry), type='trace', ecdet = 'none', K=lagLv) 
cotest2 <- ca.jo(log(cer_ctry), type='eigen', ecdet = 'none', K=lagLv) 
summary(cotest1); summary(cotest2)

cotest1tab <- cbind(round(cotest1@teststat,2), cotest1@cval[,2], cotest1@teststat >= cotest1@cval[,2])
colnames(cotest1tab) <- c("testStat", "5pct","Reject")
print(cotest1tab)

print(xtable(cotest1tab, type = "latex"),include.rownames = FALSE)

