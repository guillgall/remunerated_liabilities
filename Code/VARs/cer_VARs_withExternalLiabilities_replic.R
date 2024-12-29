# Exchange Rate Dynamics and the Central Banks' Balance Sheet (2024)
# Journal of International Money and Finance
# Authors: Guillermo Gallacher, Camilo Granados, Janelle Mann

# Replication code for VAR exercise based on eq (3) and five variables (model with external liabilities)
# Date: June 2024

## Contents

# Data reading
# Calculation of VAR/VEC models by country
  # Unit root tests
  # Variables transformation
  # Cointegration tests
  # VAR Estimation
  # IRF and Variance Decomposition (FEDV)
# Plot of results
# Other results
  # Selection of model objects for tables

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
cer_dat <- read.csv("cer_complete.csv", header = T)   # contains peru


# Exercise for Chile, Uruguay, Paraguay, Brazil, Argentina, Colombia
ctrylist <- c("Chile","Brazil","Paraguay","Uruguay","Colombia","Mexico","Peru")#  #Argentina ERs not in sample --  done with another data file

cer_dat_latam <- cer_dat %>% filter(country %in% ctrylist) %>%
  dplyr::select(period,country,exchange,asset1.claims_on_nonresid,asset2.claims_on_otherdepos,asset3.netclaims_on_centgovt,a.liabilities_to_nonresid,b.monetary_base,c.other_liabilities,d.deposits,e.loans,f.derivatives,g.shares,h.other_items,reserves,cer_base,cer_full,cer_complete) %>%
  mutate(H = reserves) %>%
  mutate(RL = c.other_liabilities + d.deposits + e.loans + f.derivatives) %>%
  mutate(MB = b.monetary_base) %>%
  mutate(ExtLiab = a.liabilities_to_nonresid/exchange) %>%
  mutate(Other = - asset2.claims_on_otherdepos - asset3.netclaims_on_centgovt + g.shares + h.other_items) %>%
  mutate(year = substr(as.character(period), start=1, stop=4)) %>%
  mutate(month = substr(as.character(period), start=6, stop=7)) %>%
  mutate_at(c("year","month"),as.numeric) 

# Remove incomplete data (end of sample)
cer_dat_latam <- cer_dat_latam %>% filter(period != "2020-09-01" & period != "2020-08-01")

# Iter 2: shorter sample 2006-2019
shorter_sample = 1
  if(shorter_sample == 1){
    cer_dat_latam <- cer_dat_latam %>% filter(year >= 2006 & year <= 2019)
  }
  
start_year <- min(unique(cer_dat_latam$year))

VARLevels <- c("Chile","Brazil","Paraguay","Colombia","Peru")# True: levels, False: VAR in diffs.
#VARDiffs <- c("Chile"     "Brazil"    "Paraguay"  "Uruguay"   "Colombia"  "Mexico"    "Argentina")

##--Main Loop
#-Selection of data by country
#-Estimation of VAR by country

for(i in 1:length(ctrylist)){
  ctry <- ctrylist[i] 
  
  
  if(!(ctry %in% c("Argentina","Argentina_informal"))){
    cer_ctry <- cer_dat_latam %>% filter(country == ctry) %>% dplyr::select(ExtLiab,RL,H,MB,exchange) %>% #alternative orderings: (RL,ExtLiab,H,MB,exchange) (exchange,H,MB,RL,ExtLiab)
      ts(start=c(start_year,1),freq=12)
  } 
 
  # Country prompt:
  cat("\n","\n",ctry,"\n","\n")
  
  # Unit root tests:
  variables <- c("ExtLiab","RL","H","MB","exchange")
  cat('I(1) test:',"\n")
  for(j in variables)  cat(j,':',adf.test(cer_ctry[,j])$p.value, '\n')
  # all I(1). Series are transformed to log-diff (or log if model is set in Levels)
  
  # cointegration
  # -- Cointegration (multiv) tests
  lagLevel <- VARselect(log(cer_ctry)); lagLv <- lagLevel$selection["AIC(n)"]
  cotest1 <- ca.jo(log(cer_ctry), type='trace', ecdet = 'const', K=lagLv) 
  cotest2 <- ca.jo(log(cer_ctry), type='eigen', ecdet = 'const', K=lagLv) 
  #summary(cotest1); summary(cotest2)
  cotest1tab <- cbind(cotest1@cval[,2], round(cotest1@teststat,2), cotest1@cval[,2] <= cotest1@teststat)
  colnames(cotest1tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test (trace) Results for',ctry,'(constant)', '\n', '\n') ; print(cotest1tab)
  cotest2tab <- cbind(cotest2@cval[,2], round(cotest2@teststat,2), cotest2@cval[,2] <= cotest2@teststat)
  colnames(cotest2tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test (max eig) Results for',ctry,'(constant)', '\n', '\n') ; print(cotest2tab)
  
  cotest3 <- ca.jo(log(cer_ctry), type='trace', ecdet = 'trend', K=lagLv) 
  cotest4 <- ca.jo(log(cer_ctry), type='eigen', ecdet = 'trend', K=lagLv) 
  #summary(cotest1); summary(cotest2)
  cotest3tab <- cbind(cotest3@cval[,2], round(cotest3@teststat,2), cotest3@cval[,2] <= cotest3@teststat)
  colnames(cotest3tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test (trace) Results for',ctry,'(w/ trend)', '\n', '\n') ; print(cotest3tab)
  cotest4tab <- cbind(cotest4@cval[,2], round(cotest4@teststat,2), cotest4@cval[,2] <= cotest4@teststat)
  colnames(cotest4tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test (max eig) Results for',ctry,'(w/ trend)' ,'\n', '\n') ; print(cotest4tab)
  
  cotest01 <- ca.jo(log(cer_ctry), type='trace', ecdet = 'none', K=lagLv) 
  cotest02 <- ca.jo(log(cer_ctry), type='eigen', ecdet = 'none', K=lagLv) 
  cotest01tab <- cbind(cotest01@cval[,2], round(cotest01@teststat,2), cotest01@cval[,2] <= cotest01@teststat)
  cotest02tab <- cbind(cotest02@cval[,2], round(cotest02@teststat,2), cotest02@cval[,2] <= cotest02@teststat)
  colnames(cotest01tab) <- c("5pct","testStat","Reject")
  colnames(cotest02tab) <- c("5pct","testStat","Reject")
  cat('\n', 'Cointegration Test (trace) Results for',ctry,'(no deterministic component)', '\n', '\n') ; print(cotest01tab)
  cat('\n', 'Cointegration Test (max eig) Results for',ctry,'(no deterministic component)', '\n', '\n') ; print(cotest02tab)
  
  # Saving cointegration test results
  coint_tra_none  <- cotest01tab
  coint_eig_none  <- cotest02tab
  coint_tra_const <- cotest1tab
  coint_eig_const <- cotest2tab
  coint_tra_trend <- cotest3tab
  coint_eig_trend <- cotest4tab
  labels <- c("tab1: trace/const", "tab2: maxeig/const", "tab3: trace/trend", "tab4: maxeig/trend")
  
  # VAR model estimation lag.max = 6
  cer_ctry_dlog <- diff(log(cer_ctry)) %>% ts(start=c(start_year,2),freq=12) 
  # Optimal lag length
  lag_ctry <- VARselect(cer_ctry_dlog, lag.max = 6) ; cat('\n')#; print(lag_ctry$selection) 
  lag <- lag_ctry$selection["AIC(n)"]
  # Estimation
  var_ctry <- VAR(cer_ctry_dlog,p=lag,type="const")
    if(ctry == "Mexico"){lag <-lag; 
      #dum1 <- 1*(time(cer_ctry_dlog) == 2009.250); dum2 <- 1*(time(cer_ctry_dlog) == 2008.750);
      #dummies <- data_frame("dum1"=dum1,"dum2"=dum2); dummies <- ts(dummies, end = c(2019,12), freq= 12)
      var_ctry <- VAR(cer_ctry_dlog,p=lag,type="const",season=12)} #adj to fix errors
  model <- "VAR in diff of logs"
  
  if(ctry %in% VARLevels){ #Overwrite VAR with model in levels for selected countries
    lag_ctry <- VARselect(log(cer_ctry), lag.max = 6); lag <- lag_ctry$selection["AIC(n)"]
    var_ctry <- VAR(log(cer_ctry),p=lag,type="const")
      if(ctry == "Chile"){lag <-lag+2; var_ctry <- VAR(log(cer_ctry),p=lag,type="const")} #adj to fix errors
      if(ctry == "Brazil"){lag <-lag+5; var_ctry <- VAR(log(cer_ctry),p=lag,type="const")} #adj to fix errors
      if(ctry == "Peru"){lag <-lag+2; var_ctry <- VAR(cer_ctry_dlog,p=lag,type="const")} #adj to fix errors
    model <- "VAR in levels - logs"
  }
  
  cat(ctry, "- Type of model:", model, '\n')
  summary(var_ctry)
  stargazer(var_ctry[["varresult"]],type='text')

  # Testing models' stability:
  stable <- all(roots(var_ctry, modulus=T) <= 1) # good is all < 1
  
  # Granger Causality (does the 'cause' variable Granger cause the rest of the system)
  granger_er_ctry <- causality(var_ctry, cause="exchange",    vcov. = vcovHC(var_ctry))
  granger_extl_ctry <- causality(var_ctry, cause="ExtLiab",   vcov. = vcovHC(var_ctry))
  granger_rl_ctry <- causality(var_ctry, cause="RL",          vcov. = vcovHC(var_ctry))
  granger_h_ctry <- causality(var_ctry, cause="H",            vcov. = vcovHC(var_ctry))
  granger_mb_ctry <- causality(var_ctry, cause="MB",          vcov. = vcovHC(var_ctry))

  # IRFs and FEVD
  #irf_ctry <- irf(var_ctry, n=24, cumulative=T, boot= F, run = 10000, ci= 0.95)
  #plot(irf_ctry)
  vd_ctry <- vars::fevd(var_ctry, n.ahead = 12)
  #plot(vd_ctry)
  
  # Diagnostic tests for VAR model 
  #Serial correlation
  arres_ctry <- serial.test(var_ctry, lags.pt =4, lags.bg = 4, type = "ES") #type = c("PT.asymptotic","PT.adjusted","BG","ES")
  arres_ctry$serial$p.value # (I verify it passes at least 1 type of test in each country)
  
  #Normality of residuals
  normal_ctry <- normality.test(var_ctry,multivariate.only=T)
  normal_ctry$jb.mul$JB$p.value
  #Structural breaks of residuals
  stab_var_ctry <- vars::stability(var_ctry, type = "OLS-CUSUM")
  plot(stab_var_ctry)
  
  
  # Saving results by country
  list_var <- list(var = var_ctry, 
                   lag = lag, 
                   lag_all = lag_ctry,
                   stable = stable,
                   coint = list(labels,tab1=cotest1tab,tab2=cotest2tab,tab3=cotest3tab,tab4=cotest4tab),
                   granger_extl = granger_extl_ctry,
                   granger_er = granger_er_ctry, 
                   granger_rl = granger_rl_ctry,
                   granger_h = granger_h_ctry, 
                   granger_mb = granger_mb_ctry, 
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
temp1 <- rbind(varURY$vardecomp$exchange*100, varBRA$vardecomp$exchange*100, varPER$vardecomp$exchange*100, 
         varCHL$vardecomp$exchange*100, varPRY$vardecomp$exchange*100, varMEX$vardecomp$exchange*100, 
         varCOL$vardecomp$exchange*100) #, varARG$vardecomp$exchange*100 )

  # ordering adjustment so variables order can be changed in models are outcome is not affected in plots (base order: (ExtLiab,RL,H,MB,exchange))
  temp1 <- cbind(temp1[,"ExtLiab"], temp1[,"RL"], temp1[,"H"], temp1[,"MB"], temp1[,"exchange"])
  colnames(temp1) <- c("ExtLiab","RL","H","MB","exchange")
  
temp2 <- t(cbind(t(varURY$vardecomp$H[,"H"]*100), t(varBRA$vardecomp$H[,"H"]*100), t(varPER$vardecomp$H[,"H"]*100),
                 t(varCHL$vardecomp$H[,"H"]*100), t(varPRY$vardecomp$H[,"H"]*100), t(varMEX$vardecomp$H[,"H"]*100), t(varCOL$vardecomp$H[,"H"]*100)))#,
                 #t(varARG$vardecomp$H[,"H"]*100),t(varARGInf$vardecomp$H[,"H"]*100)))

temp3 <- t(cbind(t(varURY$vardecomp$H[,"RL"]*100), t(varBRA$vardecomp$H[,"RL"]*100), t(varPER$vardecomp$H[,"RL"]*100),
                 t(varCHL$vardecomp$H[,"RL"]*100), t(varPRY$vardecomp$H[,"RL"]*100), t(varMEX$vardecomp$H[,"RL"]*100), t(varCOL$vardecomp$H[,"RL"]*100)))#,
                 #t(varARG$vardecomp$H[,"RL"]*100), t(varARGInf$vardecomp$H[,"RL"]*100)))

temp4 <- t(cbind(t(varURY$vardecomp$MB[,"RL"]*100), t(varBRA$vardecomp$MB[,"RL"]*100), t(varPER$vardecomp$MB[,"RL"]*100),
                 t(varCHL$vardecomp$MB[,"RL"]*100), t(varPRY$vardecomp$MB[,"RL"]*100), t(varMEX$vardecomp$MB[,"RL"]*100), t(varCOL$vardecomp$MB[,"RL"]*100)))#,
                 #t(varARG$vardecomp$MB[,"RL"]*100), t(varARGInf$vardecomp$MB[,"RL"]*100)))

# add new columns
temp5 <- t(cbind(t(varURY$vardecomp$RL[,"RL"]*100), t(varBRA$vardecomp$RL[,"RL"]*100), t(varPER$vardecomp$RL[,"RL"]*100),
                 t(varCHL$vardecomp$RL[,"RL"]*100), t(varPRY$vardecomp$RL[,"RL"]*100), t(varMEX$vardecomp$RL[,"RL"]*100), t(varCOL$vardecomp$RL[,"RL"]*100)))#,
                 #t(varARG$vardecomp$RL[,"RL"]*100), t(varARGInf$vardecomp$RL[,"RL"]*100)))

temp6 <- t(cbind(t(varURY$vardecomp$ExtLiab[,"RL"]*100), t(varBRA$vardecomp$ExtLiab[,"RL"]*100), t(varPER$vardecomp$ExtLiab[,"RL"]*100),
                 t(varCHL$vardecomp$ExtLiab[,"RL"]*100), t(varPRY$vardecomp$ExtLiab[,"RL"]*100), t(varMEX$vardecomp$ExtLiab[,"RL"]*100), t(varCOL$vardecomp$ExtLiab[,"RL"]*100)))#,
#t(varARG$vardecomp$RL[,"RL"]*100), t(varARGInf$vardecomp$RL[,"RL"]*100)))

temp7 <- t(cbind(t(varURY$vardecomp$MB[,"MB"]*100), t(varBRA$vardecomp$MB[,"MB"]*100), t(varPER$vardecomp$MB[,"MB"]*100),
                 t(varCHL$vardecomp$MB[,"MB"]*100), t(varPRY$vardecomp$MB[,"MB"]*100), t(varMEX$vardecomp$MB[,"MB"]*100), t(varCOL$vardecomp$MB[,"MB"]*100)))#,
                 #t(varARG$vardecomp$MB[,"MB"]*100), t(varARGInf$vardecomp$MB[,"MB"]*100)))

# % explained by ExtLiabs:
temp8 <- t(cbind(t(varURY$vardecomp$H[,"ExtLiab"]*100), t(varBRA$vardecomp$H[,"ExtLiab"]*100), t(varPER$vardecomp$H[,"ExtLiab"]*100),
                 t(varCHL$vardecomp$H[,"ExtLiab"]*100), t(varPRY$vardecomp$H[,"ExtLiab"]*100), t(varMEX$vardecomp$H[,"ExtLiab"]*100), t(varCOL$vardecomp$H[,"ExtLiab"]*100)))#,


temp9 <- t(cbind(t(varURY$vardecomp$MB[,"ExtLiab"]*100), t(varBRA$vardecomp$MB[,"ExtLiab"]*100), t(varPER$vardecomp$MB[,"ExtLiab"]*100),
                 t(varCHL$vardecomp$MB[,"ExtLiab"]*100), t(varPRY$vardecomp$MB[,"ExtLiab"]*100), t(varMEX$vardecomp$MB[,"ExtLiab"]*100), t(varCOL$vardecomp$MB[,"ExtLiab"]*100)))#,


temp10 <- t(cbind(t(varURY$vardecomp$RL[,"ExtLiab"]*100), t(varBRA$vardecomp$RL[,"ExtLiab"]*100), t(varPER$vardecomp$RL[,"ExtLiab"]*100),
                 t(varCHL$vardecomp$RL[,"ExtLiab"]*100), t(varPRY$vardecomp$RL[,"ExtLiab"]*100), t(varMEX$vardecomp$RL[,"ExtLiab"]*100), t(varCOL$vardecomp$RL[,"ExtLiab"]*100)))#,


temp11 <- t(cbind(t(varURY$vardecomp$ExtLiab[,"ExtLiab"]*100), t(varBRA$vardecomp$ExtLiab[,"ExtLiab"]*100), t(varPER$vardecomp$ExtLiab[,"ExtLiab"]*100),
                 t(varCHL$vardecomp$ExtLiab[,"ExtLiab"]*100), t(varPRY$vardecomp$ExtLiab[,"ExtLiab"]*100), t(varMEX$vardecomp$ExtLiab[,"ExtLiab"]*100), t(varCOL$vardecomp$ExtLiab[,"ExtLiab"]*100)))#,


temp <- cbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11) %>% as_tibble()

colnames(temp) <- c("er_extliab","er_rl","er_h","er_mb","er_er","h_h","h_rl","mb_rl","rl_rl","extliab_rl","mb_mb", "h_extliab", "mb_extliab", "rl_extliab", "extliab_extliab")
country_label <- rep(c("Uruguay","Brazil","Peru", "Chile","Paraguay","Mexico","Colombia"), times = 1, each = 12)

data_fevd <- tibble(horizon = rep(1:12,7), country = country_label , temp)

# figure A3 in the paper (appendix)
g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = er_rl, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RDL",
    breaks = c(0,2,4,6,8,10,12,14,16),
    labels = c("0%","2%","4%","6%","8%","10%","12%","14%", "16%")
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))
  
g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = h_rl, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,10,20,30,40,50),
    labels = c("0%","10%","20%","30%","40%","50%"),
    limits = c(0,50)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5))

g3 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = mb_rl, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RDL",
    breaks = c(0,10,20,30,40,50),
    labels = c("0%","10%","20%","30%","40%","50%"),
    limits = c(0,50)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Monetary Base") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

g4 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = extliab_rl, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%"),
    limits = c(0,10)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("External Liabilities") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"
    
library(patchwork)

#layout <- 
#  "AAABB#
   #CCC##"
#g1 + g2  + g4 + plot_layout(design = layout) #& scale_color_manual(values=c("royalblue3","orangered3","purple4","lightseagreen","gold4","orange","gray35"))
(g1 | g2) / (g3 | g4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("fevd_variables_by_rl_ModelExtLiab.pdf", width = 8, height=5)


# Figure A4: % of variance explained by ExtLiab 

g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = er_extliab, color = country) +
  geom_line(linewidth = 0.55) +
  scale_y_continuous(
    name = "% of variance explained by EL",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%"),
    limits = c(0,10)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))



g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = h_extliab, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,5,10,15,20,25),
    labels = c("0%","5%","10%","15%","20%","25%"),
    limits = c(0,25)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5))

g3 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = mb_extliab, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by EL",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%"),
    limits = c(0,10)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Monetary Base") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

g4 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = rl_extliab, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,2,4,6,8,10),
    labels = c("0%","2%","4%","6%","8%","10%"),
    limits = c(0,10)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Remunerated Domestic Liabilities") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

(g1 | g2) / (g3 | g4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("fevd_variables_by_extliab.pdf", width = 8, height=5)

## Other figures (not shown on the paper)

# FEVD - percentage explain by itself for each variable (1-3 out of 5 variables)
g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = mb_mb, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by MB",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,100)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Monetary Base") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))

g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = rl_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by RDL",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,100)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Remunerated Liabilities") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))

g3 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = extliab_extliab, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by EL",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,101)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("External Liabilities") +
  theme_minimal(11) + theme(legend.position = "right", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"


layout <- "AAABB#
           #CCC##"

g1 + g2  + g3 + plot_layout(design = layout) #& scale_color_manual(values=c("royalblue3","orangered3","purple4","lightseagreen","gold4","orange","gray35"))
ggsave("fevd_mb_rl_extliab_by_themselves.pdf", width = 8, height=4)


# FEVD - percentage explain by itself for each variable (4-5 out of 5 variables)

g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = er_er, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by ER",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,105)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))


g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = h_h, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% of variance explained by Reserves",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,105)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9))


(g1 | g2) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("fevd_er_h_by_themselves.pdf", width = 8, height=4)


### Other results of the models (not shown on the paper)

## -- Granger tests (does RL cause the rest of the system)

# Granger
granger_er <- c(varURY$granger_er$Granger$p.value,
                varBRA$granger_er$Granger$p.value,
                varPER$granger_er$Granger$p.value,
                varCHL$granger_er$Granger$p.value, 
                varPRY$granger_er$Granger$p.value,
                varMEX$granger_er$Granger$p.value,
                varCOL$granger_er$Granger$p.value) %>% round(3)

granger_h <- c(varURY$granger_h$Granger$p.value,
               varBRA$granger_h$Granger$p.value,
               varPER$granger_h$Granger$p.value,
               varCHL$granger_h$Granger$p.value, 
               varPRY$granger_h$Granger$p.value,
               varMEX$granger_h$Granger$p.value,
               varCOL$granger_h$Granger$p.value) %>% round(3)

granger_mb <- c(varCHL$granger_mb$Granger$p.value, 
               varBRA$granger_mb$Granger$p.value,
               varPRY$granger_mb$Granger$p.value,
               varPER$granger_mb$Granger$p.value,
               varURY$granger_mb$Granger$p.value,
               varMEX$granger_mb$Granger$p.value,
               varCOL$granger_mb$Granger$p.value) %>% round(3)

granger_rl <- c(varURY$granger_rl$Granger$p.value,
                varBRA$granger_rl$Granger$p.value,
                varPER$granger_rl$Granger$p.value,
                varCHL$granger_rl$Granger$p.value,
                varPRY$granger_rl$Granger$p.value,
                varMEX$granger_rl$Granger$p.value,
                varCOL$granger_rl$Granger$p.value) %>% round(3)

granger_extliab <- c(varURY$granger_extl$Granger$p.value,
                varBRA$granger_extl$Granger$p.value,
                varPER$granger_extl$Granger$p.value,
                varCHL$granger_extl$Granger$p.value,
                varPRY$granger_extl$Granger$p.value,
                varMEX$granger_extl$Granger$p.value,
                varCOL$granger_extl$Granger$p.value) %>% round(3)

countList <- c("Uruguay","Brazil","Peru","Chile","Paraguay","Mexico","Colombia")
tabGranger <- as.table(cbind(countList, granger_er,granger_h,granger_mb,granger_rl,granger_extliab)) 
colnames(tabGranger) <- c("Country","ER","H","MB","RL","EL")

print(tabGranger)

# Instantaneous
instant_er <- c(varURY$granger_er$Instant$p.value,
                varBRA$granger_er$Instant$p.value,
                varPER$granger_er$Instant$p.value,
                varCHL$granger_er$Instant$p.value, 
                varPRY$granger_er$Instant$p.value,
                varMEX$granger_er$Instant$p.value,
                varCOL$granger_er$Instant$p.value) %>% round(3)

instant_h <- c(varURY$granger_h$Instant$p.value,
               varBRA$granger_h$Instant$p.value,
               varPER$granger_h$Instant$p.value,
               varCHL$granger_h$Instant$p.value, 
               varPRY$granger_h$Instant$p.value,
               varMEX$granger_h$Instant$p.value,
               varCOL$granger_h$Instant$p.value) %>% round(3)

instant_mb <- c(varURY$granger_mb$Instant$p.value,
                varBRA$granger_mb$Instant$p.value,
                varPER$granger_mb$Instant$p.value,
                varCHL$granger_mb$Instant$p.value, 
                varPRY$granger_mb$Instant$p.value,
                varMEX$granger_mb$Instant$p.value,
                varCOL$granger_mb$Instant$p.value) %>% round(3)

instant_rl <- c(varURY$granger_rl$Instant$p.value,
                varBRA$granger_rl$Instant$p.value,
                varPER$granger_rl$Instant$p.value,
                varCHL$granger_rl$Instant$p.value, 
                varPRY$granger_rl$Instant$p.value,
                varMEX$granger_rl$Instant$p.value,
                varCOL$granger_rl$Instant$p.value) %>% round(3)

instant_extliab <- c(varURY$granger_extl$Instant$p.value,
                varBRA$granger_extl$Instant$p.value,
                varPER$granger_extl$Instant$p.value,
                varCHL$granger_extl$Instant$p.value, 
                varPRY$granger_extl$Instant$p.value,
                varMEX$granger_extl$Instant$p.value,
                varCOL$granger_extl$Instant$p.value) %>% round(3)

tabInstant <- as.table(cbind(countList, instant_er,instant_h,instant_mb,instant_rl,instant_extliab))
colnames(tabInstant) <- c("Country","ER","H","MB","RL","EL")

print(tabInstant)

tabLags <- cbind(countList, 
                 rbind(varURY$lag, varBRA$lag, varPER$lag, varCHL$lag, 
                       varPRY$lag, varMEX$lag, varCOL$lag)) %>% as.table()

colnames(tabLags) <- c("Country","Lags (AIC*)")

print(tabLags)


# Printing results (to paste in Latex)
library(xtable)
print(xtable(tabLags, type = "latex"),include.rownames = FALSE)
print(xtable(tabInstant, type = "latex"),include.rownames = FALSE)
print(xtable(tabGranger, type = "latex"),include.rownames = FALSE)

# -- Cointegration (multiv) tests (run manually with the last country in the loop)
# Change country code part of line below to see results for each country (e.g., varCOL$coint for Colombia)
varURY$coint 



