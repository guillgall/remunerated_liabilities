# Exchange Rate Dynamics and the Central Banks' Balance Sheet (2024)
# Journal of International Money and Finance
# Authors: Guillermo Gallacher, Camilo Granados, Janelle Mann

# Replication code for VAR exercise based on eq (3) and four variables.
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
#path <- "~/Documents/GitHub/exchange_rates_balance_sheet/Code/VARs"; setwd(path)
cer_dat <- read.csv("cer_complete.csv", header = T)            # main data file


# Exercise for Chile, Uruguay, Paraguay, Brazil, Argentina, Colombia
ctrylist <- c("Chile","Brazil","Paraguay","Uruguay","Colombia","Mexico","Peru")#  #Argentina ERs not in sample --  done with another data file

cer_dat_latam <- cer_dat %>% filter(country %in% ctrylist) %>%
  dplyr::select(period,country,exchange,asset1.claims_on_nonresid,asset2.claims_on_otherdepos,asset3.netclaims_on_centgovt,a.liabilities_to_nonresid,b.monetary_base,c.other_liabilities,d.deposits,e.loans,f.derivatives,g.shares,h.other_items,reserves,cer_base,cer_full,cer_full_wfd) %>%
  mutate(H = reserves) %>%
  mutate(RL = c.other_liabilities + d.deposits + e.loans + f.derivatives) %>%
  mutate(MB = b.monetary_base) %>%
  mutate(ExtLiab = a.liabilities_to_nonresid/exchange) %>%
  mutate(Other = - asset2.claims_on_otherdepos - asset3.netclaims_on_centgovt + g.shares + h.other_items) %>%
  mutate(year = substr(as.character(period), start=1, stop=4)) %>%
  mutate(month = substr(as.character(period), start=6, stop=7)) %>%
  mutate_at(c("year","month"),as.numeric) 

# Remove incomplete data (end of sample)
cer_dat_latam <- cer_dat_latam %>% filter(period != "2023-09-01" & period != "2023-08-01" & period != "2023-07-01")

# Iter 2: shorter sample 2004-2019 (baseline)
shorter_sample = 1 # Set to 0 to include COVID period data
  if(shorter_sample == 1){
    cer_dat_latam <- cer_dat_latam %>% filter(year >= 2006 & year <= 2019) #Alternative: 2004
  }
  
start_year <- min(unique(cer_dat_latam$year))

VARLevels <- c("Chile","Brazil","Colombia","Peru")# True: levels, False: VAR in diffs.
#VARDiffs <- c("Chile"     "Brazil"    "Paraguay"  "Uruguay"   "Colombia"  "Mexico"    "Argentina")

  if(shorter_sample == 0){ #adjust model labeling to reflect cointegration results with COVID sample (alternative exercise)
    VARLevels <- c("Brazil","Colombia","Peru")
  }

##--Main Loop
#-Selection of data by country 
#-Estimation of VAR by country

for(i in 1:length(ctrylist)){
  ctry <- ctrylist[i] 
  
  
  if(!(ctry %in% c("Argentina","Argentina_informal","Peru"))){
    cer_ctry <- cer_dat_latam %>% filter(country == ctry) %>% dplyr::select(RL,H,MB,exchange) %>% #alternative orderings: (RL,ExtLiab,H,MB,exchange) (exchange,H,MB,RL,ExtLiab)
      ts(start=c(start_year,1),freq=12)
  } else if(ctry == "Peru") { # begin later for Peru due to NA data (and end early if recent data included)
      cer_ctry <- cer_dat_latam %>% filter(year >= 2006 & year <= 2021) %>% 
        filter(period != "2021-08-01" & period != "2021-09-01" & period != "2021-10-01" & period != "2021-11-01" & period != "2021-12-01") %>% 
        filter(country == ctry) %>% dplyr::select(RL,H,MB,exchange) %>% 
        ts(start=c(2006,1),freq=12)
    }
 
  # Country prompt:
  cat("\n","\n",ctry,"\n","\n")
  
  # Unit root tests:
  variables <- c("RL","H","MB","exchange")
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
    #Fix for AR errors
    if(ctry == "Brazil"){lag <-lag+5; var_ctry <- VAR(log(cer_ctry),p=lag,type="const")} #adj to fix errors
  
  model <- "VAR in diff of logs"
  
  if(ctry %in% VARLevels){ #Overwrite VAR with model in levels for selected countries
    lag_ctry <- VARselect(log(cer_ctry), lag.max = 6); lag <- lag_ctry$selection["AIC(n)"]
    var_ctry <- VAR(log(cer_ctry),p=lag,type="const")
      if(ctry == "Chile"){lag <-lag+1; var_ctry <- VAR(log(cer_ctry),p=lag,type="const")} #adj to fix errors
      if(ctry == "Brazil"){lag <-lag+3; var_ctry <- VAR(log(cer_ctry),p=lag,type="const")} #adj to fix errors
    model <- "VAR in levels - logs"
  }
  
  cat(ctry, "- Type of model:", model, '\n')
  summary(var_ctry)
  stargazer(var_ctry[["varresult"]],type='text')
  # takeaway: (Ch) for S all variables matter, including RL, while for others only AR terms matter. This is good, 
  # implies that eq. (11) makes sense and it's not a matter of all variables having cofounding determinants.
  
  # Testing models' stability:
  stable <- all(roots(var_ctry, modulus=T) <= 1) # good is all < 1
  
  # Granger Causality (does the 'cause' variable Granger cause the rest of the system)
  granger_er_ctry <- causality(var_ctry, cause="exchange",    vcov. = vcovHC(var_ctry))
  granger_rl_ctry <- causality(var_ctry, cause="RL",          vcov. = vcovHC(var_ctry))
  granger_h_ctry <- causality(var_ctry, cause="H",            vcov. = vcovHC(var_ctry))
  granger_mb_ctry <- causality(var_ctry, cause="MB",          vcov. = vcovHC(var_ctry))
  # S does not granger cause the system. The rest of the system does granger-cause the other variables.
  
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

  # ordering adjustment so variables order can be changed in models and outcome is not affected in plots (base order: (RL,H,MB,exchange))
  temp1 <- cbind(temp1[,"RL"], temp1[,"H"], temp1[,"MB"], temp1[,"exchange"])
  colnames(temp1) <- c("RL","H","MB","exchange")
  
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

temp6 <- t(cbind(t(varURY$vardecomp$MB[,"MB"]*100), t(varBRA$vardecomp$MB[,"MB"]*100), t(varPER$vardecomp$MB[,"MB"]*100),
                 t(varCHL$vardecomp$MB[,"MB"]*100), t(varPRY$vardecomp$MB[,"MB"]*100), t(varMEX$vardecomp$MB[,"MB"]*100), t(varCOL$vardecomp$MB[,"MB"]*100)))#,
                 #t(varARG$vardecomp$MB[,"MB"]*100), t(varARGInf$vardecomp$MB[,"MB"]*100)))

temp <- cbind(temp1, temp2, temp3, temp4, temp5, temp6) %>% as_tibble()


colnames(temp) <- c("er_rl","er_h","er_mb","er_er","h_h","h_rl","mb_rl","rl_rl","mb_mb")
country_label <- rep(c("Uruguay","Brazil","Peru", "Chile","Paraguay","Mexico","Colombia"), times = 1, each = 12)

data_fevd <- tibble(horizon = rep(1:12,7), country = country_label , temp)

# figure 3 in the paper

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
    breaks = c(0,10,20,30,40),
    labels = c("0%","10%","20%","30%","40%"),
    limits = c(0,40)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5))

g3<- data_fevd %>% 
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

library(patchwork)

layout <- 
  "AAABB#
   #CCC##"
g1 + g2  + g3 + plot_layout(design = layout) #& scale_color_manual(values=c("royalblue3","orangered3","purple4","lightseagreen","gold4","orange","gray35"))

ggsave("fevd_variables_by_rl.pdf", width = 8, height=5)


# A2: FEVD - percentage explained by itself (for each bal sheet variable)

g1 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = er_er, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% explained by itself",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,105)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Exchange Rate") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) +
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

g2 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = h_h, color = country) +
  geom_line(linewidth = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,101)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Reserves") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

g3 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = mb_mb, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "% explained by itself",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,100)
  ) + scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Monetary Base") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) +
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

g4 <- data_fevd %>% 
  mutate(country = fct_relevel(country, "Uruguay", "Brazil", "Peru", "Chile", "Paraguay", "Mexico", "Colombia")) %>%
  ggplot() + aes(x = horizon, y = rl_rl, color = country) +
  geom_line(size = 0.55) + 
  scale_y_continuous(
    name = "",
    breaks = c(0,20,40,60,80,100),
    labels = c("0%","20%","40%","60%","80%","100%"),
    limits = c(20,105)
  ) +
  scale_x_continuous(
    name = "",
    breaks = c(2,4,6,8,10,12)
  ) + ggtitle("Remunerated Liabilities") +
  theme_minimal(11) + theme(legend.position = "none", plot.title = element_text(size = 11, hjust = 0.5), axis.title.y = element_text(size = 9)) + 
  guides(color=guide_legend(title=NULL)) # removes title of legend generated with color in "aes"

(g1 | g2) / (g3 | g4) + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(color=guide_legend(title=NULL))

ggsave("fevd_allvariables_by_themselves.pdf", width = 8, height=5)


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

#countListIso3 <- countrycode(ctrylist,origin="country.name",destinatio='iso3c')
countList <- c("Uruguay","Brazil","Peru","Chile","Paraguay","Mexico","Colombia")
tabGranger <- as.table(cbind(countList, granger_er,granger_h,granger_mb,granger_rl)) 
colnames(tabGranger) <- c("Country","ER","H","MB","RL")

print(tabGranger)

# INSTANTANEOUS CAUSALITY TESTS TABLE 

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


tabInstant <- as.table(cbind(countList, instant_er,instant_h,instant_mb,instant_rl))
colnames(tabInstant) <- c("Country","ER","H","MB","RL")

print(tabInstant)

tabLags <- cbind(countList, 
                 rbind(varURY$lag, varBRA$lag, varPER$lag, varCHL$lag, 
                       varPRY$lag, varMEX$lag, varCOL$lag)) %>% as.table()

colnames(tabLags) <- c("Country","Lags (AIC*)")

print(tabLags)

#for other results in table A4
varURY$lag_all$selection
varBRA$lag_all$selection
varPER$lag_all$selection
varCHL$lag_all$selection
varPRY$lag_all$selection
varMEX$lag_all$selection
varCOL$lag_all$selection

# Printing results (to paste in Latex)
library(xtable)
print(xtable(tabLags, type = "latex"),include.rownames = FALSE)
print(xtable(tabInstant, type = "latex"),include.rownames = FALSE)
print(xtable(tabGranger, type = "latex"),include.rownames = FALSE)





