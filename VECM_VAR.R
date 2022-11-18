#Personal functions
source("/Users/anthonycarr/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/Second Year/Second Semester/Time Series/intord.R")
source("/Users/anthonycarr/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/Second Year/Second Semester/Time Series/ARMA_test.R")
load("~/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/Second Year/Second Semester/Time Series/problem_set_5_data.Rda")
pacman::p_load(vars, forecast, urca, broom)
###############
#Stationarity
apply(ps5_data, 2, function(x){intord(x)})

#Engel Granger
#both have to be the same order of integration <0 / residuals create stationary series
regression <- lm(ps5_data$fire~ ps5_data$serv)
#intord of residuals. Are cointegrated
#Both I 1 and linear combination is stationary
intord(regression$residuals)


#Making a TS object
names <- colnames(ps5_data)
counter <- 1
for (col in ps5_data){
  if (counter >= 4) {counter=1}
  assign(names[counter], ts( col, start=c(1,1990), frequency = 12 ))
  counter <- counter + 1
}


data <- cbind(fire, serv)

#VAR 4
#Each seasonal matrix has different significance because of the difference cycles in 
#each sector
VectorAutoRegression <- vars::VAR(data, p=4) 
table0 <- tidy(VectorAutoRegression)
#write.csv(table0, "/Users/anthonycarr/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/Second Year/Second Semester/Time Series/Problem Set 5/Vartable.csv")
summary(VectorAutoRegression)
print('AIC:     ') ; AIC(VectorAutoRegression); print('BIC:'    ) ;BIC(VectorAutoRegression)

VectorAutoRegression_season <- vars::VAR(data, p=4, season = 12) 
summary(VectorAutoRegression_season)
table1 <- tidy(VectorAutoRegression_season)
#write.csv(table1, "/Users/anthonycarr/Library/CloudStorage/OneDrive-TheGeorgeWashingtonUniversity/Second Year/Second Semester/Time Series/Problem Set 5/Vartable_season.csv")
print('AIC:     ') ; AIC(VectorAutoRegression_season); print('BIC:     ') ; BIC(VectorAutoRegression_season)



#impulse response function #Look up irf functions for changing magnitudes
impulse_VAR_fire <- vars::irf(VectorAutoRegression_season,  n.ahead = 20, impulse = 'fire', ortho = T)
impulse_VAR_serv <- vars::irf(VectorAutoRegression_season,  n.ahead = 20, impulse = 'serv', ortho = T)





#Vector error correction model. KEEP THIS ONE DISPITE AIC/BIC
jo_test <- ca.jo(data, season = 12, K=4)
VECM <- vec2var(jo_test)


  
 #Impulse functions 
impulse_VECM_fire <- irf(VECM, impulse = 'fire', n.ahead = 20, ortho = T)
impulse_VECM_serv <- irf(VECM, impulse = 'serv', n.ahead = 20, ortho = T)


BIC(VECM)
AIC(VECM)
################
#stationarity
intord(ffr_data$FFR)
intord(ffr_data$INFL_EXP)


#making ts object
names <- colnames(ffr_data)
counter <- 1
for (col in ffr_data[-c(6,7)]){
  if (counter == 6) {counter=1}
  assign(names[counter], ts( col, start=c(11,1982), frequency = 12 ))
  counter <- counter + 1
}


#cointegration
#CANNOT BE COINTEGRATED B/C THEY ARE NOT SAME ORDER OF INTEGRSATION
regression2 <- lm(ffr_data$FFR~ ffr_data$INFL_EXP)
intord(regression2$residuals)
endo <- cbind(FFR, INFL_EXP)
exo <- cbind(GAP, SPREAD)
jo_test2 <- ca.jo(endo)


#VAR model 4
VectorAutoRegression_2 <- VAR(endo, p=4)
impulse_VAR_FF <- irf(VectorAutoRegression_2, impulse='INFL_EXP', n.ahead = 10, ortho = T)


#VAR model 2
VectorAutoRegression_3 <- VAR(endo, p=2, exogen = exo)
impulse_VAR_UNEMP <- irf(VectorAutoRegression_3, impulse="FFR", n.ahead = 10)
plot(impulse_VAR_UNEMP)
#Vector Error Correction Model
VECM_2 <- vec2var(jo_test2)


#AIC model selection
AIC(VectorAutoRegression_2) ; AIC(VectorAutoRegression_3); AIC(VECM_2)

#BIC model selection
BIC(VectorAutoRegression_2) ; BIC(VectorAutoRegression_3); BIC(VECM_2)
