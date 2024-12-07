## ==== 20.3 Indirect least squares (ILS)====

### ==== US crop case: data set ====
# The data for US crop supply and demand case show here:

us_agr<-as_tibble(read.xlsx(here("data/Table-20-1.xlsx")))
n<-dim(us_agr)[1]

### ==== US crop case: OLS estimates for reduced equation ====

models_agr<-list(
  mod.Q = Q~X, 
  mod.P = P~X, 
  mod.Q.OLS= Q~P )
Q.red <- lm(models_agr$mod.Q, data = us_agr)
P.red <- lm(models_agr$mod.P, data = us_agr)

### ==== US crop case: calculate values ====
coef_pi1<-fun_lm_coef(models_agr$mod.P,lm.dt = us_agr)$coef[[2]]
coef_pi0<-fun_lm_coef(models_agr$mod.P,lm.dt = us_agr)$coef[[1]]
coef_pi3<-fun_lm_coef(models_agr$mod.Q,lm.dt = us_agr)$coef[[2]]
coef_pi2<-fun_lm_coef(models_agr$mod.Q,lm.dt = us_agr)$coef[[1]]
avr_P<- us_agr %>% summarise(avr_P=mean(P)) %>% unlist()
avr_Q<- us_agr %>% summarise(avr_Q=mean(Q)) %>% unlist()
avr_X<- us_agr %>% summarise(avr_X=mean(X)) %>% unlist()

## ==== 20.5 Truffle supply and demand ====

### ==== Data set ====

#truffles<-read.table("data/truffles.dat",header = F)
load(here::here("data/truffles.rda"))
names(truffles)  %<>% toupper()

### ==== Scatter ====

p_sct <- ggplot(truffles,aes(x=Q,y=P))+
  geom_point(color="blue") +
  geom_smooth(se=FALSE)+
  labs(x="Q",y="P") 

### ==== OLS regression results of the reduced SEM ====


models<-list(mod.Q=Q~PS+DI+PF, mod.P=P~PS+DI+PF)
Q.red <- lm(models$mod.Q, data=truffles)
P.red <- lm(models$mod.P, data=truffles)


### ==== Two-stage least squares (2SLS) regression results ====

require(systemfit)
eq.D <- Q~P+PS+DI
eq.S <- Q~P+PF
sys <- list(eq.D, eq.S)
instr <- ~PS+DI+PF
truff.sys <- systemfit(sys, inst=instr, 
                       method="2SLS", data=truffles)
truff.smry <- summary(truff.sys) 

### ==== Comparison: the biased OLS estimation ====

ols_D <-lm(eq.D, data = truffles)
summary(ols_D)

truff.ols <-systemfit(sys, 
                     method="OLS", data=truffles)

summary(truff.ols)

systemfit::hausman.systemfit(truff.sys,truff.ols)

