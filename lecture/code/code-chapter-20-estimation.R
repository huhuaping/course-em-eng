# R packages====
library(here)
library(magrittr)
library(tidyverse)
library(xmerit)
library(scales)

# 20.3 Indirect least squares (ILS)====
## US crop case
## variables ====
agr_var <- tribble(
  ~`vars`, ~`label`, ~`note`,
  "Q", "Crop yield index", "(1996=100)",
  "P", "Agricultural products purchasing prices index", "(1990-1992=100)",
  "X", "Capital personal consumption expenditure", "(In 2007 dollars)"
)

## data set ====
# The data for US crop supply and demand case show here:

us_agr <- as_tibble(read.xlsx(here("data/Table-20-1.xlsx")))
n_agr <- dim(us_agr)[1]

## OLS estimates for reduced equation ====

models_agr <- list(
  mod.Q = formula(Q ~ X),
  mod.P = formula(P ~ X),
  mod.Q.OLS = formula(Q ~ P)
)
Q.red <- lm(models_agr$mod.Q, data = us_agr)
P.red <- lm(models_agr$mod.P, data = us_agr)


## US crop case: calculate values ====
## extract coefficients
# old codes for reference, not used
# coef_pi1 <- fun_lm_coef(models_agr$mod.P, lm.dt = us_agr)$coef[[2]]
# coef_pi0 <- fun_lm_coef(models_agr$mod.P, lm.dt = us_agr)$coef[[1]]
# coef_pi3 <- fun_lm_coef(models_agr$mod.Q, lm.dt = us_agr)$coef[[2]]
# coef_pi2 <- fun_lm_coef(models_agr$mod.Q, lm.dt = us_agr)$coef[[1]]

coef_pi1 <- coef(P.red)[2]
coef_pi0 <- coef(P.red)[1]
coef_pi3 <- coef(Q.red)[2]
coef_pi2 <- coef(Q.red)[1]

avr_P <- us_agr %>%
  summarise(avr_P = mean(P)) %>%
  unlist()
avr_Q <- us_agr %>%
  summarise(avr_Q = mean(Q)) %>%
  unlist()
avr_X <- us_agr %>%
  summarise(avr_X = mean(X)) %>%
  unlist()

# 20.4 Two-stage least square method (2SLS)====

## Case study and application for 2SLS approach
## Income and money supply case

## variable description====
var_income <- tribble(
  ~`vars`, ~`label`, ~`note`,
  "Y1", "GDP:  gross domestic product", "($1 billion in 2000)",
  "Y2", "M2:money supply", "($1 billion)",
  "X1", "GDPI: Total private domestic investment ", "($1 billion in 2000)",
  "X2", "FEDEXP: Federal expenditure ", "($1 billion)",
  "Y1.l1", "GDP_t-1: The gross domestic product of the previous period", "($1 billion in 2000)",
  "Y2.l1", "M2_t-1: The money supply in the previous period", "($1 billion)",
  "X3", "TB6: 6 month Treasury bond interest rate ", "(%)"
)

## data set====

us_money <- as_tibble(read.xlsx(here("data/Table-20-2.xlsx"))) %>%
  mutate(
    Y1.l1 = dplyr::lag(Y1),
    Y2.l1 = dplyr::lag(Y2)
  ) %>%
  select(-X3)
n_money <- dim(us_money)[1]

##  Modeling scenario 1 and 2SLS without error correction====
## Only the money supply equation is overidentifiable

### setting up the models====
models_money <- list(
  mod.Y1 = formula(Y1 ~ Y2 + X1 + X2),
  mod.Y2 = formula(Y2 ~ Y1 + X2),
  mod.instr = formula(Y1 ~ X1 + X2),
  mod.stage2 = formula(Y2 ~ Y1.hat),
  mod.ols = formula(Y2 ~ Y1)
)

### stage 1 regression====
money_out <- lm(models_money$mod.instr, data = us_money)

us_money_new <- us_money %>%
  mutate(
    Y1.hat = round(money_out$fitted.values, 4),
    v1.hat = round(money_out$residuals, 4)
  )

### OLS approach directly====
ols_money <- lm(models_money$mod.ols, data = us_money_new)

## Modeling scenario 2====
## both income equation and money supply equation are over-identifiable

### setting up the models====
models_money2 <- list(
  mod2.Y1 = formula(Y1 ~ Y2 + X1 + X2),
  mod2.Y2 = formula(Y2 ~ Y1 + Y1.l1 + Y2.l1),
  mod2.instr.1 = formula(Y1 ~ X1 + X2 + Y1.l1 + Y2.l1),
  mod2.instr.2 = formula(Y2 ~ X1 + X2 + Y1.l1 + Y2.l1),
  mod2.stage2.1 = formula(Y1 ~ Y2.hat + X1 + X2),
  mod2.stage2.2 = formula(Y2 ~ Y1.hat + Y1.l1 + Y2.l1)
)

### stage 1 regression====
money2_instr_out1 <- lm(models_money2$mod2.instr.1, data = us_money)
money2_instr_out2 <- lm(models_money2$mod2.instr.2, data = us_money)

### stage 1 fitness====
us_money_new2 <- us_money %>%
  mutate(
    Y1.hat = c(NA, money2_instr_out1$fitted.values),
    v1.hat = c(NA, money2_instr_out1$residuals),
    Y2.hat = c(NA, money2_instr_out2$fitted.values),
    v2.hat = c(NA, money2_instr_out2$residuals)
  )

### stage 2 regression for income equation====
tab_income_s2 <- lm(models_money2$mod2.stage2.1, data = us_money_new2)

### stage 2 regression for money supply equation====
tab_money_s2 <- lm(models_money2$mod2.stage2.2, data = us_money_new2)

### Simultaneous 2SLS approach with error correction automatically====

require(systemfit)
sys <- list(
  eq1 = Y1 ~ Y2 + X1 + X2,
  eq2 = Y2 ~ Y1 + Y1.l1 + Y2.l1
)
instr <- ~ X1 + X2 + Y1.l1 + Y2.l1
money.sys <- systemfit(sys,
  inst = instr,
  method = "2SLS", data = us_money
)
money.smry <- summary(money.sys)

rnames <- rownames(money.smry$coefficients)
cnames <- colnames(money.smry$coefficients)
tab_smry <- as_tibble(money.smry$coefficients)
names(tab_smry) <- cnames
tab_smry$vars <- rnames
tab_smry <- tab_smry %>%
  separate(vars, into = c("eq", "vars"), sep = "_") %>%
  select(eq, vars, everything())

### Comparison to OLS approach directly (biased estimation)====


# 20.5 Truffle supply and demand ====

## variable description====
truffles_var <- tibble::tribble(
  ~`vars`, ~`label`, ~`measure`,
  "P", "market price of trufﬂes", "dollar/ounce",
  "Q", "market quantity of trufﬂes", "ounce",
  "PS", "market price of substitute", "dollar/ounce",
  "DI", "disposable income", "dollar/person, monthly",
  "PF", "rental price of trufﬂe-pigs", "dollar/hour"
)

## Data set ====

# truffles<-read.table("data/truffles.dat",header = F)
load(here::here("data/truffles.rda"))
names(truffles) %<>% toupper()

## Scatter ====

p_sct <- ggplot(truffles, aes(x = Q, y = P)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE) +
  labs(x = "Q", y = "P")

## OLS regression directly ====
eq.D <- formula(Q ~ P + PS + DI)
eq.S <- formula(Q ~ P + PF)
# fit using direct `OLS` method
ols.D <- lm(formula = eq.D, data = truffles)
ols.S <- lm(formula = eq.S, data = truffles)
# estimation summary
smry.olsD <- summary(ols.D)
smry.olsS <- summary(ols.S)

## OLS regression results of the reduced SEM ====

models <- list(
  mod.Q = formula(Q ~ PS + DI + PF),
  mod.P = formula(P ~ PS + DI + PF)
)
Q.red <- lm(models$mod.Q, data = truffles)
P.red <- lm(models$mod.P, data = truffles)

## Simultaneous OLS solution ====
# load pkg
require(systemfit)
# set equation systems
# eq.D <- formula(Q ~ P + PS + DI)
# eq.S <- formula(P ~ Q + PF)
eq.sys <- list(eq.D, eq.S)
# system fit using `OLS` method
system.ols <- systemfit(
  formula = eq.sys,
  method = "OLS",
  data = truffles
)
# estimation summary
smry.ols <- summary(system.ols)


## IV-Two-stage least squares (2SLS) regression results ====

# load pkg
require(systemfit)
# set equation systems
# eq.D <- formula(Q ~ P + PS + DI)
# eq.S <- formula(P ~ Q + PF)
# eq.sys <- list(eq.D, eq.S)
# set instruments
instr <- ~ PS + DI + PF
# system fit using `2SLS` method
system.iv <- systemfit(
  formula = eq.sys, inst = instr,
  method = "2SLS",
  data = truffles
)
# estimation summary
smry.iv <- summary(system.iv)

## IV-2SLS Solution: tidy report (`symtemfit`)====

# Tidy `systemfit` fit results into tibble
## my custom Helper function
source(here("lecture/code/helper-tidy-systemfit.R"))
tbl_iv <- tidy_systemfit(fit.sys = smry.iv)

## Comparison: the biased OLS estimation ====
### reduced model
models <- list(
  mod.Q = formula(Q ~ PS + DI + PF),
  mod.P = formula(P ~ PS + DI + PF)
)
# Q.red <- lm(models$mod.Q, data = truffles)
# P.red <- lm(models$mod.P, data = truffles)


## Simultaneous OLS solution and Hausman test ====
## Hausman specification test for consistency of the 3SLS estimation
## not used in the lecture

# systemfit::hausman.systemfit(system.iv, system.ols)

# 20.6 Cod supply and demand ====

## variable description====

fish_var <- tribble(
  ~`vars`, ~`label`, ~`note`,
  "lprice", "the logarithmic market price of cod", "continuous variable",
  "lquan", "the logarithmic quantity of cod", "continuous variable",
  "mon", "monday", "dummy variable 0/1",
  "tue", "tuesday", "dummy variable 0/1",
  "wen", "wensday", "dummy variable 0/1",
  "thu", "thursday", "dummy variable 0/1",
  "stormy", "Stormy", "dummy variable 0/1"
)

## data set====

load(here("data/fultonfish.rda"))
fultonfish <- as_tibble(fultonfish) %>%
  mutate(
    date = str_c("19", date),
    date = ymd(date)
  ) %>%
  select(date:stormy)
n_fish <- dim(fultonfish)[1]

## Scatter====

p_sct_fish <- ggplot(fultonfish, aes(x = lquan, y = lprice)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE, color = "red") +
  labs(x = "lquan", y = "lprice")

## OLS regression results of the reduced SEM====

models_fish <- list(
  mod.Q = formula(lquan ~ mon + tue + wed + thu + stormy),
  mod.P = formula(lprice ~ mon + tue + wed + thu + stormy)
)
Q.fish <- lm(models_fish$mod.Q, data = fultonfish)
P.rish <- lm(models_fish$mod.P, data = fultonfish)

## IV-2SLS regression results====

fish.D <- formula(lquan ~ lprice + mon + tue + wed + thu)
fish.S <- formula(lprice ~ lquan + stormy)
fish.eqs <- list(fish.D, fish.S)
fish.ivs <- ~ mon + tue + wed + thu + stormy
fish.sys <- systemfit(fish.eqs,
  method = "2SLS",
  inst = fish.ivs, data = fultonfish
)
fish.smry <- summary(fish.sys)

## Tidy `systemfit` fit results into tibble
rnames <- rownames(fish.smry$coefficients)
cnames <- colnames(fish.smry$coefficients)
fish.smry.tidy <- as_tibble(fish.smry$coefficients)
names(fish.smry.tidy) <- cnames
fish.smry.tidy$vars <- rnames
fish.smry.tidy <- fish.smry.tidy %>%
  separate(vars, into = c("eq", "vars"), sep = "_") %>%
  select(eq, vars, everything())

## Comparison: Reduced equation and the biased OLS estimation ====
# demand equation
## fish.D <- formula(lquan ~ lprice + mon + tue + wed + thu)
ols_fish_D <- lm(fish.D, data = fultonfish)

# supply equation
## fish.S <- formula(lprice ~ lquan + stormy)
ols_fish_S <- lm(fish.S, data = fultonfish)

# 20.7 Exercise and computation====

## Labor market of married Working Women
## load the code of the labor market of married Working Women
# file_path <- "D://github/course-em-eng/02-homework/sem-wage-mroz/code-sem-mroz.R"
# source(file_path)
