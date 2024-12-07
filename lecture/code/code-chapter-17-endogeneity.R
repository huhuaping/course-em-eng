# wage case====

## R packages====
library(here)
library(magrittr)
library(tidyverse)
library(xmerit)
library(scales)

## dataset:  raw `mroz`====
require("wooldridge")
mroz_raw <- wooldridge::mroz %>%
  as_tibble() %>%
  select(wage, educ,exper, 
         fatheduc,motheduc,everything()) %>%
  filter(!is.na(wage))

# write.xlsx(mroz,"data/Table-11-mroz-filter.xlsx")
n <- nrow(mroz_raw)

vars_label<- read.delim(
  here::here("data","mroz-var-label.txt"), 
  header = T,sep=":"
  ) %>%
  as_tibble()

vars <- names(mroz_raw) 

tab_vars <- vars_label 

# write.xlsx(mroz,"data/Table-11-mroz-filter.xlsx")
n <- nrow(mroz_raw)


## dataset:  reduced `mroz`====
mroz <- mroz_raw %>%
  select(lwage, educ, exper, expersq, fatheduc, motheduc) %>%
  add_column(id=1:n, .before = "lwage")

## scatter plot====
plot_mroz_scatter <- mroz %>%
  ggplot(aes(educ, lwage))+
  geom_point(size=3) +
  labs(x= "educ", y="log(wage)") +
  theme(text = element_text(size=16))

# 17.2 error model OLS regression====
mod.ols <- formula("lwage ~ educ")
ls.lm <- lm(formula = mod.ols, data = mroz)

mod_origin <- formula(lwage ~ educ +exper+expersq)
ols_origin <- lm(formula = mod_origin, data = mroz)

# 17.3 IV method====

## IV method by hand====
# mod.tsls1 <-  formula("educ ~ fatheduc")
# tsls1 <- lm(formula = mod.tsls1, data = mroz)
# mroz_tsls <- mroz %>%
#   select(lwage, educ, fatheduc) %>%
#   add_column(educ.hat =tsls1$fitted.values)
# 
# mod.tsls2 <-  formula("lwage ~ educ.hat")
# tsls2 <- lm(formula = mod.tsls2, data = mroz_tsls)

## IV method by using `ivreg`====
# require("AER")
# lm.tsls <- ivreg(formu=lwage~educ|fatheduc,data=mroz)

# 17.4 Two stage least squares====

## IV `motheduc`: by hand====
mod_step1 <- formula(educ~exper + expersq + motheduc) 
ols_step1 <- lm(formula = mod_step1, data = mroz)
mroz_add <- mroz %>% mutate(educHat = fitted(ols_step1))

mod_step2 <- formula(lwage~educHat + exper + expersq)
ols_step2 <- lm(formula = mod_step2, data = mroz_add)

## IV `motheduc`: by `systemfit`==== 
require(systemfit)
# set two models
eq_1 <- educ ~  exper + expersq + motheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~  exper + expersq + motheduc
# fit models
fit.sys <- systemfit(
  sys, inst=instr,
  method="2SLS", data = mroz)
# summary of model fit
smry.system_m <- summary(fit.sys)


### prepare for tibble show----
rnames<-rownames(smry.system_m$coefficients)
cnames<-colnames(smry.system_m$coefficients)
tab_smry_m<-as_tibble(smry.system_m$coefficients)
names(tab_smry_m)<-cnames
tab_smry_m$vars<-rnames
tab_smry_m <- tab_smry_m %>%
  separate(vars,into=c("eq","vars"),sep="_")%>%
  select(eq,vars,everything())

## IV `motheduc`: by `ivreg`====
require(AER)
# specify model
mod_iv_m <- formula(lwage ~ educ + exper + expersq
                    | motheduc + exper + expersq)
# fit model
lm_iv_m <- ivreg(formula = mod_iv_m, data = mroz)
# summary of model fit
smry.ivm <- summary(lm_iv_m)

## IV `fatheduc`: by `systemfit`====
# set two models
eq_1 <- educ ~  exper + expersq + fatheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + fatheduc
# fit models
fit.sys <- systemfit(
  sys, inst=instr,
  method="2SLS", data = mroz)
# summary of model fit
smry.system_f <- summary(fit.sys)

### prepare for tibble show----
rnames<-rownames(smry.system_f$coefficients)
cnames<-colnames(smry.system_f$coefficients)
tab_smry_f<-as_tibble(smry.system_f$coefficients)
names(tab_smry_f)<-cnames
tab_smry_f$vars<-rnames
tab_smry_f <- tab_smry_f %>%
  separate(vars,into=c("eq","vars"),sep="_")%>%
  select(eq,vars,everything())

## IV `fatheduc`: by `ivreg`====
# specify model
mod_iv_f <- formula(lwage ~ educ + exper + expersq
                    | fatheduc + exper + expersq)
# fit model
lm_iv_f <- ivreg(formula = mod_iv_f, data = mroz)
# summary of model fit
smry.ivf <- summary(lm_iv_f)


## IV `motheduc` & `fatheduc`: by `systemfit`====
# set two models
eq_1 <- educ ~ exper + expersq + motheduc + fatheduc
eq_2 <- lwage ~ educ + exper + expersq
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + motheduc + fatheduc
# fit models
fit.sys <- systemfit(
  sys, inst=instr,
  method="2SLS", data = mroz)
# summary of model fit
smry.system_mf <- summary(fit.sys)

### prepare for tibble show----
rnames<-rownames(smry.system_mf$coefficients)
cnames<-colnames(smry.system_mf$coefficients)
tab_smry_mf<-as_tibble(smry.system_mf$coefficients)
names(tab_smry_mf)<-cnames
tab_smry_mf$vars<-rnames
tab_smry_mf <- tab_smry_mf %>%
  separate(vars,into=c("eq","vars"),sep="_")%>%
  select(eq,vars,everything())

## IV `motheduc` & `fatheduc`: by `ivreg`====
# specify model
mod_iv_mf <- formula(lwage ~ educ + exper + expersq
                     | motheduc + fatheduc + exper + expersq)
# fit model
lm_iv_mf <- ivreg(formula = mod_iv_mf, data = mroz)
# summary of model fit
smry.ivmf <- summary(lm_iv_mf)

## models comparison====
## see: Stargazer: Save to file, don't show in console 
## [url](https://stackoverflow.com/questions/30195718/stargazer-save-to-file-dont-show-in-console)
#install.packages("stargazer")
file_out <- here("lecture/code/chpt17-models-compare.html")
library("stargazer")
out.stgz <- stargazer(
  ols_origin, ols_step2, lm_iv_m, lm_iv_f, lm_iv_mf,
  #title="lwage equation: OLS, 2SLS, and IV models compared",
  header=FALSE,
  #type="text",  # 保存下来并且不在console中输出
  type="html", # "html" or "latex" (in index.Rmd) 
  #keep.stat="n",  # what statistics to print
  omit.table.layout="n",
  #star.cutoffs=NA,
  digits=4, 
  single.row=FALSE,
  font.size = "tiny",
  intercept.bottom=FALSE, #moves the intercept coef to top
  column.labels=c("OLS","explicit 2SLS", "IV mothereduc", "IV fathereduc",
                  "IV mothereduc and fathereduc"),
  dep.var.labels.include = FALSE,
  model.numbers = TRUE,
  dep.var.caption="Dependent variable: lwage",
  model.names=FALSE,
  notes = c("括号内为标准误差"),
  notes.append = TRUE,
  out = file_out
  #star.char=NULL #supresses the stars
)

# 17.5 test the validity of instrumental variables====

## birth weight example====

### dataset ====
data(bwght,bwght2, package = "wooldridge" )
n_birth <- nrow(bwght)

### TSLS estimation====
mod_base <- formula("lbwght ~packs")
fit_base <- lm(mod_base, data = bwght)

mod_proxy <-  formula("packs ~cigprice")
fit_proxy <- lm(mod_proxy, data = bwght)
smry_proxy <- summary(fit_proxy)

packs_hat <- data.frame(
  packs_hat = fitted(fit_proxy)
)
bwght_new <- cbind(bwght, packs_hat)
mod_s2 <- formula("lbwght~packs_hat")

fit_s2 <- lm(mod_s2, data = bwght_new)
#summary(fit_s2)

## wage example====

### IV relevance test: constrained F-test====
library("car")
mod_relevance1 <- formula(educ ~ exper +expersq + motheduc)
ols_relevance1 <- lm(formula = mod_relevance1, data = mroz)
ftest_constrain_m <- linearHypothesis(
  ols_relevance1, 
  c("motheduc=0")
  )
# obtain F statistics
F_r1 <- ftest_constrain_m$`F`[[2]]

mod_relevance2 <- formula(educ ~ exper +expersq  + fatheduc)
ols_relevance2 <- lm(formula = mod_relevance2, data = mroz)
ftest_constrain_f <- linearHypothesis(
  ols_relevance2, c("fatheduc=0")
  )

mod_relevance3 <- formula(educ ~ exper +expersq + motheduc + fatheduc)
ols_relevance3 <- lm(formula = mod_relevance3, data = mroz)
ftest_constrain_mf <- linearHypothesis(
  ols_relevance3, 
  c("motheduc=0", "fatheduc=0")
)


### compare the classical F-test results====
smry_ols1 <- summary(ols_relevance1)
F_c1 <- smry_ols1$fstatistic[[1]]

### weak instrument test(Cragg-Donald test)====
mroz1 <- wooldridge::mroz %>%
  filter(wage > 0, inlf == 1)
# set parameters
N <- nrow(mroz1)
G <- 2; B <- 2; L <- 2 
x1 <- resid(lm(mtr ~ kidslt6 + nwifeinc, data = mroz1))
x2 <- resid(lm(educ ~ kidslt6 + nwifeinc, data = mroz1))
z1 <-resid(lm(motheduc ~ kidslt6 + nwifeinc, data = mroz1))
z2 <-resid(lm(fatheduc ~ kidslt6 + nwifeinc, data = mroz1))
X <- cbind(x1,x2)
Y <- cbind(z1,z2)
rB <- min(cancor(X, Y)$cor)
CraggDonaldF <- ((N-G-L)/L)/((1-rB^2)/rB^2)

out_cragg <- tribble(
  ~"G", ~"L", ~"B", ~"N", ~"rb", ~"CraggDonaldF",
  G, L, B, N, scales::number(rB, 0.0001), scales::number(CraggDonaldF, 0.0001)
)


### exogenous IV test: J test====
# obtain residual of IV regression
mroz_resid <- mroz %>%
  mutate(resid_iv_mf = residuals(lm_iv_mf)) 
# run auxiliary regression
mod_jtest <- formula(resid_iv_mf ~ exper +expersq +motheduc +fatheduc)
lm_jtest <- lm(formula = mod_jtest, data = mroz_resid)

# restricted F-test
restricted_ftest <- linearHypothesis(
  lm_jtest, c("motheduc = 0", "fatheduc = 0"), 
  test = "F"
  )
F_star <- restricted_ftest[2, 5]

# calculate J statistics by hand
# numbers of instruments
m <- 2
# calculate J statistics
J_calc <- m*F_star

# chi-square test directly
jtest <- linearHypothesis(
  lm_jtest, c("motheduc = 0", "fatheduc = 0"), 
  test = "Chisq"
  )
# extract J statistic (chi-square value)
J_star <- jtest[2, 5]

# calculate J statistics (adjusted the degrees of freedom)
f <- m -1  # correct freedoms
# compute correct p-value for J-statistic
pchi <- pchisq(J_star, df = f, lower.tail = FALSE)


# 17.6 Testing Regressor endogeneity====

## Hausman test----
### ==== solution 1 for Hausman test (diagonose) ====
# show model as before
#mod_iv_mf

# IV test for 'ivreg' object
#summary(lm_iv_mf, diagnostics = TRUE)

### ==== solution 2 for Hausman test (calculate) ====
## guide with Hansen's chpt 12.29 Endogeneity test
## reduced function for endogenous education
red_mf <- formula(educ ~ exper + expersq + motheduc + fatheduc)
fit_red_mf<- lm(formula = red_mf, data = mroz)
## extract residual u2 and combined new dataset
resid_mf <- data.frame(resid_mf = resid(fit_red_mf))
tbl_mf <- cbind(mroz, resid_mf)
## control function OLS estimation
control_mf <- formula(lwage ~ educ +exper + expersq  + resid_mf)
fit_control_mf <- lm(formula = control_mf, data = tbl_mf)
smry_control_mf <- summary(fit_control_mf)

## extract t statistics of alpha
t_star_resid <- pull(
  as_tibble(t(smry_control_mf$coefficients[,"t value"])),
  "resid_mf")
## calculate equivalent F statistics
restricted_F_mf <- linearHypothesis(model = fit_control_mf, "resid_mf=0")
F_star_resid <- restricted_F_mf$F[2]
p_F_resid <- restricted_F_mf$`Pr(>F)`[2]

