
## ==== 19.3 Endogeneity test (Test of Simultaneity)====

### ==== Example: First OLS estimation on price equation====

load(here::here("data/truffles.rda"))
names(truffles)  %<>% toupper()

Hausman_models <-list(mod.P = P~PS+DI+PF,
                      mod.Q.Hausman = Q~hat.Pi+hat.vi,
                      mod.Q.Pindyck = Q~P+hat.vi)

P.red <- lm(Hausman_models$mod.P, data=truffles)

truffles_Hausman <- truffles %>%
  mutate(hat.Pi=round(P.red$fitted.values, digits = 4),
         hat.vi=round(P.red$residuals, digits = 4))

### ==== Example: Second OLS estimation on Hausman equation====

out_Hausman<-lm(Hausman_models$mod.Q.Hausman,
                data =truffles_Hausman)
summary(out_Hausman)

### ==== Example: Second OLS estimation on Pindyck equation====

out_Pindyck <- lm(Hausman_models$mod.Q.Pindyck,
                data =truffles_Hausman)
summary(out_Pindyck)

## check
require(systemfit)
eq1 <- Q ~ P + PS + DI
eq2 <- Q ~ P + PF
instr <- ~ PS + DI + PF

iv_fit <- systemfit::systemfit(
  formula = list(eq1, eq2), inst = instr,
  method =  "2SLS", data = truffles
  )

#hausman.systemfit()
