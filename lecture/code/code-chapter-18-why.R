# R packages====
library(here)
library(magrittr)
library(tidyverse)
library(xmerit)
library(scales)

# 18.1 The Nature of Simultaneous-Equation Models====

## turtle example====

## variables====

truffles_var <- tibble::tribble(
  ~`vars`, ~`label`, ~`measure`,
  "P", "market price of truffles", "dollar/ounce",
  "Q", "market quantity of truffles", "ounce",
  "PS", "market price of substitute", "dollar/ounce",
  "DI", "disposable income", "dollar/capita, monthly",
  "PF", "rental price of truffles-pigs", "dollar/hour"
)

## data set ====

# truffles<-read.table("data/truffles.dat",header = F)
load(here("data/truffles.rda"))
names(truffles) %<>% toupper()

##  scatter plot  (P VS Q)====

p_scatter_pq <- ggplot(truffles, aes(x = Q, y = P)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE) +
  labs(x = "Q", y = "P")

## The Scatter matrix====

# panel.smooth function is built in.
## panel.cor puts correlation in upper panels, size proportional to correlation
## do not use this code
# panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- abs(cor(x, y))
#   txt <- format(c(r, 0.123456789), digits = digits)[1]
#   txt <- paste(prefix, txt, sep = "")
#   if (missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
#   text(0.5, 0.5, txt, cex = cex.cor * r)
# }

# Plot 2: same as above, but add loess smoother in lower and correlation in upper
## do not disply chart immediatly but save the object
## do not use this code
# p_scatter_matrix <- pairs(~ P + Q + DI + PS + PF,
#   data = truffles,
#   lower.panel = panel.smooth, upper.panel = panel.cor,
#   pch = 20, main = "Truffles Scatter Matrix"
# )

library(GGally)
p_scatter_matrix <- ggpairs(truffles, 
  columns = c("P", "Q", "DI", "PS", "PF"),
  title = "Truffles Scatter Matrix",
  lower = list(continuous = "smooth"),
  upper = list(continuous = "cor")
)

## simple-model ====

mod_simple <- list(
  mod.Q = formula(Q ~ P),
  mod.P = formula(P ~ Q)
)

## not run
# qx.out <- xmerit::qx.est(
#   lm.mod = mod_simple$mod.P,
#   lm.dt = truffles,
#   inf = c("fit", "Ftest")
# )

## The sample regression line (SRL) Q to P ====

p_srl_q2p <- ggplot(truffles, aes(x = Q, y = P)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE, method = "lm", color = "red") +
  labs(x = "Q", y = "P")

## The sample regression line (SRL) P to Q ====

p_srl_p2q <- ggplot(truffles, aes(x = P, y = Q)) +
  geom_point(color = "blue") +
  geom_smooth(se = FALSE, method = "lm", color = "red") +
  labs(x = "P", y = "Q")

## The multi-variables regression model ====

mod_added <- list(
  mod.Q = formula(Q ~ P + DI + PS + PF),
  mod.P = formula(P ~ Q + DI + PS + PF)
)


# 18.3 Is the OLS Method Still applicable ?====

## Simulation====

### artificially population ====

monte<- as_tibble(read_table(here("data/table-18.1.txt")))

monte<- monte %>%
  select(-u) %>%
  mutate(u=C-2-0.8*Y)

cov_yu <- monte %>% 
  mutate(yu=(Y-mean(Y))*(u-mean(u))) %>%
  summarise(sum(yu)) %>% 
  unlist() 
ss_y <- monte %>% 
  mutate(Y_sqr=(Y-mean(Y))^2) %>% 
  summarise(sum(Y_sqr)) %>% 
  unlist()

## scatter plots====

p_scatter <- ggplot(monte,aes(x=Y,y=C))+
  geom_point(color="red") +
  geom_smooth(se=FALSE,method = "lm",alpha=0.6,color="grey")+
  labs(x="Y",y="C"
       #,subtitle = "scatter"
  ) 

## regression report====


mod_monte <-list(mod.C = C~Y)
#fun_report_eq(mod_monte$mod.C,lm.dt = monte)
out_monte <-lm(mod_monte$mod.C,data = monte)
summary(out_monte)

## sample regression line (SRL)====



p_srl <- ggplot(monte,aes(x=Y,y=C))+
  geom_point(color="red") +
  geom_smooth(se=FALSE,method = "lm",alpha=0.6,color="grey")+
  geom_text(aes(x=28, y=25, label= "slope:0.8207") ) +
  labs(x="Y",y="C") 
