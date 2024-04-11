
## ====18.3 Is the OLS Method Still applicable ?====

### ==== Simulation: artificially population ====

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

### ==== Simulation: scatter plots====

p_scatter <- ggplot(monte,aes(x=Y,y=C))+
  geom_point(color="red") +
  geom_smooth(se=FALSE,method = "lm",alpha=0.6,color="grey")+
  labs(x="Y",y="C"
       #,subtitle = "scatter"
  ) 

### ==== Simulation: regression report====


mod_monte <-list(mod.C = C~Y)
#fun_report_eq(mod_monte$mod.C,lm.dt = monte)
out_monte <-lm(mod_monte$mod.C,data = monte)
summary(out_monte)

### ==== Simulation: sample regression line (SRL)====



p_srl <- ggplot(monte,aes(x=Y,y=C))+
  geom_point(color="red") +
  geom_smooth(se=FALSE,method = "lm",alpha=0.6,color="grey")+
  geom_text(aes(x=28, y=25, label= "slope:0.8207") ) +
  labs(x="Y",y="C") 
