#install.packages("Hmisc")

# curve1 and curve2 should be data.frames with an x and y column
# For instance, as_data_frame(Hmisc::bezier(c(1, 8, 9), c(1, 5, 9)))
curve_intersect <- function(curve1, curve2) {
  # Approximate the functional form of both curves
  curve1_f <- approxfun(curve1$x, curve1$y, rule = 2)
  curve2_f <- approxfun(curve2$x, curve2$y, rule = 2)
  
  # Calculate the intersection of curve 1 and curve 2 along the x-axis
  point_x <- uniroot(function(x) curve1_f(x) - curve2_f(x), 
                     c(min(curve1$x), max(curve1$x)))$root
  
  # Find where point_x is in curve 2
  point_y <- curve2_f(point_x)
  
  # All done!
  return(list(x = point_x, y = point_y))
}

# plot1-scatter --------
supply1 <- Hmisc::bezier(x = c(2,6),
                         y = c(6,10)) %>% as_data_frame()
demand1 <- Hmisc::bezier(x = c(6,2),
                         y = c(6,10)) %>% as_data_frame()
supply2 <- Hmisc::bezier(x = c(4,9),
                         y = c(15,17)) %>% as_data_frame()
demand2 <- Hmisc::bezier(x = c(9,4),
                         y = c(15,17)) %>% as_data_frame()
supply3 <- Hmisc::bezier(x = c(8,10),
                         y = c(4,9)) %>% as_data_frame()
demand3 <- Hmisc::bezier(x = c(10,8),
                         y = c(4,9)) %>% as_data_frame()
supply4 <- Hmisc::bezier(x = c(12,16),
                         y = c(11,14)) %>% as_data_frame()
demand4 <- Hmisc::bezier(x = c(16,12),
                         y = c(11,14)) %>% as_data_frame()

intersection_xy1<- curve_intersect(supply1, demand1) %>% as_data_frame()
intersection_xy2<- curve_intersect(supply2, demand2) %>% as_data_frame()  
intersection_xy3<- curve_intersect(supply3, demand3) %>% as_data_frame()
intersection_xy4<- curve_intersect(supply4, demand4) %>% as_data_frame()

plot1<- ggplot(mapping=aes(x = x, y = y)) +
  geom_point(data = intersection_xy1, size = 2) +
  geom_point(data = intersection_xy2, size = 2) +
  geom_point(data = intersection_xy3, size = 2) +
  geom_point(data = intersection_xy4, size = 2) +
  xlim(0, 20)+
  ylim(0, 20)+
  labs(x="Q",y="P")+
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))

# plot2-intersection -------

labels2 <- data_frame(label = c(rep("S",4), rep("D",4)),
                      x = c(6, 9, 10,16,2,4,8,12)+0.5,
                      y = c(10,17,9,14,10,17,9,14)+0.5)
plot2<- ggplot(mapping=aes(x = x, y = y)) +
  geom_path(data=supply1, color = "#0073D9", size = 1) + 
  geom_path(data = demand1, color = "#FF4036", size = 1) + 
  geom_path(data=supply2, color = "#0073D9", size = 1) + 
  geom_path(data = demand2, color = "#FF4036", size = 1) +
  geom_path(data=supply3, color = "#0073D9", size = 1) + 
  geom_path(data = demand3, color = "#FF4036", size = 1) + 
  geom_path(data=supply4, color = "#0073D9", size = 1) + 
  geom_path(data = demand4, color = "#FF4036", size = 1)+
  geom_point(data = intersection_xy1, size = 2) +
  geom_point(data = intersection_xy2, size = 2) +
  geom_point(data = intersection_xy3, size = 2) +
  geom_point(data = intersection_xy4, size = 2) +
  geom_text(data = labels2,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro") +
  xlim(0, 20)+
  ylim(0, 20)+
  labs(x="Q",y="P")+
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))

# plot3-both ------

data_line<-function(know,inter,pre_x){
  lm_mod<-lm(data=data.frame(x=c(know[1],inter[1]),y=c(know[2],inter[2])),y~x)
  pre_y<-predict.lm(lm_mod,newdata=data.frame(x=pre_x))
  df <- Hmisc::bezier(x = c(know[1],pre_x),
                      y = c(know[2],pre_y)) %>% as_data_frame()
}

supply_41 <- data_line(know= c(19,19),inter = c(14,12.5),pre_x = 5)
supply_42 <- data_line(know= c(19,17),inter = c(14,12.5),pre_x = 5)
supply_43 <- data_line(know= c(19,15),inter = c(14,12.5),pre_x = 5)
demand_41 <- data_line(know= c(19,2),inter = c(14,12.5),pre_x = 5)
demand_42 <- data_line(know= c(19,4),inter = c(14,12.5),pre_x = 5)
demand_43 <- data_line(know= c(19,6),inter = c(14,12.5),pre_x = 5)


labels4 <- data_frame(label = c(str_c("S[",1:3,"]"),
                                str_c("D[",1:3,"]")),
                      x = rep(19,6)+0.5,
                      y = c(19,17,15,2,4,6))

inter_points4<-curve_intersect(supply_41, supply_42) %>% as_data_frame()

plot3<- ggplot(mapping=aes(x = x, y = y)) +
  geom_path(data = supply_41, color = "#0073D9", size = 1,linetype="dashed")+
  geom_path(data = supply_42, color = "#0073D9", size = 1,linetype="dashed")+
  geom_path(data = supply_43, color = "#0073D9", size = 1,linetype="dashed")+
  geom_path(data = demand_41, color = "#FF4036", size = 1) +
  geom_path(data = demand_42, color = "#FF4036", size = 1) +
  geom_path(data = demand_43, color = "#FF4036", size = 1) +
  #geom_point(data=intersection_xy4,size=2) +
  geom_point(data=inter_points4,size=2) +
  geom_text(data = labels4,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro") +
  xlim(0, 20)+
  ylim(0, 20)+
  labs(x="Q",y="P")+
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))

# plot4-demand -------

data_parallel<-function(know_data_fix,know_data_par,
                        next_x,need_x,pre_x){
  lm_mod_fix<-lm(data=know_data_fix,y~x)
  next_y<-predict.lm(lm_mod_fix,newdata=data.frame(x=next_x))
  next_point<-c(next_x,next_y)
  
  lm_mod_par<-lm(data=know_data_par,y~x)
  slope<-lm_mod_par$coefficients[2]
  need_y<-next_y-slope*(next_x-need_x)
  need_ponit<-c(need_x,need_y)
  
  lm_mod_new<-lm(data=data.frame(x=c(next_point[1],need_ponit[1]),y=c(next_point[2],need_ponit[2])),y~x)
  pre_y<-predict.lm(lm_mod_new,newdata=data.frame(x=pre_x))
  df <- Hmisc::bezier(x = c(need_ponit[1],pre_x),
                      y = c(need_ponit[2],pre_y)) %>% as_data_frame()
}

D2<-data_parallel(know_data_fix = supply_41, 
                  know_data_par = demand_41,
                  next_x=13,need_x = 5, pre_x = 18)
D3<-data_parallel(know_data_fix = supply_41, 
                  know_data_par = demand_41,
                  next_x=12,need_x = 5, pre_x = 18)
D4<-data_parallel(know_data_fix = supply_41, 
                  know_data_par = demand_41,
                  next_x=11,need_x = 2, pre_x = 18)
D5<-data_parallel(know_data_fix = supply_41, 
                  know_data_par = demand_41,
                  next_x=10,need_x = 2, pre_x = 18)


inter_D2<-curve_intersect(supply_41, D2) %>% as_data_frame()
inter_D3<-curve_intersect(supply_41, D3) %>% as_data_frame()
inter_D4<-curve_intersect(supply_41, D4) %>% as_data_frame()
inter_D5<-curve_intersect(supply_41, D5) %>% as_data_frame()

x_pos<-c(19,
         filter(demand_41,near(y,19,tol = .1))[[1]],
         filter(D2,near(y,19,tol = .15))[[1]],
         filter(D3,near(y,19,tol = .1))[[1]],
         filter(D4,near(y,19,tol = .1))[[1]],
         filter(D5,near(y,19,tol = .1))[[1]])
labels_d <- data_frame(label = c(str_c("S[",1,"]"),
                                 str_c("D[",1:5,"]")),
                       x = x_pos+0.6,
                       y = c(19,rep(19,5)))

plot4<-ggplot(mapping=aes(x = x, y = y)) +
  xlim(0, 20)+
  ylim(0, 20)+
  labs(x="Q",y="P")+
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))+
  geom_path(data = supply_41, color = "#0073D9", size = 1)+
  geom_path(data = demand_41, color = "#FF4036", size = 1,linetype="dashed") +
  geom_path(data = D2, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = D3, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = D4, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = D5, color = "blue", size = 1,linetype="dashed")  +
  geom_point(data=inter_points4,size=2) +
  geom_point(data=inter_D2,size=2) +
  geom_point(data=inter_D3,size=2) +
  geom_point(data=inter_D4,size=2) +
  geom_point(data=inter_D5,size=2) +
  geom_text(data = labels_d,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro") 

# plot5-supply -------
S2<-data_parallel(know_data_fix = demand_41, 
                  know_data_par = supply_41,
                  next_x=15,need_x = 10, pre_x = 20)
S3<-data_parallel(know_data_fix = demand_41, 
                  know_data_par = supply_41,
                  next_x=16,need_x = 13, pre_x = 20)
S4<-data_parallel(know_data_fix = demand_41, 
                  know_data_par = supply_41,
                  next_x=12,need_x = 3, pre_x = 18)
S5<-data_parallel(know_data_fix = demand_41, 
                  know_data_par = supply_41,
                  next_x=13,need_x = 5, pre_x = 18)


inter_S2<-curve_intersect(demand_41, S2) %>% as_data_frame()
inter_S3<-curve_intersect(demand_41, S3) %>% as_data_frame()
inter_S4<-curve_intersect(demand_41, S4) %>% as_data_frame()
inter_S5<-curve_intersect(demand_41, S5) %>% as_data_frame()

x_pos<-c(19,
         filter(supply_41,near(y,5,tol = .1))[[1]],
         filter(S2,near(y,5,tol = .05))[[1]],
         filter(S3,near(y,5,tol = .045))[[1]],
         filter(S4,near(y,5,tol = .1))[[1]],
         filter(S5,near(y,5,tol = .1))[[1]])
labels_e <- data_frame(label = c(str_c("D[",1,"]"),
                                 str_c("S[",1:5,"]")),
                       x = x_pos+0.6,
                       y = c(2,rep(5,5)))

plot5<-ggplot(mapping=aes(x = x, y = y)) +
  xlim(0, 20)+
  ylim(0, 20)+
  labs(x="Q",y="P")+
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = 0.5))+
  geom_path(data = supply_41, color = "#0073D9", size = 1,linetype="dashed")+
  geom_path(data = demand_41, color = "#FF4036", size = 1) +
  geom_path(data = S2, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = S3, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = S4, color = "blue", size = 1,linetype="dashed") +
  geom_path(data = S5, color = "blue", size = 1,linetype="dashed")  +
  geom_point(data=inter_points4,size=2) +
  geom_point(data=inter_S2,size=2) +
  geom_point(data=inter_S3,size=2) +
  geom_point(data=inter_S4,size=2) +
  geom_point(data=inter_S5,size=2) +
  geom_text(data = labels_e,
            aes(x = x, y = y, label = label), parse = TRUE,
            family = "Source Sans Pro")