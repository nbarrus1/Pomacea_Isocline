###lambda vs survival and kgrowth###

threshold %>% 
  filter(k != 0.005) %>% 
  ggplot(aes(x = k, y = log(lambda), col = Sinteract))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  scale_color_continuous(type = "viridis")+
  xlab("Growth rate (k)")+
  ylab("Recruits (ln(lamda))")+
  labs(color = "Total Juvenile Survival")

threshold %>% 
  filter(k != 0.005) %>% 
  ggplot(aes(x = Sinteract, y = log(lambda), col = as.character(k)))+
  geom_point()+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")+
  scale_color_viridis_d()

levels(threshold$FST1)