# Fig 3

# a-0.3
Plot_particle_0.3 = ggplot(data_figre_4, aes(x = Ch_0.3_Mean.x, y = Ct))+
  geom_point(color = "red4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 0.3 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "a")+
  stat_fit_glance(method = 'lm',
                  geom = 'text', label.x = 10200,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# b - 1
Plot_particle_1 = ggplot(data_figre_4, aes(x = data_figre_4$Ch_1_Mean, y = Ct))+
  geom_point(color = "deepskyblue4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 1 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "b")+
  stat_fit_glance(method = 'lm',
                  geom = 'text', label.x = 1000,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# c - 2.5
Plot_particle_2.5 = ggplot(data_figre_4, aes(x = data_figre_4$Ch_2.5_Mean, y = Ct))+
  geom_point(color = "seagreen4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 2.5 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "c")+
  stat_fit_glance(method = 'lm',
                  geom = 'text',label.x = 100,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# d - 3
Plot_particle_3 = ggplot(data_figre_4, aes(x = data_figre_4$Ch_3_Mean, y = Ct))+
  geom_point(color = "deeppink4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 3 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "d")+
  stat_fit_glance(method = 'lm',
                  geom = 'text', label.x = 125,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# e - 5
Plot_particle_5 = ggplot(data_figre_4, aes(x = data_figre_4$Ch_5_Mean, y = Ct))+
  geom_point(color = "chocolate4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 5 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "e")+
  stat_fit_glance(method = 'lm',
                  geom = 'text', label.x = 100,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# f - 10
Plot_particle_10 = ggplot(data_figre_4, aes(x = data_figre_4$Ch_10_Mean, y = Ct))+
  geom_point(color = "darkorchid4", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average 10 micron particles (#)", y = expression(paste("Aerosol C"["T"])), tag = "f")+
  stat_fit_glance(method = 'lm',
                  geom = 'text', label.x = 15,
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  stat_poly_eq(formula = x~y, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, rr.digits = 2, label.y = 0.88, label.x.npc = 0.15) + 
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# combine 
ggarrange(Plot_particle_0.3, Plot_particle_1, Plot_particle_2.5, Plot_particle_3, Plot_particle_5, Plot_particle_10, nrow = 3, ncol = 2, widths = 12.5, heights = 25)+
  ggsave(filename = "Figure 4.tif", width = 13.889/1.4, height = 18.056/1.4, units = "in" ,device = "tiff", path ="/Users/hoomanp/Desktop/RDM/TIFF" ,dpi = 600, limitsize = FALSE)
