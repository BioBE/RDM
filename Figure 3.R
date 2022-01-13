# Combine_scatter 
surface_scatter =  ggplot(data_figre_3a , aes(x = Ct_nose, y = Ct_surface, fill = Location)) +
  geom_point(aes(color = Location), alpha = 0.5, size = 3, shape = 21)+
  geom_smooth(method = "lm", aes(color = Location), se = FALSE)+
  scale_fill_manual(values = c("orange2","violetred4","seagreen"))+
  scale_color_manual(values = c("orange2","violetred4","seagreen"))+
  labs(x= expression(paste("C"["T"], " value of paired nasal samples")), y = expression(paste("C"["T"], " value of high touched surface samples")), tag = "a")+
  stat_fit_glance(method = 'lm',
                  label.x = c(21,21.7,21),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), " ","(",c("Chair", "Computer", "Phone"),")", sep = "")),
                  size = 4, fontface = "italic")+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))


Gian_scatter = ggplot(data_figre_3c, aes(x = Nasal, y = Ct, color = Location)) +
  geom_point(size = 3, aes(color = Location, shape = Type), alpha = 0.3)+
  scale_color_manual(values = c("orange2","violetred4","skyblue4","blue","coral3", "black","seagreen"))+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x= expression(paste("C"["T"], " value of paired nasal samples")), y = expression(paste("C"["T"])), color = "Type", tag = "c")+
  theme_classic()+
  expand_limits(y = 20)+
  theme(plot.tag = element_text(face = "bold", size = 20))



# Figure #3 1600 * 1400
ggarrange(ggarrange(surface_scatter, Plot_plates_scatter), Gian_scatter, nrow = 2, ncol = 1)+
  ggsave(filename = "Figure 3.tif", width = 16.667/1.4, height = 13.194/1.4, units = "in" ,device = "tiff", path ="/Users/hoomanp/Desktop/RDM/TIFF" ,dpi = 600, limitsize = FALSE)


#Plates 

Plot_plates_scatter = ggplot(data_figre_3b , aes(x = Ct_nose, y = Ct_plates, fill = Location)) +
  geom_point(aes(color = Location))+
  geom_smooth(method = "lm", aes(color = Location), se = FALSE)+
  scale_fill_manual(values = c("coral3", "skyblue4"))+
  scale_color_manual(values = c("coral3", "skyblue4"))+
  labs(x= expression(paste("C"["T"], " value of paired nasal samples")), y = expression(paste("C"["T"], " value of settling plates")), tag = "b")+
  stat_fit_glance(method = 'lm',
                  label.y = c(38,39),
                  label.x = 18.4,
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), " ","(", c("Near field","Far field"),")",  sep = "")),
                  size = 4, fontface = "italic")+
  expand_limits(y = 20)+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))