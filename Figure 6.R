# Relative humidity 
plot_RH_aerosol = ggplot(data_figre_6a, aes(x = R_Average, y = Ct))+
  geom_point(size = 2, color = "skyblue4")+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average relative humidity (%)", y = expression(paste("Aerosol C"["T"])), tag = "a")+
  stat_fit_glance(method = 'lm', label.x =  30,
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  expand_limits(y = 20)


# RH box plot 
Plot_RH_box = ggplot(data_humidity_scatter, aes(x = Trial, y = Ct))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 2, aes(color = Trial))+
  scale_color_manual(values = c("goldenrod4", "deepskyblue"))+
  labs(x= "Trial", y = expression(paste("Aerosol C"["T"])), tag  = "b")+
  geom_signif(comparisons = list(c("Dehumidification","Humidification")), annotations = str_c("n = ", "58" , " ,", " ", "P = ", formatC(ttesthumidity$p.value, digits = 2)), digits = 2, map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic')+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")
ttesthumidity = t.test(x = data_figre_6b_ttest$Ct_dehumidification, y = data_figre_6b_ttest$Ct_humidification, alternative = "less", paired = TRUE)



plot_RH_surface = ggplot(data_figre_6c, aes(x = R_Average, y = Ct))+
  geom_point(size = 2, color = "firebrick2")+
  geom_smooth(method = "lm", color = "black")+
  labs(x= "Average relative humidity (%)", y = expression(paste("Surface C"["T"])), tag = "c")+
  stat_fit_glance(method = 'lm', label.x =  30,
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")),
                  size = 4, fontface = "italic")+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  expand_limits(y = 20)



# Surface boxplot 
ttestsurface = t.test(x = data_surface_humidity_comparison$Ct_Dehumidification[data_surface_humidity_comparison$location %in% "Computer"], y = data_surface_humidity_comparison$Ct_humidification[data_surface_humidity_comparison$location %in% "Computer"], paired = TRUE, alternative = "greater")

plot_surface_box=  ggplot(subset(data_surface_humidity_comparison_scatter, data_surface_humidity_comparison_scatter$Location %in% "Computer") , aes(x = Trial, y = Ct))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 2, aes(color = Trial))+
  scale_color_manual(values = c("goldenrod4", "deepskyblue"))+
  labs(x= "Trial", y = expression(paste("Surface C"["T"], " (computer)")), tag  = "d")+
  geom_signif(comparisons = list(c("Dehumidification","Humidification")), annotations = str_c("n = ", "86" , " ,", " ", "P = ", formatC(ttestsurface$p.value, digits = 2)), digits = 2, map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic')+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")


ggarrange(plot_RH_aerosol, Plot_RH_box ,plot_RH_surface, plot_surface_box)+
  ggsave(filename = "Figure 6.tif", width = 13.194/1.4, height = 13.194/1.4, units = "in" ,device = "tiff", path ="/Users/hoomanp/Desktop/RDM/TIFF" ,dpi = 600, limitsize = FALSE)

