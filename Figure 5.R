# Ventilation figures 
plot_vent_boxplot = ggplot(data_figre_5a, aes(x = Trial, y = Ct))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 2, aes(color = Trial))+
  scale_color_manual(values = c("springgreen4", "skyblue2"))+
  labs(x= "Condition", y = expression(paste("Aerosol C"["T"])), tag  = "a")+
  geom_signif(comparisons = list(c("Removal mechanism","Control")), annotations = str_c("n = ", "120" , " ,", " ", "P = ", formatC(ttestvent$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic')+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")
ttestvent = t.test(x = data_figre_5a_ttest$Ct_vent[data_figre_5a_ttest$ACH %in% c(3,6,9,12,14,30)], y = data_figre_5a_ttest$Ct_normal[data_figre_5a_ttest$ACH %in% c(3,6,9,12,14,30)], alternative = "greater", paired = TRUE)  


# all ACH
plot_Vent_scatter= ggplot(data_figre_5b, aes(x = CO2, y = Ct))+
  geom_point(color = "springgreen3", size = 3)+
  geom_smooth(method = "lm", color = "black")+
  labs(x= expression(paste("Average CO"[2]," ", "concentration (PPM) affected by ventilation")), y = expression(paste("Aerosol C"["T"])), tag = "b")+
  stat_fit_glance(method = 'lm', label.x = 500,
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = "")), size = 4, fontface = "italic")+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

# Filtration 
Filtration_box = ggplot(data_figre_5d, aes(x = Trial, y = Ct))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 2, aes(color = Trial))+
  scale_color_manual(values = c("skyblue2", "violetred3"))+
  labs(x= "Trial", y = expression(paste("Aerosol C"["T"])), tag  = "d")+
  geom_signif(comparisons = list(c("Control","Filtration")), annotations = str_c("n = ", "36" , " ,", " ", "P = ", formatC(ttestHEPA$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic')+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

ttestHEPA =t.test(x = data_figre_5d_ttest$Ct_normal, y = data_figre_5d_ttest$Ct_hepa, paired = TRUE, alternative = "less")


# ACH 3,6 Vs 9,12 
ttest_v6_original = t.test(x = data_figre_5c$Ct[data_figre_5c$ACH %in% c(0,1,3,4.5)], y = data_figre_5c$Ct[data_figre_5c$ACH %in% c(9,12)], alternative = "less")
ttest_v6_near = t.test(x = data_figre_5c$Ct[data_figre_5c$ACH %in% c(0,1,3,4.5) & data_figre_5c$Location %in% "TF-Desk"], y = data_figre_5c$Ct[data_figre_5c$ACH %in% c(9,12) & data_figre_5c$Location %in% "TF-Desk"], alternative = "less")
ttest_v6_far = t.test(x = data_figre_5c$Ct[data_figre_5c$ACH %in% c(0,1,3,4.5) & data_figre_5c$Location %in% "TF-door"], y = data_figre_5c$Ct[data_figre_5c$ACH %in% c(9,12) & data_figre_5c$Location %in% "TF-door"], alternative = "less")

aLLo = data.frame( label = c(str_c("n = ", "35" , " ,", " ", "P = ", formatC(ttest_v6_far$p.value, digits = 2)), str_c("n = ", "32" , " ,", " ", "P = ", formatC(ttest_v6_near$p.value, digits = 2))),
                   Loc = c("Far field", "Near field"),
                   x = c(1.5,1.5),
                   y = c(42.7, 42.7))


plot_vent_6ach_boxplot = ggplot(subset(data_figre_5c, data_figre_5c$ACH %in% c(0,1,3,4.5,9,12)), aes(x = factor(Trial_6, levels = c("Under ~4.5 ACH","Above ~9 ACH")), y = Ct))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~Loc)+
  geom_jitter(width = .2, height = .2, size = 2, aes(color = Trial_6))+
  scale_color_manual(values = c("springgreen4", "skyblue2"))+
  labs(x= "Trial", y = expression(paste("Aerosol C"["T"])), tag  = "c", color = "Trial")+
  geom_signif(comparisons = list(c("Under ~4.5 ACH", "Above ~9 ACH")), annotations = " ", map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 3, fontface = 'italic' ,y_position = 39.6)+
  geom_text(  size = 4, fontface = 'italic',
              data    = aLLo, 
              mapping = aes(x = 1.5, y = 41.1, label = label),
  )+    theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")


ggarrange(plot_vent_boxplot, plot_Vent_scatter,plot_vent_6ach_boxplot, Filtration_box, nrow = 2, ncol = 2)+
  ggsave(filename = "Figure 5.tif", width = 13.194/1.4, height = 13.194/1.4, units = "in" ,device = "tiff", path ="/Users/hoomanp/Desktop/RDM/TIFF" ,dpi = 600, limitsize = FALSE)
