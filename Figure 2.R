# Figure 1a
Plot_Close_field_scatter = ggplot(Data_figure_1a, aes(x = Ct_nose, y = Ct_air, shape = Location)) +
  geom_point(size = 3, aes(color = factor(sub, levels = c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11")), shape = Location))+
  geom_smooth(method = "lm", colour = "black", aes(linetype = Location))+
  labs(x= expression(paste("C"["T"]," value of paired nasal samples")), y = expression(paste("Aerosol C"["T"])), tag = "a", linetype = "Trend", color = "Subject")+
  scale_linetype_manual(values = c("dotdash", "solid"))+
  stat_fit_glance(method = 'lm', label.x = 22.2, 
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), " ","(",c("Far field", "Near field"),")", sep = ""), fontface = "italic"), size = 4)+
  theme_classic()+
  expand_limits(y = 20)+
  theme(plot.tag = element_text(face = "bold", size = 20), legend.key.size = unit(0.45, "cm"))

#Figure 1b
Plot_Close_Far_Box = ggplot(Data_figure_1b, aes(x = Location, y = Ct, color = Location))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 0.8)+
  scale_color_manual(values=c("coral3", "skyblue4"))+
  labs(x= "Trial", y = expression(paste("Aerosol C"["T"])), tag = "b")+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "80" , " ,", " ", "P = ", formatC(ttest_cf$p.value, digits = 4)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black")+
  theme_classic()+
  expand_limits(y = 20)+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")
# T.test near field far field 
ttest_cf = t.test(x = Data_figure_1b_ttest$Ct_near, y = Data_figure_1b_ttest$Ct_far, alternative = "less", paired = TRUE)


# Figure 1c
Plot_CO2_nearfar = ggplot(Data_figure_1c , aes(x = Location, y = co, color = Location))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, height = .2, size = 0.8)+
  scale_color_manual(values=c("forestgreen", "goldenrod4"))+
  labs(x= "Trial", y = expression(paste("Average CO"[2]," ", "concentration (PPM)")), tag = "c")+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "54" , " ,", " ", "P = ", formatC(t.test_CO2_nearfar$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black")+
  expand_limits(y = 400)+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  theme(legend.position = "none")
t.test_CO2_nearfar = t.test(x = Data_figure_1c$co[Data_figure_1c$Location %in% "Near field" & Data_figure_1c$notes %in% c("Sitting", "Standing") & Data_figure_1c$ACH %in% 0], y = Data_figure_1c$co[Data_figure_1c$Location %in% "Far field" & Data_figure_1c$notes %in% c("Sitting", "Standing") &  Data_figure_1c$ACH %in% 0], alternative = "greater", paired = TRUE)



# Figure 1d 
PLOT_PARTICLES = ggplot(table_particles_2, aes(x = Location, y = log(Mean), shape = factor(Size, levels = c("0.3-1", "1-2.5", "2.5-3", "3-5", "5-10", "10-25")), color = Location))+
  geom_point(size = 3)+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "92" , " ,", " ", "P = ", formatC(ttest_particle_final_0.3$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black")+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "92" , " ,", " ", "P = ", formatC(ttest_particle_final_1$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black", y_position = 6.2)+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "92" , " ,", " ", "P(2.5-3µ) = ", formatC(ttest_particle_final_2.5$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black", y_position = 4.1)+
  geom_signif(comparisons = list(c("Near field","Far field")), annotations = str_c("n = ", "92" , " ,", " ", "P(3-5µ) = ", formatC(ttest_particle_final_3$p.value, digits = 2)), map_signif_level = TRUE, step_increase = 0.05, tip_length = 0.01, textsize = 4, fontface = 'italic', color= "black", y_position = 4.6)+
  labs(x= "Trial", y = "Log(mean) number of particles", shape = "Size bin (µ)",tag = "d")+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))+
  guides(color = FALSE)

ttest_particle_final_0.3 = t.test(x = data_particle_DesKmainDoor$TSI_0.3_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_0.3_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )
ttest_particle_final_1 = t.test(x = data_particle_DesKmainDoor$TSI_1_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_1_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )
ttest_particle_final_2.5 = t.test(x = data_particle_DesKmainDoor$TSI_2.5_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_2.5_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )
ttest_particle_final_3 = t.test(x = data_particle_DesKmainDoor$TSI_3_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_3_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )
ttest_particle_final_5 = t.test(x = data_particle_DesKmainDoor$TSI_5_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_5_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )
ttest_particle_final_10 = t.test(x = data_particle_DesKmainDoor$TSI_10_Desk[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], y =data_particle_DesKmainDoor$TSI_10_door[data_particle_DesKmainDoor$Notes %in% c("Sitting", "Standing") & data_particle_DesKmainDoor$ACH %in% 0], paired = TRUE, alternative = "greater" )


#Figure 1e
plot_particle_ct = ggplot(data_figre_2e, aes(y = Ct, x = log(Particle), color = factor(bin, levels = c("0.3-1", "1-2.5", "2.5-3", "3-5", "5-10", "10-25"))))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(x= "log(# of far field particles)", y = expression(paste("Aerosol C"["T"], " in far field")), tag = "e", color = "Size bin (µ)") +
  stat_fit_glance(method = 'lm',  method.args = list(formula = y ~ exp(x)),
                  geom = 'text',
                  aes(label = paste("P = ", signif(..p.value.., digits = 4), sep = ""), fontface = "italic"),
                  label.x.npc = "right", label.y.npc = 0.35, size = 4)+
  theme_classic()+
  theme(plot.tag = element_text(face = "bold", size = 20))

ggarrange(ggarrange(Plot_Close_field_scatter, Plot_Close_Far_Box), ggarrange(Plot_CO2_nearfar, PLOT_PARTICLES) , plot_particle_ct, nrow = 3, ncol = 1, widths = 25, heights = 25)+
  ggsave(filename = "Figure 2.tif", width = 13.889/1.4, height = 17.361/1.4, units = "in" ,device = "tiff", path ="/Users/hoomanp/Desktop/RDM/TIFF" ,dpi = 600, limitsize = FALSE)
