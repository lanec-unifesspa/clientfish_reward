install.packages("tidyverse")
install.packages("dplyr")
install.packages("haven")
install.packages("stringr")
install.packages("ggplot2")
install.packages("psych")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("WRS2")
install.packages("dabestr")

client_CPP <- read_csv("Dropbox/LaNec/dados/Cleaners/Detour/client-CPP.csv") #Load dataframe in short form
client_CPP <- gather(client_CPP, key = "Timepoint", value = "time", 2:3) #Join time variables in one column, reorganizing dataframe in longform
dabest_obj_CPP <- load(data =  client_CPP, x = Timepoint, y = time, idx = c("Baseline", "Test"), paired = "sequential", id_col = ID) %>% mean_diff() #Prepare data for plotting
dabest_plot(dabest_obj_CPP, raw_marker_size = 0.7, raw_marker_alpha = 0.3, swarm_label = "Time in non-preferred/conditioned compartment (%)") #Gardner-Altman estimation plot

yuend(x = client_CPP$Test, y = client_CPP$Basal, tr = 0.2) #Yuen's test on trimmed means for dependent samples
dep.effect(x = client_CPP$Test, y = client_CPP$Basal) #Effect size calculation for the test

client_CPP <- read_csv("Dropbox/LaNec/dados/Cleaners/Detour/client_CPP.csv") #Load dataframe in short form
corr <- round(cor(client_CPP[2:11]), 1) #Correlation matrix; since overall jolt levels were too low, these were not included in the matrix
p.mat <- cor_pmat(client_CPP[2:11]) #P-values for the correlation matrix
ggcorrplot(corr, method = "circle", type = "lower", p.mat = p.mat, insig = "blank") #Correlation plot

naloxone_CPP <- read_csv("Dropbox/LaNec/dados/Cleaners/Detour/naloxone_CPP.csv",
                         col_types = cols(Dose = col_factor(levels = c("0 mg/kg",
                                                                       "1.5 mg/kg", "3 mg/kg"))))
View(naloxone_CPP)

t1waybt(Delta ~ Dose, data = naloxone_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for preference data
mcppb20(Delta ~ Dose, data = naloxone_CPP, tr = 0.2, nboot = 200) #Post-hoc test
dabest_obj_naloxone_delta <- load(data =  naloxone_CPP, x = Dose, y = Delta, idx = c("0 mg/kg", "1.5 mg/kg", "3 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_naloxone_delta, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Preference index (%)")

t1waybt(Time_immobile ~ Dose, data = naloxone_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for immobility data
dabest_obj_naloxone_imm <- load(data =  naloxone_CPP, x = Dose, y = Time_immobile, idx = c("0 mg/kg", "1.5 mg/kg", "3 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_naloxone_imm, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Time immobile (s)")

t1waybt(Distance ~ Dose, data = naloxone_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for immobility data
dabest_obj_naloxone_dist <- load(data =  naloxone_CPP, x = Dose, y = Distance, idx = c("0 mg/kg", "1.5 mg/kg", "3 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_naloxone_dist, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Distance swam (cm)")

t1waybt(Speed ~ Dose, data = naloxone_CPP, tr = 0.2, nboot = 200)
dabest_obj_naloxone_speed <- load(data =  naloxone_CPP, x = Dose, y = Speed, idx = c("0 mg/kg", "1.5 mg/kg", "3 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_naloxone_speed, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Avg. swimming speed (cm/s)")

DAMGO_CPP <- read_csv("Dropbox/LaNec/dados/Cleaners/Detour/DAMGO_CPP.csv",
                      col_types = cols(Dose = col_factor(levels = c("0 mg/kg",
                                                                    "1 mg/kg", "5 mg/kg"))))
View(DAMGO_CPP)

t1waybt(Delta ~ Dose, data = DAMGO_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for preference data
mcppb20(Delta ~ Dose, data = DAMGO_CPP, tr = 0.2, nboot = 200) #Post-hoc test
dabest_obj_DAMGO_delta <- load(data =  DAMGO_CPP, x = Dose, y = Delta, idx = c("0 mg/kg", "1 mg/kg", "5 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_DAMGO_delta, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Preference index (%)")

t1waybt(Time_immobile ~ Dose, data = DAMGO_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for immobiity data
dabest_obj_DAMGO_imm <- load(data =  DAMGO_CPP, x = Dose, y = Time_immobile, idx = c("0 mg/kg", "1 mg/kg", "5 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_DAMGO_imm, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Time immobile (s)")

t1waybt(Distance ~ Dose, data = DAMGO_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for distance data
dabest_obj_DAMGO_dist <- load(data =  DAMGO_CPP, x = Dose, y = Distance, idx = c("0 mg/kg", "1 mg/kg", "5 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_DAMGO_dist, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Distance swam (cm)")

t1waybt(Speed ~ Dose, data = DAMGO_CPP, tr = 0.2, nboot = 200) #Robust bootstrap ANOVA for speed data
dabest_obj_DAMGO_speed <- load(data =  DAMGO_CPP, x = Dose, y = Speed, idx = c("0 mg/kg", "1 mg/kg", "5 mg/kg")) %>% hedges_g()
dabest_plot(dabest_obj_DAMGO_speed, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Avg. swimming speed (cm/s)")

detour_drugs <- read_csv("Dropbox/LaNec/dados/Cleaners/Detour/detour_drugs.csv",
                         col_types = cols(Treatment = col_factor(levels = c("Trial 1",
                                                                            "Trial 2", "Trial 3", "PBS", "NAL",
                                                                            "DAMGO")), Individual = col_skip()))  #Load dataframe in short form
View(detour_drugs)
rmanova(detour_drugs$Barriers_stuck, detour_drugs$Treatment, detour_drugs$ID) #Robust repeated measures ANOVA on number of barriers crossed
rmmcp(detour_drugs$Barriers_stuck, detour_drugs$Treatment, detour_drugs$ID) #Post-hoc test
dabest_plot(dabest_obj_detour, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Max. barriers crossed in trial") #Plot barrier data


rmanova(detour_drugs$Latency_cross_barrier, detour_drugs$Treatment, detour_drugs$ID) #Robust repeated measures ANOVA on number of barriers crossed
rmmcp(detour_drugs$Latency_cross_barrier, detour_drugs$Treatment, detour_drugs$ID) #Post-hoc test
dabest_plot(dabest_obj_detourlat, raw_marker_size = 0.7, contrast_ylim = c(-3, 3), raw_marker_alpha = 0.3, swarm_label = "Latency to cross barriers (s)") #Plot latency data
