##install packages
install.packages("multcompView")
install.packages("DHARMa")
install.packages("dplyr")


# 1. Load necessary libraries
library(readxl)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyverse)
library(multcompView)
library(DHARMa)
library(tidyverse) 
library(lubridate) 
library(scales) 
library(readxl) 
library(skimr) 
library(janitor) 
library(patchwork)
library(car)
library(dplyr)
library(ggplot2)
library(lme4)
library(janitor) 
library(emmeans)
library(multcomp)
library(multcompView)


#####data_load####
soil.df<-read_excel(
  "data/soil analysis_cc_mgnt_26.xlsx")


######check data####

# Check column names and structure
names(soil.df)
str(soil.df)

# Check crop levels
unique(soil.df$crop)
length(unique(soil.df$crop))


#####variables into factor#####
soil.df<- soil.df %>% 
  mutate(location= factor(location),
         crop= factor(crop),
         rep= factor(rep),
         OM_percent = as.numeric(OM_percent),
         CEC_meq_100g = as.numeric(CEC_meq_100g),
         K_Sat_percent = as.numeric(K_Sat_percent),
         Mg_Sat_percent = as.numeric(Mg_Sat_percent),
         Ca_Sat_percent = as.numeric(Ca_Sat_percent),
         H_Sat_percent = as.numeric(H_Sat_percent)
         )


###summary_OM######
om_summary <- soil.df %>% 
  group_by(location, crop) %>% 
  summarise(
    n= sum(!is.na(OM_percent)),
    mean_OM = mean(OM_percent),
    sd_OM = sd( OM_percent, na.rm=TRUE),
    se_OM = sd_OM/sqrt(n),
    .groups ="drop"
  )


######plot_OM#######
############################################################
# GRAPH: ORGANIC MATTER ACROSS ALL LOCATIONS
############################################################

OM_plot <- ggplot(
  om_summary,
  aes(
    x = location,
    y = mean_OM,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_OM - se_OM,
      ymax = mean_OM + se_OM
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Organic Matter (%)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

OM_plot
  


ggsave(
  filename = "plot/soil/OM_all_locations_26.png",
  plot = OM_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: BRAY-1 PHOSPHORUS
############################################################

p_summary <- soil.df %>% 
  group_by(location, crop) %>% 
  summarise(
    n = sum(!is.na(P_Bray_1_ppm)),
    mean_P = mean(P_Bray_1_ppm, na.rm = TRUE),
    sd_P = sd(P_Bray_1_ppm, na.rm = TRUE),
    se_P = sd_P / sqrt(n),
    .groups = "drop"
  )

View(soil.df)

############################################################
# GRAPH: BRAY-1 PHOSPHORUS ACROSS ALL LOCATIONS
############################################################

P_plot <- ggplot(
  p_summary,
  aes(
    x = location,
    y = mean_P,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_P - se_P,
      ymax = mean_P + se_P
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Bray-1 P (ppm)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

P_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/P_Bray_1_all_locations_26.png",
  plot = P_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: POTASSIUM (K)
############################################################

k_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(K_ppm)),
    mean_K = mean(K_ppm, na.rm = TRUE),
    sd_K = sd(K_ppm, na.rm = TRUE),
    se_K = sd_K / sqrt(n),
    .groups = "drop"
  )

k_summary


############################################################
# GRAPH: POTASSIUM ACROSS ALL LOCATIONS
############################################################

K_plot <- ggplot(
  k_summary,
  aes(
    x = location,
    y = mean_K,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_K - se_K,
      ymax = mean_K + se_K
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Potassium (ppm)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

K_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/K_all_locations_26.png",
  plot = K_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: MAGNESIUM (Mg)
############################################################

mg_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(Mg_ppm)),
    mean_Mg = mean(Mg_ppm, na.rm = TRUE),
    sd_Mg = sd(Mg_ppm, na.rm = TRUE),
    se_Mg = sd_Mg / sqrt(n),
    .groups = "drop"
  )

mg_summary


############################################################
# GRAPH: MAGNESIUM ACROSS ALL LOCATIONS
############################################################

Mg_plot <- ggplot(
  mg_summary,
  aes(
    x = location,
    y = mean_Mg,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_Mg - se_Mg,
      ymax = mean_Mg + se_Mg
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Magnesium (ppm)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

Mg_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/Mg_all_locations_26.png",
  plot = Mg_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: CALCIUM (Ca)
############################################################

ca_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(Ca_ppm)),
    mean_Ca = mean(Ca_ppm, na.rm = TRUE),
    sd_Ca = sd(Ca_ppm, na.rm = TRUE),
    se_Ca = sd_Ca / sqrt(n),
    .groups = "drop"
  )

ca_summary


############################################################
# GRAPH: CALCIUM ACROSS ALL LOCATIONS
############################################################

Ca_plot <- ggplot(
  ca_summary,
  aes(
    x = location,
    y = mean_Ca,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_Ca - se_Ca,
      ymax = mean_Ca + se_Ca
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Calcium (ppm)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

Ca_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/Ca_all_locations_26.png",
  plot = Ca_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: SOIL pH (1:1)
############################################################

ph_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(pH_1_1)),
    mean_pH = mean(pH_1_1, na.rm = TRUE),
    sd_pH = sd(pH_1_1, na.rm = TRUE),
    se_pH = sd_pH / sqrt(n),
    .groups = "drop"
  )

ph_summary


############################################################
# GRAPH: SOIL pH ACROSS ALL LOCATIONS
############################################################

pH_plot <- ggplot(
  ph_summary,
  aes(
    x = location,
    y = mean_pH,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_pH - se_pH,
      ymax = mean_pH + se_pH
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Soil pH (1:1)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

pH_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/pH_all_locations_26.png",
  plot = pH_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: BUFFER pH
############################################################

buffer_ph_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(Buffer_pH)),
    mean_buffer_pH = mean(Buffer_pH, na.rm = TRUE),
    sd_buffer_pH = sd(Buffer_pH, na.rm = TRUE),
    se_buffer_pH = sd_buffer_pH / sqrt(n),
    .groups = "drop"
  )

buffer_ph_summary


############################################################
# GRAPH: BUFFER pH ACROSS ALL LOCATIONS
############################################################

Buffer_pH_plot <- ggplot(
  buffer_ph_summary,
  aes(
    x = location,
    y = mean_buffer_pH,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_buffer_pH - se_buffer_pH,
      ymax = mean_buffer_pH + se_buffer_pH
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Buffer pH",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

Buffer_pH_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/Buffer_pH_all_locations_26.png",
  plot = Buffer_pH_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: CATION EXCHANGE CAPACITY (CEC)
############################################################

cec_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(CEC_meq_100g)),
    mean_CEC = mean(CEC_meq_100g, na.rm = TRUE),
    sd_CEC = sd(CEC_meq_100g, na.rm = TRUE),
    se_CEC = sd_CEC / sqrt(n),
    .groups = "drop"
  )

cec_summary


############################################################
# GRAPH: CEC ACROSS ALL LOCATIONS
############################################################

CEC_plot <- ggplot(
  cec_summary,
  aes(
    x = location,
    y = mean_CEC,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_CEC - se_CEC,
      ymax = mean_CEC + se_CEC
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "CEC (meq/100 g)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

CEC_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/CEC_all_locations_26.png",
  plot = CEC_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: POTASSIUM SATURATION (%)
############################################################

k_sat_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(K_Sat_percent)),
    mean_K_sat = mean(K_Sat_percent, na.rm = TRUE),
    sd_K_sat = sd(K_Sat_percent, na.rm = TRUE),
    se_K_sat = sd_K_sat / sqrt(n),
    .groups = "drop"
  )

k_sat_summary


############################################################
# GRAPH: POTASSIUM SATURATION ACROSS ALL LOCATIONS
############################################################

K_sat_plot <- ggplot(
  k_sat_summary,
  aes(
    x = location,
    y = mean_K_sat,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_K_sat - se_K_sat,
      ymax = mean_K_sat + se_K_sat
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "K Saturation (%)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

K_sat_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/K_Saturation_all_locations_26.png",
  plot = K_sat_plot,
  width = 10,
  height = 6,
  dpi = 300
)



############################################################
# SUMMARY: MAGNESIUM SATURATION (%)
############################################################

mg_sat_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(Mg_Sat_percent)),
    mean_Mg_sat = mean(Mg_Sat_percent, na.rm = TRUE),
    sd_Mg_sat = sd(Mg_Sat_percent, na.rm = TRUE),
    se_Mg_sat = sd_Mg_sat / sqrt(n),
    .groups = "drop"
  )

mg_sat_summary



############################################################
# GRAPH: MAGNESIUM SATURATION ACROSS ALL LOCATIONS
############################################################

Mg_sat_plot <- ggplot(
  mg_sat_summary,
  aes(
    x = location,
    y = mean_Mg_sat,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_Mg_sat - se_Mg_sat,
      ymax = mean_Mg_sat + se_Mg_sat
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Mg Saturation (%)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

Mg_sat_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/Mg_Saturation_all_locations_26.png",
  plot = Mg_sat_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: CALCIUM SATURATION (%)
############################################################

ca_sat_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(Ca_Sat_percent)),
    mean_Ca_sat = mean(Ca_Sat_percent, na.rm = TRUE),
    sd_Ca_sat = sd(Ca_Sat_percent, na.rm = TRUE),
    se_Ca_sat = sd_Ca_sat / sqrt(n),
    .groups = "drop"
  )

ca_sat_summary


############################################################
# GRAPH: CALCIUM SATURATION ACROSS ALL LOCATIONS
############################################################

Ca_sat_plot <- ggplot(
  ca_sat_summary,
  aes(
    x = location,
    y = mean_Ca_sat,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_Ca_sat - se_Ca_sat,
      ymax = mean_Ca_sat + se_Ca_sat
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Ca Saturation (%)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

Ca_sat_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/Ca_Saturation_all_locations_26.png",
  plot = Ca_sat_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: HYDROGEN SATURATION (%)
############################################################

h_sat_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(H_Sat_percent)),
    mean_H_sat = mean(H_Sat_percent, na.rm = TRUE),
    sd_H_sat = sd(H_Sat_percent, na.rm = TRUE),
    se_H_sat = sd_H_sat / sqrt(n),
    .groups = "drop"
  )

h_sat_summary


############################################################
# GRAPH: HYDROGEN SATURATION ACROSS ALL LOCATIONS
############################################################

H_sat_plot <- ggplot(
  h_sat_summary,
  aes(
    x = location,
    y = mean_H_sat,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_H_sat - se_H_sat,
      ymax = mean_H_sat + se_H_sat
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "H Saturation (%)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

H_sat_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/H_Saturation_all_locations_26.png",
  plot = H_sat_plot,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# SUMMARY: SULFUR (S)
############################################################

s_summary <- soil.df |> 
  group_by(location, crop) |> 
  summarise(
    n = sum(!is.na(S_ppm)),
    mean_S = mean(S_ppm, na.rm = TRUE),
    sd_S = sd(S_ppm, na.rm = TRUE),
    se_S = sd_S / sqrt(n),
    .groups = "drop"
  )

s_summary


############################################################
# GRAPH: SULFUR ACROSS ALL LOCATIONS
############################################################

S_plot <- ggplot(
  s_summary,
  aes(
    x = location,
    y = mean_S,
    fill = crop
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_S - se_S,
      ymax = mean_S + se_S
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  labs(
    x = "Location",
    y = "Sulfur (ppm)",
    fill = "Crop"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

S_plot


############################################################
# SAVE FIGURE
############################################################

ggsave(
  filename = "plot/soil/S_all_locations_26.png",
  plot = S_plot,
  width = 10,
  height = 6,
  dpi = 300
)
