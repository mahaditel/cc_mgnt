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
library(writexl)


##1. data load
cc_biomass<-read_excel("data/biomass_dryweight_cc_ngmt_26.xlsx")


######check data####

# Check column names and structure
names(cc_biomass)
str(cc_biomass)



# Check crop levels
unique(cc_biomass$crop)
unique(cc_biomass$covercrop)
unique(cc_biomass$tillage)
length(unique(cc_biomass$crop))

unique(cc_biomass$location)



#####variables into factor#####
cc_biomass<- cc_biomass %>% 
  mutate(plot_id = factor(plot_id),
         crop= factor(crop),
         location= factor(location),
         tillage = factor(tillage),
         covercrop = factor(covercrop),
         rep= factor(rep))


####Checking missing values/duplictes


cc_biomass %>%
  summarise(across(everything(), ~sum(is.na(.))))





missing_summary <- cc_biomass %>%
  summarise(
    across(
      everything(),
      list(
        Missing = ~sum(is.na(.)),
        Percent = ~round(mean(is.na(.)) * 100, 2)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(Missing|Percent)$"
  ) %>%
  arrange(desc(Missing))

print(missing_summary, n = Inf)




#missing by cover crop
cc_biomass %>%
  group_by(covercrop) %>%
  summarise(
    Total_Rows = n(),
    Missing = sum(is.na(`net_biomass_Mg_per_ha~t/ha`)),
    Missing_Percent = round(
      100 * mean(is.na(`net_biomass_Mg_per_ha~t/ha`)), 2
    ),
    .groups = "drop"
  )

##missing by tillage
cc_biomass %>%
  group_by(tillage) %>%
  summarise(
    Total_Rows = n(),
    Missing = sum(is.na(`net_biomass_Mg_per_ha~t/ha`)),
    Missing_Percent = round(
      100 * mean(is.na(`net_biomass_Mg_per_ha~t/ha`)), 2
    ),
    .groups = "drop"
  )

##missing by tillage
cc_biomass %>%
  group_by(location) %>%
  summarise(
    Total_Rows = n(),
    Missing = sum(is.na(`net_biomass_Mg_per_ha~t/ha`)),
    Missing_Percent = round(
      100 * mean(is.na(`net_biomass_Mg_per_ha~t/ha`)), 2
    ),
    .groups = "drop"
  )




#missing by loa=ca==

missing_by_location <- cc_biomass %>%
  group_by(location, crop, covercrop) %>%
  summarise(
    Total = n(),
    Missing = sum(is.na(`net_biomass_Mg_per_ha~t/ha`)),
    Available = sum(!is.na(`net_biomass_Mg_per_ha~t/ha`)),
    Missing_Percent = round(
      100 * mean(is.na(`net_biomass_Mg_per_ha~t/ha`)),
      1
    ),
    .groups = "drop"
  ) %>%
  arrange(location, crop, covercrop)

print(missing_by_location, n = Inf)

##########################################################################
######plotting fo initial visualization


# make a plotting dataset
plot_data <- cc_biomass %>%
  filter(!is.na(`net_biomass_Mg_per_ha~t/ha`)) %>%
  mutate(
    covercrop = factor(covercrop, levels = c("None", "Single", "Mix 1", "Mix 2"))
  )

ggplot(plot_data, aes(x = covercrop, y = `net_biomass_Mg_per_ha~t/ha`, fill = covercrop)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2) +
  labs(
    title = "Cover crop biomass by cover crop treatment",
    x = "Cover crop treatment",
    y = "Biomass (Mg ha⁻¹)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#####

summary_data <- cc_biomass %>%
  filter(!is.na(`net_biomass_Mg_per_ha~t/ha`)) %>%
  group_by(covercrop) %>%
  summarise(
    n = n(),
    mean_biomass = mean(`net_biomass_Mg_per_ha~t/ha`, na.rm = TRUE),
    sd_biomass = sd(`net_biomass_Mg_per_ha~t/ha`, na.rm = TRUE),
    se_biomass = sd_biomass / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    covercrop = factor(covercrop, levels = c("None", "Single", "Mix 1", "Mix 2"))
  )

ggplot(summary_data, aes(x = covercrop, y = mean_biomass, fill = covercrop)) +
  geom_col(width = 0.7, color = "black") +
  geom_errorbar(
    aes(ymin = mean_biomass - se_biomass,
        ymax = mean_biomass + se_biomass),
    width = 0.2
  ) +
  labs(
    title = "Mean cover crop biomass by treatment",
    x = "Cover crop treatment",
    y = "Mean biomass (Mg ha⁻¹)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


##facet by crop
plot_data3 <- cc_biomass %>%
  filter(!is.na(`net_biomass_Mg_per_ha~t/ha`)) %>%
  mutate(
    covercrop = factor(covercrop, levels = c("None", "Single", "Mix 1", "Mix 2"))
  )

ggplot(plot_data3, aes(x = covercrop, y = `net_biomass_Mg_per_ha~t/ha`, fill = covercrop)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8) +
  facet_wrap(~ crop) +
  labs(
    title = "Cover crop biomass by treatment and crop phase",
    x = "Cover crop treatment",
    y = "Biomass (Mg ha⁻¹)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )
################################################
############Tillage ########
#######by crop phase
ggplot(plot_tillage,
       aes(x = tillage,
           y = `net_biomass_Mg_per_ha~t/ha`,
           fill = tillage)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.8) +
  facet_wrap(~ crop) +
  labs(
    title = "Cover Crop Biomass by Tillage and Crop Phase",
    x = "Tillage Practice",
    y = expression("Biomass (Mg ha"^{-1}*")")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

#########################################
###Test Stat analysis####


biomass_analysis <- cc_biomass %>%
  filter(
    location != "Altamont",
    !is.na(`net_biomass_Mg_per_ha~t/ha`)
  ) %>%
  mutate(
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    ),
    tillage = factor(
      tillage,
      levels = c("NT", "SR", "ST")
    )
  )




cc_biomass %>%
  filter(location %in% c("Monticello", "Belleville")) %>%
  distinct(
    location, crop, rep, tillage,
    covercrop, plot_id
  ) %>%
  arrange(location, crop, rep, plot_id) %>%
  print(n = Inf)



######test code having location in the model####


# Prepare data
biomass_primary <- cc_biomass %>%
  filter(
    year == 2026,
    location %in% c("Orr", "Urbana", "Monmouth"),
    !is.na(`net_biomass_Mg_per_ha~t/ha`)
  ) %>%
  mutate(
    biomass = `net_biomass_Mg_per_ha~t/ha`,
    location = factor(location),
    crop = factor(tolower(as.character(crop))),
    rep = factor(rep),
    tillage = factor(tillage),
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    )
  )

# -------------------------
# CORN FIELD ANALYSIS
# -------------------------

corn_data <- biomass_primary %>%
  filter(crop == "corn") %>%
  droplevels() %>%
  mutate(
    block_id = interaction(location, rep, drop = TRUE),
    wholeplot_id = interaction(location, rep, tillage, drop = TRUE)
  )

model_corn <- lmer(
  biomass ~ location + tillage * covercrop +
    (1 | block_id) +
    (1 | wholeplot_id),
  data = corn_data,
  REML = TRUE
)

summary(model_corn)
anova(model_corn, type = 3)

# -------------------------
# SOYBEAN FIELD ANALYSIS
# -------------------------

soybean_data <- biomass_primary %>%
  filter(crop == "soybean") %>%
  droplevels() %>%
  mutate(
    block_id = interaction(location, rep, drop = TRUE),
    wholeplot_id = interaction(location, rep, tillage, drop = TRUE)
  )

model_soybean <- lmer(
  biomass ~ location + tillage * covercrop +
    (1 | block_id) +
    (1 | wholeplot_id),
  data = soybean_data,
  REML = TRUE
)

summary(model_soybean)
anova(model_soybean, type = 3)


#####analysis of Urbana###

# Select Urbana, 2026
urbana <- cc_biomass %>%
  filter(
    location == "Urbana",
    year == 2026
  ) %>%
  mutate(
    biomass = `net_biomass_Mg_per_ha~t/ha`,
    crop = factor(tolower(as.character(crop))),
    rep = factor(rep),
    tillage = factor(tillage, levels = c("NT", "SR", "ST")),
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    )
  )

# Check available observations
urbana %>%
  group_by(crop, tillage, covercrop) %>%
  summarise(
    Total = n(),
    Available = sum(!is.na(biomass)),
    .groups = "drop"
  ) %>%
  print(n = Inf)


#corn analysis

urbana_corn <- urbana %>%
  filter(
    crop == "corn") %>%
  droplevels() %>%
  mutate(
    wholeplot = interaction(rep, tillage, drop = TRUE)
  )

# Check remaining observations
urbana_corn %>%
  count(tillage, covercrop) %>%
  print(n = Inf)  

library(dplyr)
library(ggplot2)

urbana_plot <- cc_biomass %>%
  filter(
    location == "Urbana",
    year == 2026
  ) %>%
  mutate(
    crop = factor(tolower(as.character(crop)),
                  levels = c("corn", "soybean")),
    tillage = factor(tillage, levels = c("NT", "SR", "ST")),
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    ),
    biomass = `net_biomass_Mg_per_ha~t/ha`,
    
    # for plotting only:
    biomass_plot = case_when(
      covercrop == "None" & is.na(biomass) ~ 0,
      TRUE ~ biomass
    )
  ) %>%
  filter(!is.na(biomass_plot))


ggplot(
  urbana_plot,
  aes(x = tillage, y = biomass_plot, fill = covercrop)
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    alpha = 0.8,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = covercrop),
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.8
    ),
    alpha = 0.7,
    size = 2
  ) +
  facet_wrap(~ crop) +
  scale_fill_manual(
    values = c(
      "None" = "gray60",
      "Single" = "#009E73",
      "Mix 1" = "#E69F00",
      "Mix 2" = "#0072B2"
    )
  ) +
  scale_color_manual(
    values = c(
      "None" = "gray30",
      "Single" = "#009E73",
      "Mix 1" = "#E69F00",
      "Mix 2" = "#0072B2"
    )
  ) +
  labs(
    title = "Cover Crop Biomass at Urbana (2026)",
    subtitle = "Missing biomass in None plots shown as 0 for visualization",
    x = "Tillage practice",
    y = expression("Biomass (Mg ha"^-1 * ")"),
    fill = "Cover crop",
    color = "Cover crop"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )


summary_urbana <- urbana_plot %>%
  group_by(crop, tillage, covercrop) %>%
  summarise(
    n = n(),
    mean_biomass = mean(biomass_plot, na.rm = TRUE),
    sd_biomass = sd(biomass_plot, na.rm = TRUE),
    se_biomass = sd_biomass / sqrt(n),
    .groups = "drop"
  )

ggplot(summary_urbana,
       aes(x = tillage, y = mean_biomass, fill = covercrop)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = mean_biomass - se_biomass,
      ymax = mean_biomass + se_biomass
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ crop) +
  scale_fill_manual(
    values = c(
      "None" = "gray60",
      "Single" = "#009E73",
      "Mix 1" = "#E69F00",
      "Mix 2" = "#0072B2"
    )
  ) +
  labs(
    title = "Mean Cover Crop Biomass at Urbana (2026)",
    subtitle = "Missing biomass in None plots shown as 0 for visualization",
    x = "Tillage practice",
    y = expression("Mean biomass (Mg ha"^-1 * ")"),
    fill = "Cover crop"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top"
  )


# Create a backup before modifying the original data
cc_biomass_backup <- cc_biomass

# Replace NA with 0 only for None treatment
cc_biomass <- cc_biomass %>%
  mutate(
    `net_dry_weight_gm` = if_else(
      covercrop == "None" &
        is.na(`net_dry_weight_gm`),
      0,
      `net_dry_weight_gm`
    )
  )

##save
write_xlsx(
  cc_biomass,
  "data/cc_biomass_updated_2026.xlsx"
)
