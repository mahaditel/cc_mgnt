##install packages
install.packages("multcompView")
install.packages("DHARMa")


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




############################################################
# MAHADI STAND COUNT ANALYSIS
# Location: Urbana
# Crops: Corn and Soybean
# Response: Mean stand count from two subsamples per plot
############################################################


############################################################
# 1. LOAD REQUIRED PACKAGES
############################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(multcomp)


############################################################
# 2. IMPORT DATA
############################################################

mahadi.df <- read_excel(
  "data/st_count_all_26_corn_soy.xlsx"
)


############################################################
# 3. EXPLORE DATA STRUCTURE AND TREATMENTS
############################################################

# Check column names and structure
names(mahadi.df)
str(mahadi.df)

# Check crop levels
unique(mahadi.df$crop)
length(unique(mahadi.df$crop))

# Check cover crop treatments
unique(mahadi.df$covercrop)

# Check tillage treatments
unique(mahadi.df$tillage)

# Check treatment numbers
unique(mahadi.df$treatment_number)

# Check locations
unique(mahadi.df$location)

# Check plot IDs
unique(mahadi.df$plot_id)

# Number of observations for each treatment combination
mahadi.df |>
  count(location, crop, covercrop, tillage)


############################################################
# 4. CONVERT CATEGORICAL VARIABLES TO FACTORS
############################################################

mahadi.df <- mahadi.df |>
  mutate(
    crop = factor(crop),
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    ),
    tillage = factor(tillage),
    treatment_number = factor(treatment_number),
    location = factor(location),
    plot_id = factor(plot_id),
    rep = factor(rep)
  )


############################################################
# 5. SELECT VARIABLES NEEDED FOR STAND COUNT ANALYSIS
############################################################

mahadi.df1 <- mahadi.df |>
  dplyr::select(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop,
    stand1,
    stand2
  )


############################################################
# 6. FILTER URBANA LOCATION
############################################################

mahadi_urbana <- mahadi.df1 |>
  filter(location == "Urbana")


############################################################
# 7. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

# stand1 and stand2 are two subsamples collected
# from the same experimental plot at the same time.

mahadi_urbana_long <- mahadi_urbana |>
  pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 8. AVERAGE THE TWO STAND SUBSAMPLES WITHIN EACH PLOT
############################################################

# This creates one stand value per experimental plot.
# stand1 and stand2 are subsamples, not independent replicates.

mahadi_urbana_long_avg <- mahadi_urbana_long |>
  group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 9. CHECK REPLICATION
############################################################

# Ideally each crop x tillage x cover crop combination
# should contain 4 independent reps.

mahadi_urbana_long_avg |>
  count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 10. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

# These summaries are used for plotting.
# Statistical models should use mahadi_urbana_long_avg,
# not this summarized dataframe.

mahadi_plot_urbana_summary <- mahadi_urbana_long_avg |>
  group_by(
    crop,
    tillage,
    covercrop
  ) |>
  summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 11. PLOT MEAN STAND COUNT ± SE
############################################################

# Cover crop order:
# None -> Single -> Mix 1 -> Mix 2
#
# Rows = crop
# Columns = tillage

urbana_count <- ggplot(
  mahadi_plot_urbana_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


# Display graph
urbana_count


############################################################
# 12. SAVE FIGURE
############################################################

ggsave(
  filename = "plot/urbana_count.png",
  plot = urbana_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 13. SEPARATE CORN AND SOYBEAN FOR STATISTICAL ANALYSIS
############################################################

urbana_corn <- mahadi_urbana_long_avg |>
  filter(crop == "corn")

urbana_soybean <- mahadi_urbana_long_avg |>
  filter(crop == "soybean")


############################################################
# 14. CORN ANALYSIS
############################################################

# Factorial RCBD model:
# Response = stand_mean
# Block = rep
# Fixed effects = tillage, covercrop,
# and tillage x covercrop interaction

corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = urbana_corn
)


############################################################
# 15. CORN ANOVA
############################################################

anova(corn_model)


############################################################
# 16. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(corn_model)
)


############################################################
# 17. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

corn_tillage_emm <- emmeans(
  corn_model,
  ~ tillage
)

# Estimated marginal means
corn_tillage_emm

# Tukey-adjusted pairwise comparisons
pairs(
  corn_tillage_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


###########################################################
# 18. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

corn_covercrop_emm <- emmeans(
  corn_model,
  ~ covercrop
)

# Estimated marginal means
corn_covercrop_emm

# Tukey-adjusted pairwise comparisons
pairs(
  corn_covercrop_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 19. SOYBEAN ANALYSIS
############################################################

soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = urbana_soybean
)


############################################################
# 20. SOYBEAN ANOVA
############################################################

anova(soybean_model)


############################################################
# 21. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(soybean_model)
)


############################################################
# 22. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

soybean_tillage_emm <- emmeans(
  soybean_model,
  ~ tillage
)

soybean_tillage_emm

pairs(
  soybean_tillage_emm,
  adjust = "tukey"
)

cld(
  soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 23. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

soybean_covercrop_emm <- emmeans(
  soybean_model,
  ~ covercrop
)

soybean_covercrop_emm

pairs(
  soybean_covercrop_emm,
  adjust = "tukey"
)

cld(
  soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)





############################################################
# 6. FILTER MONTICELLO LOCATION
############################################################



mahadi_monticello <- mahadi.df1 |>
  dplyr::filter(location == "Monticello")


############################################################
# 7. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

# stand1 and stand2 are two subsamples collected
# from the same experimental plot at the same time.

mahadi_monticello_long <- mahadi_monticello |>
  tidyr::pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 8. AVERAGE THE TWO STAND SUBSAMPLES WITHIN EACH PLOT
############################################################

# This creates one stand value per experimental plot.
# stand1 and stand2 are subsamples, not independent replicates.

mahadi_monticello_long_avg <- mahadi_monticello_long |>
  dplyr::group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 9. CHECK REPLICATION
############################################################

# Ideally each crop x tillage x cover crop combination
# should contain 4 independent reps.

mahadi_monticello_long_avg |>
  dplyr::count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 10. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

# These summaries are used for plotting.
# Statistical models should use mahadi_monticello_long_avg,
# not this summarized dataframe.

mahadi_plot_monticello_summary <- mahadi_monticello_long_avg |>
  dplyr::group_by(
    crop,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 11. PLOT MEAN STAND COUNT ± SE
############################################################

# Rows = crop
# Columns = tillage
# Cover crop order = None, Single, Mix 1, Mix 2

monticello_count <- ggplot(
  mahadi_plot_monticello_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


# Display graph
monticello_count


############################################################
# 12. SAVE MONTICELLO FIGURE
############################################################

ggsave(
  filename = "plot/monticello_count.png",
  plot = monticello_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 13. SEPARATE CORN AND SOYBEAN FOR STATISTICAL ANALYSIS
############################################################

monticello_corn <- mahadi_monticello_long_avg |>
  dplyr::filter(crop == "corn")

monticello_soybean <- mahadi_monticello_long_avg |>
  dplyr::filter(crop == "soybean")


############################################################
# 14. MONTICELLO CORN ANALYSIS
############################################################

# Factorial RCBD model
# Response = stand_mean
# Block = rep
# Fixed effects = tillage, covercrop,
# and tillage x covercrop interaction

monticello_corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = monticello_corn
)


############################################################
# 15. MONTICELLO CORN ANOVA
############################################################

anova(monticello_corn_model)


############################################################
# 16. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(monticello_corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(monticello_corn_model)
)


############################################################
# 17. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

monticello_corn_tillage_emm <- emmeans(
  monticello_corn_model,
  ~ tillage
)

# Estimated marginal means
monticello_corn_tillage_emm

# Tukey-adjusted pairwise comparisons
pairs(
  monticello_corn_tillage_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  monticello_corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 18. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

monticello_corn_covercrop_emm <- emmeans(
  monticello_corn_model,
  ~ covercrop
)

# Estimated marginal means
monticello_corn_covercrop_emm

# Tukey-adjusted pairwise comparisons
pairs(
  monticello_corn_covercrop_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  monticello_corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 19. MONTICELLO SOYBEAN ANALYSIS
############################################################

monticello_soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = monticello_soybean
)


############################################################
# 20. MONTICELLO SOYBEAN ANOVA
############################################################

anova(monticello_soybean_model)


############################################################
# 21. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(monticello_soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(monticello_soybean_model)
)


############################################################
# 22. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

monticello_soybean_tillage_emm <- emmeans(
  monticello_soybean_model,
  ~ tillage
)

# Estimated marginal means
monticello_soybean_tillage_emm

# Tukey-adjusted pairwise comparisons
pairs(
  monticello_soybean_tillage_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  monticello_soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 23. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

monticello_soybean_covercrop_emm <- emmeans(
  monticello_soybean_model,
  ~ covercrop
)

# Estimated marginal means
monticello_soybean_covercrop_emm

# Tukey-adjusted pairwise comparisons
pairs(
  monticello_soybean_covercrop_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  monticello_soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)





############################################################
# ORR STAND COUNT ANALYSIS
# Crops: Corn and Soybean
# Response: Mean of stand1 and stand2 within each plot
############################################################


############################################################
# 1. FILTER ORR LOCATION
############################################################

mahadi_orr <- mahadi.df1 |>
  dplyr::filter(location == "Orr")



# Check number of observations
nrow(mahadi_orr)


############################################################
# 2. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

# stand1 and stand2 are two subsamples collected
# from the same plot at the same time.

mahadi_orr_long <- mahadi_orr |>
  tidyr::pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 3. AVERAGE STAND1 AND STAND2 WITHIN EACH PLOT
############################################################

# Creates one stand value per experimental plot.

mahadi_orr_long_avg <- mahadi_orr_long |>
  dplyr::group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 4. CHECK REPLICATION
############################################################

# Each crop x tillage x cover crop combination
# should ideally have 4 reps.

mahadi_orr_long_avg |>
  dplyr::count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 5. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

# This dataframe is for plotting.
# Do NOT use the summarized dataframe for the ANOVA.

mahadi_plot_orr_summary <- mahadi_orr_long_avg |>
  dplyr::group_by(
    crop,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 6. PLOT MEAN STAND COUNT ± SE
############################################################

# Rows = crop
# Columns = tillage
# X-axis = cover crop treatment

orr_count <- ggplot(
  mahadi_plot_orr_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


# Display graph
orr_count


############################################################
# 7. SAVE ORR FIGURE
############################################################

ggsave(
  filename = "plot/orr_count.png",
  plot = orr_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 8. SEPARATE CORN AND SOYBEAN
############################################################

orr_corn <- mahadi_orr_long_avg |>
  dplyr::filter(crop == "corn")

orr_soybean <- mahadi_orr_long_avg |>
  dplyr::filter(crop == "soybean")


############################################################
# 9. ORR CORN MODEL
############################################################

# Factorial RCBD model
# Response = stand_mean
# Block = rep
# Treatments = tillage and cover crop

orr_corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = orr_corn
)


############################################################
# 10. ORR CORN ANOVA
############################################################

anova(orr_corn_model)


############################################################
# 11. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(orr_corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(orr_corn_model)
)


############################################################
# 12. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

orr_corn_tillage_emm <- emmeans(
  orr_corn_model,
  ~ tillage
)

# Estimated marginal means
orr_corn_tillage_emm

# Tukey-adjusted pairwise comparisons
pairs(
  orr_corn_tillage_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  orr_corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 13. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

orr_corn_covercrop_emm <- emmeans(
  orr_corn_model,
  ~ covercrop
)

# Estimated marginal means
orr_corn_covercrop_emm

# Tukey-adjusted pairwise comparisons
pairs(
  orr_corn_covercrop_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  orr_corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 14. ORR SOYBEAN MODEL
############################################################

orr_soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = orr_soybean
)


############################################################
# 15. ORR SOYBEAN ANOVA
############################################################

anova(orr_soybean_model)


############################################################
# 16. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(orr_soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(orr_soybean_model)
)


############################################################
# 17. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

orr_soybean_tillage_emm <- emmeans(
  orr_soybean_model,
  ~ tillage
)

# Estimated marginal means
orr_soybean_tillage_emm

# Tukey-adjusted pairwise comparisons
pairs(
  orr_soybean_tillage_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  orr_soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 18. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

orr_soybean_covercrop_emm <- emmeans(
  orr_soybean_model,
  ~ covercrop
)

# Estimated marginal means
orr_soybean_covercrop_emm

# Tukey-adjusted pairwise comparisons
pairs(
  orr_soybean_covercrop_emm,
  adjust = "tukey"
)

# Compact letter display
cld(
  orr_soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# BELLEVILLE STAND COUNT ANALYSIS
# Crops: Corn and Soybean
# Response: Mean of stand1 and stand2 within each plot
############################################################


############################################################
# 1. FILTER BELLEVILLE LOCATION
############################################################

mahadi_belleville <- mahadi.df1 |>
  dplyr::filter(location == "Belleville")



# Check number of observations
nrow(mahadi_belleville)


############################################################
# 2. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

mahadi_belleville_long <- mahadi_belleville |>
  tidyr::pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 3. AVERAGE STAND1 AND STAND2 WITHIN EACH PLOT
############################################################

mahadi_belleville_long_avg <- mahadi_belleville_long |>
  dplyr::group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 4. CHECK REPLICATION
############################################################

mahadi_belleville_long_avg |>
  dplyr::count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 5. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

mahadi_plot_belleville_summary <- mahadi_belleville_long_avg |>
  dplyr::group_by(
    crop,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 6. PLOT MEAN STAND COUNT ± SE
############################################################

belleville_count <- ggplot(
  mahadi_plot_belleville_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


# Display graph
belleville_count


############################################################
# 7. SAVE BELLEVILLE FIGURE
############################################################

ggsave(
  filename = "plot/belleville_count.png",
  plot = belleville_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 8. SEPARATE CORN AND SOYBEAN
############################################################

belleville_corn <- mahadi_belleville_long_avg |>
  dplyr::filter(crop == "corn")

belleville_soybean <- mahadi_belleville_long_avg |>
  dplyr::filter(crop == "soybean")


############################################################
# 9. BELLEVILLE CORN MODEL
############################################################

belleville_corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = belleville_corn
)


############################################################
# 10. BELLEVILLE CORN ANOVA
############################################################

anova(belleville_corn_model)


############################################################
# 11. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(belleville_corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(belleville_corn_model)
)


############################################################
# 12. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

belleville_corn_tillage_emm <- emmeans(
  belleville_corn_model,
  ~ tillage
)

belleville_corn_tillage_emm

pairs(
  belleville_corn_tillage_emm,
  adjust = "tukey"
)

cld(
  belleville_corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 13. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

belleville_corn_covercrop_emm <- emmeans(
  belleville_corn_model,
  ~ covercrop
)

belleville_corn_covercrop_emm

pairs(
  belleville_corn_covercrop_emm,
  adjust = "tukey"
)

cld(
  belleville_corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 14. BELLEVILLE SOYBEAN MODEL
############################################################

belleville_soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = belleville_soybean
)


############################################################
# 15. BELLEVILLE SOYBEAN ANOVA
############################################################

anova(belleville_soybean_model)


############################################################
# 16. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(belleville_soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(belleville_soybean_model)
)


############################################################
# 17. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

belleville_soybean_tillage_emm <- emmeans(
  belleville_soybean_model,
  ~ tillage
)

belleville_soybean_tillage_emm

pairs(
  belleville_soybean_tillage_emm,
  adjust = "tukey"
)

cld(
  belleville_soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 18. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

belleville_soybean_covercrop_emm <- emmeans(
  belleville_soybean_model,
  ~ covercrop
)

belleville_soybean_covercrop_emm

pairs(
  belleville_soybean_covercrop_emm,
  adjust = "tukey"
)

cld(
  belleville_soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)




############################################################
# MONMOUTH STAND COUNT ANALYSIS
# Crops: Corn and Soybean
# Response: Mean of stand1 and stand2 within each plot
############################################################


############################################################
# 1. FILTER MONMOUTH LOCATION
############################################################

mahadi_monmouth <- mahadi.df1 |>
  dplyr::filter(location == "Monmouth")

# Check data
View(mahadi_monmouth)

# Check number of observations
nrow(mahadi_monmouth)


############################################################
# 2. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

mahadi_monmouth_long <- mahadi_monmouth |>
  tidyr::pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 3. AVERAGE STAND1 AND STAND2 WITHIN EACH PLOT
############################################################

mahadi_monmouth_long_avg <- mahadi_monmouth_long |>
  dplyr::group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 4. CHECK REPLICATION
############################################################

mahadi_monmouth_long_avg |>
  dplyr::count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 5. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

mahadi_plot_monmouth_summary <- mahadi_monmouth_long_avg |>
  dplyr::group_by(
    crop,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 6. PLOT MEAN STAND COUNT ± SE
############################################################

monmouth_count <- ggplot(
  mahadi_plot_monmouth_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


# Display graph
monmouth_count


############################################################
# 7. SAVE MONMOUTH FIGURE
############################################################

ggsave(
  filename = "plot/monmouth_count.png",
  plot = monmouth_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 8. SEPARATE CORN AND SOYBEAN
############################################################

monmouth_corn <- mahadi_monmouth_long_avg |>
  dplyr::filter(crop == "corn")

monmouth_soybean <- mahadi_monmouth_long_avg |>
  dplyr::filter(crop == "soybean")


############################################################
# 9. MONMOUTH CORN MODEL
############################################################

monmouth_corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = monmouth_corn
)


############################################################
# 10. MONMOUTH CORN ANOVA
############################################################

anova(monmouth_corn_model)


############################################################
# 11. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(monmouth_corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(monmouth_corn_model)
)


############################################################
# 12. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

monmouth_corn_tillage_emm <- emmeans(
  monmouth_corn_model,
  ~ tillage
)

monmouth_corn_tillage_emm

pairs(
  monmouth_corn_tillage_emm,
  adjust = "tukey"
)

cld(
  monmouth_corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 13. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

monmouth_corn_covercrop_emm <- emmeans(
  monmouth_corn_model,
  ~ covercrop
)

monmouth_corn_covercrop_emm

pairs(
  monmouth_corn_covercrop_emm,
  adjust = "tukey"
)

cld(
  monmouth_corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 14. MONMOUTH SOYBEAN MODEL
############################################################

monmouth_soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = monmouth_soybean
)


############################################################
# 15. MONMOUTH SOYBEAN ANOVA
############################################################

anova(monmouth_soybean_model)


############################################################
# 16. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(monmouth_soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(monmouth_soybean_model)
)


############################################################
# 17. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

monmouth_soybean_tillage_emm <- emmeans(
  monmouth_soybean_model,
  ~ tillage
)

monmouth_soybean_tillage_emm

pairs(
  monmouth_soybean_tillage_emm,
  adjust = "tukey"
)

cld(
  monmouth_soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 18. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

monmouth_soybean_covercrop_emm <- emmeans(
  monmouth_soybean_model,
  ~ covercrop
)

monmouth_soybean_covercrop_emm

pairs(
  monmouth_soybean_covercrop_emm,
  adjust = "tukey"
)

cld(
  monmouth_soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)



############################################################
# ALTAMONT STAND COUNT ANALYSIS
# Crops: Corn and Soybean
# Response: Mean of stand1 and stand2 within each plot
############################################################


############################################################
# 1. FILTER ALTAMONT LOCATION
############################################################

mahadi_altamont <- mahadi.df1 |>
  dplyr::filter(location == "Altamont")

View(mahadi_altamont)
nrow(mahadi_altamont)


############################################################
# 2. CONVERT STAND1 AND STAND2 TO LONG FORMAT
############################################################

mahadi_altamont_long <- mahadi_altamont |>
  tidyr::pivot_longer(
    cols = c(stand1, stand2),
    names_to = "stand",
    values_to = "stand_count"
  )


############################################################
# 3. AVERAGE STAND1 AND STAND2 WITHIN EACH PLOT
############################################################

mahadi_altamont_long_avg <- mahadi_altamont_long |>
  dplyr::group_by(
    plot_id,
    crop,
    location,
    rep,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    stand_mean = mean(
      stand_count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


############################################################
# 4. CHECK REPLICATION
############################################################

mahadi_altamont_long_avg |>
  dplyr::count(
    crop,
    tillage,
    covercrop
  )


############################################################
# 5. CALCULATE TREATMENT MEANS AND STANDARD ERRORS
############################################################

mahadi_plot_altamont_summary <- mahadi_altamont_long_avg |>
  dplyr::group_by(
    crop,
    tillage,
    covercrop
  ) |>
  dplyr::summarise(
    n = sum(!is.na(stand_mean)),
    
    mean_stand = mean(
      stand_mean,
      na.rm = TRUE
    ),
    
    sd_stand = sd(
      stand_mean,
      na.rm = TRUE
    ),
    
    se_stand = sd_stand / sqrt(n),
    
    .groups = "drop"
  )


############################################################
# 6. PLOT MEAN STAND COUNT ± SE
############################################################

altamont_count <- ggplot(
  mahadi_plot_altamont_summary,
  aes(
    x = covercrop,
    y = mean_stand
  )
) +
  
  geom_col(
    width = 0.70
  ) +
  
  geom_errorbar(
    aes(
      ymin = mean_stand - se_stand,
      ymax = mean_stand + se_stand
    ),
    width = 0.20
  ) +
  
  facet_grid(
    crop ~ tillage,
    scales = "free_y"
  ) +
  
  labs(
    x = "Cover Crop",
    y = "Mean Stand Count"
  ) +
  
  theme_classic() +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    )
  )


altamont_count


############################################################
# 7. SAVE ALTAMONT FIGURE
############################################################

ggsave(
  filename = "plot/altamont_count.png",
  plot = altamont_count,
  width = 10,
  height = 6,
  dpi = 300
)


############################################################
# 8. SEPARATE CORN AND SOYBEAN
############################################################

altamont_corn <- mahadi_altamont_long_avg |>
  dplyr::filter(crop == "corn")

altamont_soybean <- mahadi_altamont_long_avg |>
  dplyr::filter(crop == "soybean")


############################################################
# 9. ALTAMONT CORN MODEL
############################################################

altamont_corn_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = altamont_corn
)


############################################################
# 10. ALTAMONT CORN ANOVA
############################################################

anova(altamont_corn_model)


############################################################
# 11. CHECK CORN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(altamont_corn_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(altamont_corn_model)
)


############################################################
# 12. CORN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

altamont_corn_tillage_emm <- emmeans(
  altamont_corn_model,
  ~ tillage
)

altamont_corn_tillage_emm

pairs(
  altamont_corn_tillage_emm,
  adjust = "tukey"
)

cld(
  altamont_corn_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 13. CORN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

altamont_corn_covercrop_emm <- emmeans(
  altamont_corn_model,
  ~ covercrop
)

altamont_corn_covercrop_emm

pairs(
  altamont_corn_covercrop_emm,
  adjust = "tukey"
)

cld(
  altamont_corn_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 14. ALTAMONT SOYBEAN MODEL
############################################################

altamont_soybean_model <- lm(
  stand_mean ~ rep + tillage * covercrop,
  data = altamont_soybean
)


############################################################
# 15. ALTAMONT SOYBEAN ANOVA
############################################################

anova(altamont_soybean_model)


############################################################
# 16. CHECK SOYBEAN MODEL ASSUMPTIONS
############################################################

par(mfrow = c(2, 2))
plot(altamont_soybean_model)

par(mfrow = c(1, 1))

shapiro.test(
  residuals(altamont_soybean_model)
)


############################################################
# 17. SOYBEAN TILLAGE MAIN-EFFECT COMPARISONS
############################################################

altamont_soybean_tillage_emm <- emmeans(
  altamont_soybean_model,
  ~ tillage
)

altamont_soybean_tillage_emm

pairs(
  altamont_soybean_tillage_emm,
  adjust = "tukey"
)

cld(
  altamont_soybean_tillage_emm,
  adjust = "tukey",
  Letters = letters
)


############################################################
# 18. SOYBEAN COVER-CROP MAIN-EFFECT COMPARISONS
############################################################

altamont_soybean_covercrop_emm <- emmeans(
  altamont_soybean_model,
  ~ covercrop
)

altamont_soybean_covercrop_emm

pairs(
  altamont_soybean_covercrop_emm,
  adjust = "tukey"
)

cld(
  altamont_soybean_covercrop_emm,
  adjust = "tukey",
  Letters = letters
)
