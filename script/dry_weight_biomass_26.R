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
mis_data<-cc_biomass %>%
  group_by(location, covercrop) %>%
  summarise(
    Total_Rows = n(),
    Missing = sum(is.na(`net_biomass_kg_ha`)),
    Missing_Percent = round(
      100 * mean(is.na(`net_biomass_kg_ha`)), 2
    ),
    .groups = "drop"
  )


#initial visualization

biomass_plot <- cc_biomass %>%
  filter(location != "Altamont",
    year == 2026,
    !is.na(`net_biomass_kg_ha`)
  ) %>%
  mutate(
    biomass = `net_biomass_kg_ha`,
    tillage = factor(tillage, levels = c("NT", "SR", "ST")),
    covercrop = factor(
      covercrop,
      levels = c("None", "Single", "Mix 1", "Mix 2")
    ),
    crop = factor(
      tolower(as.character(crop)),
      levels = c("corn", "soybean")
    )
  )



p1 <- ggplot(
  biomass_plot,
  aes(
    x = tillage,
    y = biomass,
    fill = covercrop
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.85),
    width = 0.75,
    alpha = 0.75,
    outlier.shape = NA
  ) +
  geom_point(
    aes(group = covercrop),
    position = position_jitterdodge(
      jitter.width = 0.12,
      dodge.width = 0.85
    ),
    size = 1.5,
    alpha = 0.55
  ) +
  facet_grid(crop ~ location) +
  scale_fill_manual(
    values = c(
      "None" = "#999999",
      "Single" = "#009E73",
      "Mix 1" = "#E69F00",
      "Mix 2" = "#0072B2"
    )
  ) +
  labs(
    title = "Cover Crop Biomass by Tillage and Cover Crop Treatment",
    subtitle = "2026 growing season",
    x = "Tillage Practice",
    y = expression("Biomass (kg ha"^{-1}*")"),
    fill = "Cover Crop"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(p1)




