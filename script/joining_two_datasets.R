install.packages("writexl")
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
library(writexl)


#load data

template<- read_excel("data/template_all_site_crop_treatment.xlsx")
biomass<- read_excel("data/all_site_biomass_result_cc_mgnt_26.xlsx")


#join data
joined_data <- left_join(
  template,
  biomass,
  by = c("plot_id", "crop", "location")
)

joined_data %>%
  count(covercrop, is.na(N_percent_dm))

write_xlsx(joined_data, "data/joined_all_site_biomass_result_cc_mgnt_26.xlsx")


