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


###1. data load####
cc_analysis<-read_excel("data/joined_all_site_biomass_result_cc_mgnt_26.xlsx")


######check data####

# Check column names and structure
names(cc_analysis)
str(cc_analysis)

# Check crop levels
unique(cc_analysis$crop)
length(unique(cc_analysis$crop))

unique(cc_analysis$location)


#####variables into factor#####
cc_analysis<- cc_analysis %>% 
  mutate(plot_id = factor(plot_id),
         crop= factor(crop),
         location= factor(location),
         tillage = factor(tillage),
         covercrop = factor(covercrop),
         rep= factor(rep),
         
  )


### 3. conversion

