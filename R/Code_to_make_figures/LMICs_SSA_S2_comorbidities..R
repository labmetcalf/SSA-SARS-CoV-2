# ---------------------------------------------------------------------------- #
#' Figure S3: NCDS vs. Age   
# ---------------------------------------------------------------------------- #

# Packages
library(tidyverse)
library(cowplot)
library(patchwork)
library(mgcv)
library(here)

# Data
iso_codes <- read_csv(here("data/Data/iso_codes.csv"))
ncds <- read_csv(here("data/processed/ihme_cleaned.csv"))
ssa_countries <- read_csv(here("data/processed/ssa_countries.csv"))

# Fit ncd mortality/prevalence to ifr ests --------------------------------
ncds %>%
  mutate(age_lower = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[1])),
         age_upper = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[3])),
         age_upper = case_when(age_lower == 80 ~ 100, 
                               age_lower != 80 ~ age_upper),
         age_mid = (age_upper + age_lower)/2,
         loc_type = case_when(!(location_name %in% c("Italy", "China", "France")) ~ "SSA",
                              location_name %in% c("Italy", "China", "France") ~ location_name)) -> ncds

# Country colors
country_cols <- c("#1b9e77", "#d95f02", "#7570b3")
names(country_cols) <- c("Italy", "China", "France")

# Supplementary Figure: look at deaths & prev for the 4 main ones  ----------------------------
ncds %>%
  pivot_longer(`Diabetes mellitus`:`Chronic respiratory diseases (-Asthma)`, 
               names_to = "cause") -> ncds_long

ggplot(data = filter(ncds_long, measure_name == "Deaths" & cause != "Neoplasms"), 
       aes(x = reorder(age_name, age_mid), 
           y = value, group = location_name)) +
  geom_line(aes(color = loc_type, alpha = loc_type)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "grey"),
                     name = "Location") +
  scale_alpha_manual(values = c(1, 1, 1, 0.25), guide = "none") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Annual deaths attributable \n per 100,000 persons") +
  facet_wrap(~ cause, scales = "free", labeller = labeller(cause = label_wrap_gen(30)),
             ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) -> sfig_ncds_age

# comparisons
ncds_long %>%
  filter(location_name %in% c("China", "France", "Italy"), measure_name == "Deaths") %>%
  select(location_name, age_name, mean_100k, cause) %>%
  pivot_wider(names_from = location_name, values_from = mean_100k) %>%
  left_join(filter(ncds_long, measure_name == "Deaths", iso %in% ssa_countries$iso),
            by = c("age_name" = "age_name", "cause" = "cause")) %>%
  mutate(China = if_else(mean_100k <= China, 0, 1), 
         France = if_else(mean_100k <= China, 0, 1), 
         Italy = if_else(mean_100k <= China, 0, 1)) %>%
  group_by(cause, age_name) %>%
  summarize(China = sum(China), France = sum(France), Italy = sum(Italy)) %>%
  filter(age_name == "50 to 54") -> compare_deaths_at_50

# save them
ggsave(here("Figures/Fig S2 20200727.pdf"), sfig_ncds_age, device = "jpeg", height = 8, width = 5)

