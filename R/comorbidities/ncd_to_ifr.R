# ---------------------------------------------------------------------------- #
#' Proccessing NCD data from IHME     
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(data.table)
library(cowplot)

ncds <- fread("data/raw/IHME_ageNCDs/IHME-GBD_2017_DATA-f8d93ddd-1.csv")
iso_codes <- fread("output/iso_codes.csv")
ifr_ests <- fread("data/raw/ifrs_from_lit.csv")

# Fit age to IFR estimates ------------------------------------------------
ifr_ests$age_mid <- (ifr_ests$age_lower + ifr_ests$age_upper)/2
ifr_ests$ifr_prop_est <- ifr_ests$ifr_perc_est/100

library(mgcv)

ifr_fit <- gam(ifr_prop_est ~ s(age_mid), family = betar(link="logit"), 
               data = ifr_ests, method = "REML")
ages <- seq(0, 99, by = 1)

ifr_fit <- gam(ifr_prop_est ~ s(age_mid) + country_name, family = betar(link="logit"), 
               data = ifr_ests, method = "REML")

ifr_preds <- predict(ifr_fit, 
                     data.frame(age_mid = rep(ages, 3), 
                                country_name = rep(c("Italy", "France", "China"),
                                                   each = length(ages))), type = "response", se.fit = TRUE)
ifrs_to_plot <- data.frame(ages = rep(ages, 3), 
                           country_name = rep(c("Italy", "France", "China"),
                                              each = length(ages)),
                           est = ifr_preds$fit, upper = ifr_preds$fit + 2*ifr_preds$se.fit, 
                           lower = ifr_preds$fit - 2*ifr_preds$se.fit)

ggplot(data = ifrs_to_plot, aes(x = ages, color = country_name)) + 
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100, fill = country_name), alpha = 0.5) +
  geom_line(aes(y = est*100)) +
  geom_point(data = ifr_ests, 
             aes(x = age_mid, y = ifr_perc_est, fill = country_name, shape = country_name), 
             size = 2, color = "black") +
  scale_color_brewer(aesthetics = c("fill", "color"), palette = "Set2", name = "Country") +
  scale_shape_manual(values = c(21, 22, 23),name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Age (years)", y = "Infection fatality ratio (%)")

# Fit ncd mortality/prevalence to ifr ests --------------------------------
# Get deaths for the 4 causes by age
ncds %>%
  filter(location_name %in% ifr_ests$country_name, 
         age_name != "Age-standardized", measure_name == "Prevalence") %>%
  group_by(location_name, age_name) %>%
  summarize(mean_val = mean(val), mean_upper = mean(upper), 
            mean_lower = mean(lower)) %>%
  mutate(age_lower = sapply(strsplit(age_name, " "), 
                          function (x) as.numeric(unlist(x)[1])),
         age_upper = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[3])),
         age_upper = case_when(age_lower == 80 ~ 99, 
                               age_lower != 80 ~ age_upper)) %>%
  ungroup() %>% 
  mutate(age_bin = pmap_chr(list(age_lower, age_upper, location_name), 
       function(first, second, third) {
         ifr_ests$age_bin[which(first >= ifr_ests$age_lower & 
                                  second <= ifr_ests$age_upper & 
                                  third == ifr_ests$country_name)]
        })) %>%
  group_by(location_name, age_bin) %>%
  summarize_at(vars(starts_with("mean")), mean) %>%
  right_join(ifr_ests, by = c("location_name" = "country_name", 
                              "age_bin" = "age_bin")) -> ncds_mean_prev

ncds %>%
  filter(location_name %in% ifr_ests$country_name, 
         age_name != "Age-standardized", measure_name == "Deaths") %>%
  group_by(location_name, age_name) %>%
  summarize(sum_val = sum(val), sum_upper = sum(upper), 
            sum_lower = sum(lower)) %>%
  mutate(age_lower = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[1])),
         age_upper = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[3])),
         age_upper = case_when(age_lower == 80 ~ 99, 
                               age_lower != 80 ~ age_upper)) %>%
  ungroup() %>% 
  mutate(age_bin = pmap_chr(list(age_lower, age_upper, location_name), 
                            function(first, second, third) {
                              ifr_ests$age_bin[which(first >= ifr_ests$age_lower & 
                                                       second <= ifr_ests$age_upper & 
                                                       third == ifr_ests$country_name)]
                            })) %>%
  group_by(location_name, age_bin) %>%
  summarize_at(vars(starts_with("sum")), sum) %>%
  right_join(ifr_ests, by = c("location_name" = "country_name", 
                              "age_bin" = "age_bin")) -> ncds_sum_death

ifr_fit <- gam(ifr_prop_est ~ s(mean_val) + location_name, family = betar(link="logit"), 
               data = ncds_mean_prev, method = "REML")

ncd_death_seq <- seq(min(ncds_mean_prev$mean_val), max(ncds_mean_prev$mean_val), length.out = 100)

ifr_preds <- predict(ifr_fit, 
                     data.frame(mean_val = rep(ncd_death_seq, 3), 
                                location_name = rep(c("Italy", "France", "China"),
                                                   each = length(ncd_death_seq))), 
                     type = "response", se.fit = TRUE)

ifrs_to_plot <- data.frame(ncd_deaths = rep(ncd_death_seq, 3), 
                           country_name = rep(c("Italy", "France", "China"),
                                              each = length(ncd_death_seq)),
                           est = ifr_preds$fit, upper = ifr_preds$fit + 2*ifr_preds$se.fit, 
                           lower = ifr_preds$fit - 2*ifr_preds$se.fit)

ggplot(data = ifrs_to_plot, aes(x = ncd_deaths, color = country_name)) + 
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100, fill = country_name), alpha = 0.5) +
  geom_line(aes(y = est*100)) +
  geom_point(data = ncds_mean_prev, 
             aes(x = mean_val, y = ifr_perc_est, fill = location_name, shape = location_name), 
             size = 2, color = "black") +
  scale_color_brewer(aesthetics = c("fill", "color"), palette = "Set2", name = "Country") +
  scale_shape_manual(values = c(21, 22, 23),name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Mean prevalence of target NCDs \n per 100,000 persons", 
       y = "Infection fatality ratio (%)")


ifr_fit <- gam(ifr_prop_est ~ s(mean_val), family = betar(link="logit"), 
               data = ncds_mean_prev, method = "REML")

ncd_death_seq <- seq(min(ncds_mean_prev$mean_val), max(ncds_mean_prev$mean_val), length.out = 100)

ifr_preds <- predict(ifr_fit, 
                     data.frame(mean_val = ncd_death_seq), 
                     type = "response", se.fit = TRUE)

ifrs_to_plot <- data.frame(ncd_deaths = ncd_death_seq, 
                           est = ifr_preds$fit, upper = ifr_preds$fit + 2*ifr_preds$se.fit, 
                           lower = ifr_preds$fit - 2*ifr_preds$se.fit)

ggplot(data = ifrs_to_plot, aes(x = ncd_deaths)) + 
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100), alpha = 0.5) +
  geom_line(aes(y = est*100)) +
  geom_point(data = ncds_mean_prev, 
             aes(x = mean_val, y = ifr_perc_est, fill = location_name, shape = location_name), 
             size = 2, color = "black") +
  scale_color_brewer(aesthetics = c("fill", "color"), palette = "Set2", name = "Country") +
  scale_shape_manual(values = c(21, 22, 23),name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Mean prevalence of target NCDs \n per 100,000 persons", 
       y = "Infection fatality ratio (%)")

ggplot(data = ncds_mean_prev, aes(x = age_mid, y = mean_val)) +
  geom_pointrange(aes(ymin = mean_lower, ymax = mean_upper,
                      fill = location_name, shape = location_name), 
                  fatten = 4, color = "black", alpha = 1,
                  position = position_dodge(width = 0.2)) +
  scale_color_brewer(aesthetics = c("fill", "color"), palette = "Set2", name = "Country") +
  scale_shape_manual(values = c(21, 22, 23),name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Mean prevalence of target NCDs \n per 100,000 persons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Translate to ifr estimates by country --------------------------------
ihme_countries <- unique(ncds$location_name)
iso_codes$ihme_countries <- ihme_countries[apply(adist(iso_codes$country, 
                                                       ihme_countries, 
                                                       partial = FALSE), 1, 
                                                 which.min)]
# Manual fixes
iso_codes %>%
  case_when(ihme_countries == "Gambia", "The Gambia", 
            country %in% c("Mayotte", "Saint Helena", "Reunion", 
                           "Western Sahara") ~ NA, 
            !(country %in% c("Mayotte",
                             "Saint Helena", "Reunion", "Western Sahara", 
                             "Gambia")) ~ ihme_countries) -> iso_codes

ncds %>%
  filter(location_name %in% iso_codes$ihme_countries | location_name %in% c("Italy", "China", "France"), 
         age_name != "Age-standardized", measure_name == "Prevalence") %>%
  group_by(location_name, age_name) %>%
  summarize(mean_val = mean(val), mean_upper = mean(upper), 
            mean_lower = mean(lower)) %>%
  ungroup() %>%
  mutate(age_lower = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[1])),
         age_upper = sapply(strsplit(age_name, " "), 
                            function (x) as.numeric(unlist(x)[3])),
         age_upper = case_when(age_lower == 80 ~ 99, 
                               age_lower != 80 ~ age_upper), 
         age_mid = (age_upper + age_lower)/2,
         loc_type = case_when(location_name %in% iso_codes$ihme_countries ~ "SSA",
                                   location_name %in% c("Italy", "China", "France") ~ location_name)) -> ncds_mean_prev


ggplot(data = ncds_mean_prev, aes(x = age_mid, y = mean_val, group = location_name)) +
  geom_line(aes(color = loc_type)) +
  scale_color_manual(values = c("blue", "red", "purple", alpha("grey", 0.25)),
                     name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Mean prevalence of target NCDs \n per 100,000 persons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = ncds_mean_prev, aes(x = reorder(age_name, age_mid), 
                                  y = mean_val, group = location_name)) +
  geom_pointrange(aes(ymin = mean_lower, ymax = mean_upper,
                      color = loc_type), fatten = 4,
                  position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("blue", "red", "purple", alpha("grey", 0.25)),
                     name = "Country") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Mean prevalence of target NCDs \n per 100,000 persons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Do the countries to compare have data for each metric? (Add iso codes to this table!)


# Subtract out asthma from respiratory diseases

