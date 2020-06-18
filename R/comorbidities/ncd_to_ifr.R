# ---------------------------------------------------------------------------- #
#' Fit gams to ncds & age & compare     
# ---------------------------------------------------------------------------- #

# Packages
library(tidyverse)
library(cowplot)
library(patchwork)
library(mgcv)
library(here)

# Data
ifr_ests <- read_csv(here("data/raw/ifrs_from_lit.csv"))
un_ages <- read_csv(here("data/processed/un_ages.csv"))
iso_codes <- read_csv(here("output/iso_codes.csv"))
ncds <- read_csv(here("data/processed/ihme_cleaned.csv"))

# Fit age to IFR estimates ------------------------------------------------
ifr_ests$predictor <- (ifr_ests$age_lower + ifr_ests$age_upper)/2
ifr_ests$ifr_prop_est <- ifr_ests$ifr_perc_est/100

ifr_fit_age <- gam(ifr_prop_est ~ s(predictor), family = betar(link="logit"), 
               data = ifr_ests, method = "REML")

ages <- seq(0, 100, by = 1)
ifr_preds_age <- predict(ifr_fit_age,  data.frame(predictor = ages), 
                     type = "response", se.fit = TRUE)
ifrs_to_plot_age <- data.frame(ages = ages, 
                           est = ifr_preds_age$fit, upper = ifr_preds_age$fit + 2*ifr_preds_age$se.fit, 
                           lower = ifr_preds_age$fit - 2*ifr_preds_age$se.fit)

# Fit age to hosp estimates ------------------------------------------------
# Have to choose k in smoothing term because not many data points
# Look into other papers to see if they have age-stratified ests
hosp_ests <- read_csv(here("data/raw/hosp_from_salje.csv"))
hosp_ests %>%
  mutate(predictor = (age_lower + age_upper)/2, hosp_rate = inf_to_hosp_mean/100, 
         icu_rate = (inf_to_hosp_mean/100)*(hosp_to_icu_mean/100)) -> hosp_ests

# IFR equal to hospitalization rate
hosp_fit_age <- gam(hosp_rate ~ s(predictor, k = 8), family = betar(link="logit"), 
                    data = hosp_ests, method = "REML")

# IFR equal to icu rate
icu_fit_age <- gam(icu_rate ~ s(predictor, k = 8), family = betar(link="logit"), 
                   data = hosp_ests, method = "REML")

# Make middle panel plot ------------------------------------------------
predict_ifr <- function(predictor, gam = ifr_fit_age) {
  predict(gam, newdata = data.frame(predictor = predictor), type = "response")
}

age_ifrs <- data.frame(age = seq(1, 100, by = 1))
age_ifrs %>%
  mutate(ifr_by_age = predict_ifr(predictor = age, gam = ifr_fit_age), 
         ifr_by_ageplus5 = predict_ifr(predictor = age + 5, gam = ifr_fit_age),
         ifr_by_ageplus10 = predict_ifr(predictor = age + 10, gam = ifr_fit_age), 
         ifr_by_hosp = predict_ifr(predictor = age, gam = hosp_fit_age),
         ifr_by_icu = predict_ifr(predictor = age, gam = icu_fit_age)) %>%
  pivot_longer(starts_with("ifr"), names_to = "ifr_type", values_to = "ifr_est") -> ages_to_plot

un_ages %>%
  filter(iso %in% iso_codes$iso) %>%
  mutate(ifr_by_age = predict_ifr(predictor = age, gam = ifr_fit_age), 
         ifr_by_ageplus5 = predict_ifr(predictor = age + 5, gam = ifr_fit_age),
         ifr_by_ageplus10 = predict_ifr(predictor = age + 10, gam = ifr_fit_age), 
         ifr_by_hosp = predict_ifr(predictor = age, gam = hosp_fit_age),
         ifr_by_icu = predict_ifr(predictor = age, gam = icu_fit_age), 
         pop_over_60 = ifelse(age > 60, pop, 0)) %>%
  pivot_longer(starts_with("ifr"), names_to = "ifr_type", values_to = "ifr_est") %>%
  mutate(burden = ifr_est*pop*0.2) %>%
  group_by(country, iso, ifr_type) %>%
  summarize(burden_age = sum(burden), 
            pop = sum(pop), pop_over_60 = sum(pop_over_60)) %>%
  mutate(prop_over_60 = pop_over_60/pop) -> burden_by_age

ifr_cols <- c("ifr_by_age" = "#1b9e77", "ifr_by_ageplus5" = "#d95f02", "ifr_by_ageplus10" = "#7570b3", 
              "ifr_by_icu" = "#e7298a", "ifr_by_hosp" = "#66a61e")
ifr_labs <- c("Age (baseline)", "Age, shifted + 5", "Age, shifted + 10", "IFR = ICU rate by age", 
              "IFR = Hospitalization rate by age")
names(ifr_labs) <- names(ifr_cols)

ggplot(data = ages_to_plot, aes(x = age, y = ifr_est, color = ifr_type)) +
  geom_line(size = 1.2) + 
  scale_color_manual(values = ifr_cols, labels = ifr_labs, guide = "none") +
  theme_minimal_hgrid() +
  labs(x = "Age", y = "IFR") +
  theme(text = element_text(size = 12)) -> ifr_leg

ggsave("figs/fig4_middle_inset.jpeg", height = 5, width = 5)

ggplot(data = burden_by_age, aes(x = reorder(country, prop_over_60))) +
  geom_point(aes(y = burden_age/pop*1e5, color = ifr_type)) +
  coord_flip() +
  scale_color_manual(values = ifr_cols, labels = ifr_labs,
                     name = "Predictor of IFR") +
  theme_minimal_hgrid() +
  labs(x = "Countries (ordered by age)", y = "Incidence of deaths \n per 100,000 persons", tag = "?") +
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12)) -> middle_panel

ggsave("figs/fig4_middle_exe.jpeg", height = 7, width = 7)

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

# Get deaths for the 4 causes by age and match to age bins in studies
ncds %>%
  filter(measure_name == "Prevalence", location_name %in% c("Italy", "France", "China")) %>%
  mutate(age_bin = pmap_chr(list(age_lower, age_upper, location_name), 
       function(first, second, third) {
         ifr_ests$age_bin[which(first >= ifr_ests$age_lower & 
                                  second <= ifr_ests$age_upper & 
                                  third == ifr_ests$country_name)]
        })) %>%
  group_by(location_name, iso, age_bin) %>%
  summarize(predictor = mean(mean_100k)) %>%
  right_join(select(ifr_ests, -predictor), 
             by = c("location_name" = "country_name", 
                              "age_bin" = "age_bin")) -> ncds_mean_prev

ifr_fit_ncd <- gam(ifr_prop_est ~ s(predictor), family = betar(link="logit"), 
               data = ncds_mean_prev, method = "REML")

ncd_prev_seq <- seq(min(ncds_mean_prev$predictor), max(ncds_mean_prev$predictor), length.out = 100)

ifr_preds_ncd <- predict(ifr_fit_ncd, 
                     data.frame(predictor = ncd_prev_seq),  
                     type = "response", se.fit = TRUE)

ifrs_to_plot_ncd <- data.frame(ncd_prev = ncd_prev_seq,  
                           est = ifr_preds_ncd$fit, upper = ifr_preds_ncd$fit + 2*ifr_preds_ncd$se.fit, 
                           lower = ifr_preds_ncd$fit - 2*ifr_preds_ncd$se.fit)

# Translate to ifr estimates by country --------------------------------
predict_ifr <- function(predictor, gam = ifr_fit_age) {
  predict(gam, newdata = data.frame(predictor = predictor), type = "response")
}

un_ages %>%
  filter(iso %in% iso_codes$iso) %>%
  mutate(ifr_est = predict_ifr(predictor = age, gam = ifr_fit_age), 
         burden = pop*0.20*ifr_est) %>%
  group_by(country, iso) %>%
  summarize(burden_age = sum(burden), 
            pop = sum(pop)) -> burden_by_age

ncds %>%
  select(age_name, age_lower, age_upper) %>%
  group_by(age_name) %>%
  distinct() -> age_bins

un_ages %>%
  mutate(age_name = pmap_chr(list(age = age), function(age) {
                              if(age == 0) {
                                return("1 to 4")
                              } else {
                                return(age_bins$age_name[which(age >= age_bins$age_lower & 
                                                          age <= age_bins$age_upper)])
                              }
                            })) %>%
  group_by(age_name, country, iso) %>%
  summarize(pop = sum(pop)) -> un_pops

ncds %>%
  left_join(un_pops) %>%
  filter(iso %in% iso_codes$iso, measure_name == "Prevalence") %>%
  mutate(ifr_est = predict_ifr(predictor = mean_100k, gam = ifr_fit_ncd), 
         burden = pop*0.20*ifr_est) %>%
  group_by(iso) %>%
  summarize(burden_ncd = sum(burden)) %>%
  left_join(burden_by_age) %>%
  filter(!is.na(country)) %>%
  mutate(country = gsub("CIte d'Ivoire", "Cote d'Ivoire", country)) -> burden_all

burden_all %>%
  pivot_longer(c("burden_ncd", "burden_age")) -> burden_long

# Put it all together in a figure --------------------------------

# Country colors
country_cols <- c("#1b9e77", "#d95f02", "#7570b3")
names(country_cols) <- c("Italy", "China", "France")

# Age vs IFR
ggplot(data = ifrs_to_plot_age, aes(x = ages)) + 
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100), alpha = 0.5) +
  geom_line(aes(y = est*100)) +
  geom_line(data = ifr_ests, aes(x = predictor, y = ifr_perc_est,
                                 color = country_name), alpha = 0, size = 1) + # Dummy for legend
  geom_hline(aes(linetype = "SSA", yintercept = 0), alpha = 0, color = "grey", 
             show.legend = TRUE) +
  geom_hline(aes(linetype = "GAM fit", yintercept = 0), alpha = 0, color = "black", 
             show.legend = TRUE) +
  geom_point(data = ifr_ests, 
             aes(x = predictor, y = ifr_perc_est, fill = country_name, shape = country_name), 
             size = 2.5, color = "black") +
  scale_fill_manual(aesthetics = c("color", "fill"), values = country_cols, 
                    name = "") + 
  scale_shape_manual(values = c(21, 22, 23), name = "") +
  scale_linetype(name = "") +
  theme_minimal_hgrid() +
  labs(x = "Age (years)", y = "Infection fatality ratio (%)", tag = "A") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         linetype = guide_legend(override.aes = list(color = c("black", "grey"), 
                                                     alpha = 1, linetype = 1))) +
  theme(text = element_text(size = 12)) -> ncd_fig_A

# NCDs vs Age by country
ncds %>%
  filter(measure_name == "Prevalence") -> ncds_mean_all

ggplot() +
  geom_line(data = filter(ncds_mean_all, loc_type == "SSA"), 
            aes(x = age_mid, y = mean_100k, group = location_name), color = "grey", alpha = 0.25, 
            size = 1) +
  geom_line(data = filter(ncds_mean_all, loc_type != "SSA"), 
            aes(x = age_mid, y = mean_100k, group = location_name, color = location_name),
            alpha = 1, size = 1) +
  scale_color_manual(values = country_cols,
                     guide = "none") +
  scale_alpha_manual(values = c(1, 1, 1, 0.25), guide = "none") +
  theme_minimal_hgrid() +
  labs(x = "Age (midpoint of bracket)", 
       y = str_wrap("Mean prevalence of target NCDs per 100,000 persons", 25), tag = "B") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12)) -> ncd_fig_B

# NCDs vs IFR
ggplot(data = ifrs_to_plot_ncd, aes(x = ncd_prev)) + 
  geom_ribbon(aes(ymin = lower*100, ymax = upper*100), alpha = 0.5) +
  geom_line(aes(y = est*100)) +
  geom_point(data = ncds_mean_prev, 
             aes(x = predictor, y = ifr_perc_est, fill = location_name, shape = location_name), 
             size = 2, color = "black") +
  scale_color_manual(aesthetics = c("fill", "color"), values = country_cols, 
                     guide = "none") +
  scale_shape_manual(values = c(21, 22, 23), guide = "none") +
  theme_minimal_hgrid() +
  labs(x = str_wrap("Mean prevalence of target NCDs per 100,000 persons", 25), 
       y = "Infection fatality ratio (%)", tag = "C") +
  theme(text = element_text(size = 12)) -> ncd_fig_C

ggplot(data = burden_all, aes(x = reorder(country, burden_age/pop*1e5))) +
  geom_linerange(aes(ymax = burden_ncd/pop*1e5, ymin = burden_age/pop*1e5)) +
  geom_point(data = burden_long, aes(x = country, y = value/pop*1e5, color = name)) +
  coord_flip() +
  scale_color_manual(values = c("#e7298a", "#66a61e"), 
                     labels = c("IFR by age", "IFR by NCD prevalence"), 
                     name = "Predictor of burden") +
  theme_minimal_hgrid() +
  labs(x = "Countries (ordered by age)", y = "Incidence of deaths \n per 100,000 persons", tag = "D") +
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12)) -> ncd_fig_D

ncd_fig <- 
  (((ncd_fig_A | ncd_fig_B | ncd_fig_C) + plot_layout(guides = "collect")) / 
     cowplot::as_grob(ncd_fig_D)) + 
  plot_layout(heights = c(1, 2))

ggsave(here("figs/main/ncds_age.jpeg"), ncd_fig, width = 8, height = 10)

# Supplementary Figure: look at deaths & prev for the 4 main ones  ----------------------------
ncds %>%
  pivot_longer(`Diabetes mellitus`:`Chronic respiratory diseases (-Asthma)`, 
               names_to = "cause") -> ncds_long

ggplot(data = filter(ncds_long, measure_name == "Prevalence"), 
       aes(x = reorder(age_name, age_mid), 
           y = value, group = location_name)) +
  geom_line(aes(color = loc_type, alpha = loc_type)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "grey"),
                     guide = "none") +
  scale_alpha_manual(values = c(1, 1, 1, 0.25), guide = "none") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Prevalence \n per 100,000 persons", tag = "A") +
  facet_grid(cause ~ measure_name, scales = "free", labeller = labeller(cause = label_wrap_gen(10))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text.y = element_blank(), strip.text.x = element_blank()) -> sfig_ncds_age_A


ggplot(data = filter(ncds_long, measure_name == "Deaths"), 
       aes(x = reorder(age_name, age_mid), 
           y = value, group = location_name)) +
  geom_line(aes(color = loc_type, alpha = loc_type)) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "grey"),
                     name = "Location") +
  scale_alpha_manual(values = c(1, 1, 1, 0.25), guide = "none") +
  theme_minimal_hgrid() +
  labs(x = "Age", 
       y = "Annual deaths attributable \n per 100,000 persons", tag = "B") +
  facet_grid(cause ~ measure_name, scales = "free", labeller = labeller(cause = label_wrap_gen(10))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
        strip.text.x = element_blank()) -> sfig_ncds_age_B

sfig_ncds_age <- sfig_ncds_age_A | sfig_ncds_age_B

ggsave(here("figs/supplement/SX_ncds_age.jpeg"), sfig_ncds_age, device = "jpeg", height = 8, width = 8)

