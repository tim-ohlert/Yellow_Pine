library(tidyverse)
library(ggthemes)



annual_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/annual_belt_with_zone.csv")



perennial_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/perrenial_belt_with_zone.csv")


all_metrics <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/all_metrics_by_zone.csv")




metrics_summ <- all_metrics %>%
  group_by(year, location) %>%
  dplyr::summarize(
    perennial_dens = mean(perennial_belt__density_indiv_m2, na.rm = TRUE),
    perennial_se   = sd(perennial_belt__density_indiv_m2, na.rm = TRUE) /
      sqrt(sum(!is.na(perennial_belt__density_indiv_m2))),
    annual_dens    = mean(annual_belt__density_indiv_m2, na.rm = TRUE),
    annual_se      = sd(annual_belt__density_indiv_m2, na.rm = TRUE) /
      sqrt(sum(!is.na(annual_belt__density_indiv_m2))),
    .groups = "drop"
  )


metrics_long <- metrics_summ %>%
  select(year, location,
         perennial_dens, perennial_se,
         annual_dens, annual_se) %>%
  pivot_longer(
    cols = -c(year, location),
    names_to = c("life_history", ".value"),
    names_pattern = "(perennial|annual)_(dens|se)"
  )




ggplot(  metrics_long,aes(x = as.factor(year),    y = dens,color = location,shape = life_history,    group = interaction(location, life_history))
) +
  facet_wrap(~life_history, scales = "free_y")+
  geom_point() +
  geom_line() +
  geom_errorbar(
    aes(ymin = dens - se, ymax = dens + se),
    width = 0.15
  ) +
  labs(
    x = "Year",
    y = expression("Density (indiv " * m^-2 * ")"),
    color = "Location",
    shape = "Life history"
  ) +
  theme_base()






metrics_summ2 <- all_metrics %>%
  group_by(year, location, zone) %>%
  dplyr::summarize(
    perennial_dens = mean(perennial_belt__density_indiv_m2, na.rm = TRUE),
    perennial_se   = sd(perennial_belt__density_indiv_m2, na.rm = TRUE) /
      sqrt(sum(!is.na(perennial_belt__density_indiv_m2))),
    annual_dens    = mean(annual_belt__density_indiv_m2, na.rm = TRUE),
    annual_se      = sd(annual_belt__density_indiv_m2, na.rm = TRUE) /
      sqrt(sum(!is.na(annual_belt__density_indiv_m2))),
    .groups = "drop"
  )


metrics_long2 <- metrics_summ2 %>%
  select(year, location,zone,
         perennial_dens, perennial_se,
         annual_dens, annual_se) %>%
  pivot_longer(
    cols = -c(year, location, zone),
    names_to = c("life_history", ".value"),
    names_pattern = "(perennial|annual)_(dens|se)"
  )


ggplot(  metrics_long2,aes(x = as.factor(year),    y = dens,color = location,shape = life_history,    group = zone)
) +
  facet_grid(life_history~zone, scales = "free_y")+
  geom_point() +
  #geom_line() +
  geom_errorbar(
    aes(ymin = dens - se, ymax = dens + se),
    width = 0.15
  ) +
  labs(
    x = "Year",
    y = expression("Density (indiv " * m^-2 * ")"),
    color = "Location",
    shape = "Life history"
  ) +
  theme_base()







###########
###Beta diversity



zone_template <- annual_belt %>%
  filter(zone != "never_under") %>%
  distinct(transect, year, zone, seg_start_cm, seg_end_cm) %>%
  mutate(zone_length = seg_end_cm - seg_start_cm) %>%
  group_by(zone) %>%
  summarise(
    template_length = median(zone_length),
    .groups = "drop"
  ) %>%
  rename(zone_std = zone) 




control_keys <- annual_belt %>%
  group_by(transect, year) %>%
  summarise(has_only_never = all(zone == "never_under"),
            .groups = "drop") %>%
  filter(has_only_never)




control_fake_zones <- annual_belt %>%
  filter(zone == "never_under") %>%
  inner_join(control_keys, by = c("transect", "year")) %>%
  cross_join(zone_template) %>%          # ✅ correct cross join
  group_by(transect, year, zone_std) %>% # ✅ zone name now exists
  mutate(
    zone_start = min(loc_cm) +
      cumsum(lag(template_length, default = 0)),
    zone_end   = zone_start + template_length
  ) %>%
  ungroup() %>%
  filter(loc_cm >= zone_start & loc_cm < zone_end) %>%
  select(-zone_start, -zone_end, -template_length, -zone)



facility_zones <- annual_belt %>%
  filter(!transect %in% control_keys$transect | zone != "never_under") %>%
  mutate(zone_std = zone)

dat_std <- bind_rows(facility_zones, control_fake_zones)

comm <- dat_std %>%
  distinct(transect, year, zone_std, spp) %>%
  mutate(presence = 1) %>%
  pivot_wider(
    names_from = spp,
    values_from = presence,
    values_fill = 0
  )



library(vegan)

beta_within <- comm %>%
  group_by(transect, year) %>%
  group_modify(~ {
    
    mat <- as.matrix(select(.x, -zone_std))
    rownames(mat) <- .x$zone_std
    
    # drop empty zones
    mat <- mat[rowSums(mat) > 0, , drop = FALSE]
    
    if (nrow(mat) < 2) {
      return(tibble(
        beta_mean = NA_real_,
        beta_sd   = NA_real_,
        n_zones   = nrow(mat)
      ))
    }
    
    d <- vegdist(mat, method = "jaccard")
    
    tibble(
      beta_mean = mean(d, na.rm = TRUE),
      beta_sd   = sd(d),
      n_zones   = nrow(mat)
    )
  })























facility_zones <- annual_belt %>%
  filter(!transect %in% control_keys$transect | zone != "never_under") %>%
  mutate(zone_std = zone)

dat_std <- bind_rows(facility_zones, control_fake_zones)











library(vegan)


beta_within <- comm %>%
  group_by(transect, year) %>%
  group_modify(~ {
    
    mat <- as.matrix(select(.x, -zone_std))
    rownames(mat) <- .x$zone_std
    
    d <- vegdist(mat, method = "jaccard")
    
    tibble(
      beta_mean = mean(as.numeric(d), na.rm = TRUE),
      beta_sd   = sd(as.numeric(d), na.rm = TRUE),
      n_zones   = nrow(mat)
    )
  })

