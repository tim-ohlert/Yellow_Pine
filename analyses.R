library(tidyverse)
library(ggthemes)



annual_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/annual_belt_with_zone.csv")



perennial_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/perennial_belt_with_zone.csv")


all_metrics <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/all_metrics_by_zone.csv")

annual_line <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/annual_line_zone_overlap_wide.csv")%>%subset(location != "landslide")
perennial_line <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/perennial_line_zone_overlap_wide.csv")%>%subset(location != "landslide")


panel_zones <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/panel_geom_with_intervals.csv")





perennial_summ <- perennial_line %>%
  group_by(year, transect, location)%>%
  dplyr::summarize(
    perennial_cover_prop = sum(cover_prop_total)
                            ) %>%
    group_by(year, location) %>%
  dplyr::summarize(
    perennial_dens = mean(perennial_cover_prop, na.rm = TRUE),
    perennial_se   = sd(perennial_cover_prop, na.rm = TRUE) /
      sqrt(sum(!is.na(perennial_cover_prop))),
    .groups = "drop"
  )


annual_summ <- annual_line %>%
  group_by(year, transect, location)%>%
  dplyr::summarize(
    annual_cover_prop = sum(cover_prop_total)
  ) %>%
  group_by(year, location) %>%
  dplyr::summarize(
    annual_dens = mean(annual_cover_prop, na.rm = TRUE),
    annual_se   = sd(annual_cover_prop, na.rm = TRUE) /
      sqrt(sum(!is.na(annual_cover_prop))),
    .groups = "drop"
  )

ann_per_total <- left_join(perennial_summ, annual_summ, by = c("year", "location"))

ann_per_total_long <- pivot_longer(
  ann_per_total,
  cols = c(perennial_dens, perennial_se, annual_dens, annual_se),
  names_to = c("type", ".value"),
  names_sep = "_"
)

ggplot(ann_per_total_long, aes(x = factor(year), y = dens, color = location, shape = type,
                               group = interaction(location, type))) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_pointrange(
    aes(ymin = dens - se, ymax = dens + se),
    position = position_dodge(width = 0.2)
  ) +
#  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Cover", color = "Location") +
  scale_color_manual(
    values = c("control" = "darkorange", "solar facility" = "steelblue")
  ) +
  theme_base()

#metrics_long <- metrics_summ %>%
#  select(year, location,
#         perennial_dens, perennial_se,
#         annual_dens, annual_se) %>%
#  pivot_longer(
#    cols = -c(year, location),
#    names_to = c("life_history", ".value"),
#    names_pattern = "(perennial|annual)_(dens|se)"
#  )




#ggplot(  ann_per_total,aes(x = as.factor(year),    y = dens)
#) +
#  facet_wrap(~life_history, scales = "free_y")+
#  geom_point() +
#  geom_line() +
#  geom_errorbar(
#    aes(ymin = dens - se, ymax = dens + se),
#    width = 0.15
#  ) +
#  labs(
#    x = "Year",
#    y = expression("Density (indiv " * m^-2 * ")"),
#    color = "Location",
#    shape = "Life history"
#  ) +
#  theme_base()


ggsave( "C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/figures/figure_1.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 8,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)





perennial_summ2 <- perennial_line %>%
  group_by(year, transect, location, zone) %>%
  dplyr::summarize(
    perennial_cover_prop = sum(cover_prop_total))%>%
  group_by(year, location, zone) %>%
  dplyr::summarize(
    perennial_dens = mean(perennial_cover_prop, na.rm = TRUE),
    perennial_se   = sd(perennial_cover_prop, na.rm = TRUE) /
      sqrt(sum(!is.na(perennial_cover_prop))),
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




ggplot(
  metrics_long2,
  aes(
    x = as.factor(year),
    y = dens,
    color = location,
    shape = life_history,
    group = location
  )
) +
  facet_grid(life_history ~ zone, scales = "free_y") +
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

ggsave( "C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/figures/figure_2.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 10,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)

###########
###Beta diversity annuals



zone_template <- annual_belt %>%
  filter(location != "control" & year == 2024) %>%
  distinct(transect, year, zone, seg_start_cm, seg_end_cm) %>%
  mutate(zone_length = seg_end_cm - seg_start_cm) %>%
  group_by(zone) %>%
  summarise(
    template_length = median(zone_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(zone_std = zone) 




control_keys <- annual_belt %>%
  group_by(transect, year) %>%
  summarise(is_control = all(location != "solar facility"),
            .groups = "drop") %>%
  filter(is_control)






control_fake_zones <- annual_belt %>%
  subset(location != "solar facility" | year == "2022") %>%
  inner_join(control_keys, by = c("transect", "year")) %>%
  group_by(transect, year) %>%
  mutate(transect_start = min(loc_cm, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(
    zone_template %>%
      arrange(zone_std) %>%   # define zone order explicitly if needed
      mutate(
        zone_start = cumsum(lag(template_length, default = 0)),
        zone_end   = zone_start + template_length
      ),
    by = character()
  ) %>%
  mutate(
    rel_loc = loc_cm - transect_start
  ) %>%
  filter(rel_loc >= zone_start & rel_loc < zone_end) %>%
  select(-transect_start, -rel_loc, -zone_start, -zone_end, -template_length, -zone)





facility_zones <- annual_belt %>%
  filter(#!transect %in% control_keys$transect |
    location == "solar facility" & year >= 2023) %>%
  mutate(zone_std = zone)

dat_std <- bind_rows(facility_zones, control_fake_zones)

comm <- dat_std %>%
  distinct(location, transect, year, zone_std, spp) %>%
  mutate(presence = 1) %>%
  pivot_wider(
    names_from = spp,
    values_from = presence,
    values_fill = 0
  )



library(vegan)

set.seed(123)  # for reproducibility

beta_within <- comm %>%
  group_by(transect, year, location) %>%
  group_modify(~ {
    
    # if multiple rows per zone category, randomly keep one
    dat <- .x %>%
      group_by(zone_std) %>%
      slice_sample(n = 1) %>%
      ungroup()
    
    # build matrix
    mat <- dat %>%
      select(-zone_std) %>%
      as.matrix()
    
    rownames(mat) <- dat$zone_std
    
    # count retained zones
    n_zones <- nrow(mat)
    
    # skip if fewer than 2 zones
    if (n_zones < 2) {
      return(tibble(
        beta_mean = NA_real_,
        beta_sd   = NA_real_,
        n_zones   = n_zones
      ))
    }
    
    # calculate beta diversity
    d <- vegdist(mat, method = "jaccard")
    
    tibble(
      beta_mean = mean(d, na.rm = TRUE),
      beta_sd   = sd(d),
      n_zones   = n_zones
    )
  }) %>%
  filter(n_zones == 4)


beta_within_summ <- beta_within%>%
                    group_by(year, location)%>%
                    dplyr::summarize(mean = mean(beta_mean), standard_deviation = sd(beta_mean))
  
#this is just annual beta div
ggplot(beta_within_summ, aes(as.factor(year), mean, color = location))+
  geom_pointrange(aes(ymax = mean+standard_deviation, ymin = mean-standard_deviation),position = position_dodge(width = 0.4)
  )+
  xlab("")+
  ylab("Beta diversity - annuals")+
  theme_base()


ggsave( "C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/figures/beta_annual.pdf",
        plot = last_plot(),
        device = "pdf",
        path = NULL,
        scale = 1,
        width = 6,
        height = 4,
        units = c("in"),
        dpi = 600,
        limitsize = TRUE
)




#########
##Beta diversity perennial

###########
### Beta diversity — perennial community

zone_template <- perennial_belt %>%
  filter(location != "control" & year == 2024) %>%
  distinct(transect, year, zone, seg_start_cm, seg_end_cm) %>%
  mutate(zone_length = seg_end_cm - seg_start_cm) %>%
  group_by(zone) %>%
  summarise(
    template_length = mean(zone_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(zone_std = zone)



control_keys <- perennial_belt %>%
  group_by(transect, year) %>%
  summarise(
    is_control = all(location != "solar facility"),
    .groups = "drop"
  ) %>%
  filter(is_control)



control_fake_zones <- perennial_belt %>%
  subset(location != "solar facility" | year == "2022") %>%
  inner_join(control_keys, by = c("transect", "year")) %>%
  group_by(transect, year) %>%
  mutate(transect_start = min(loc_cm, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(
    zone_template %>%
      arrange(zone_std) %>%
      mutate(
        zone_start = cumsum(lag(template_length, default = 0)),
        zone_end   = zone_start + template_length
      ),
    by = character()
  ) %>%
  mutate(
    rel_loc = loc_cm - transect_start
  ) %>%
  filter(rel_loc >= zone_start & rel_loc < zone_end) %>%
  select(
    -transect_start,
    -rel_loc,
    -zone_start,
    -zone_end,
    -template_length,
    -zone
  )



facility_zones <- perennial_belt %>%
  filter(
    location == "solar facility" & year >= 2023
  ) %>%
  mutate(zone_std = zone)



dat_std <- bind_rows(facility_zones, control_fake_zones)



comm <- dat_std %>%
  distinct(location, transect, year, zone_std, spp) %>%
  mutate(presence = 1) %>%
  pivot_wider(
    names_from = spp,
    values_from = presence,
    values_fill = 0
  )%>%
  subset(is.na(zone_std) == FALSE)





set.seed(123)  # for reproducibility

beta_within <- comm %>%
  group_by(transect, year, location) %>%
  group_modify(~ {
    
    # if multiple rows per zone category, randomly keep one
    dat <- .x %>%
      group_by(zone_std) %>%
      slice_sample(n = 1) %>%
      ungroup()
    
    # build matrix
    mat <- dat %>%
      select(-zone_std) %>%
      as.matrix()
    
    rownames(mat) <- dat$zone_std
    
    # count retained zones
    n_zones <- nrow(mat)
    
    # skip if fewer than 2 zones
    if (n_zones < 2) {
      return(tibble(
        beta_mean = NA_real_,
        beta_sd   = NA_real_,
        n_zones   = n_zones
      ))
    }
    
    # calculate beta diversity
    d <- vegdist(mat, method = "jaccard")
    
    tibble(
      beta_mean = mean(d, na.rm = TRUE),
      beta_sd   = sd(d),
      n_zones   = n_zones
    )
  }) %>%
  filter(n_zones == 4)












beta_within_summ <- beta_within %>%
  group_by(year, location) %>%
  dplyr::summarize(
    mean = mean(beta_mean, na.rm = TRUE),
    standard_deviation = sd(beta_mean, na.rm = TRUE),
    .groups = "drop"
  )



# perennial beta diversity
ggplot(
  beta_within_summ,
  aes(as.factor(year), mean, color = location)
) +
  geom_pointrange(
    aes(
      ymax = mean + standard_deviation,
      ymin = mean - standard_deviation
    ),
    position = position_dodge(width = 0.4)
  ) +
  xlab("") +
  ylab("Beta diversity - perennials") +
  theme_base()



ggsave(
  "C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/figures/beta_perennial.pdf",
  plot = last_plot(),
  device = "pdf",
  scale = 1,
  width = 6,
  height = 4,
  units = "in",
  dpi = 600,
  limitsize = TRUE
)
