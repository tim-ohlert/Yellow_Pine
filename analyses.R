library(tidyverse)
library(ggthemes)

library(dplyr)
library(tidyr)
library(purrr)
library(codyn)


annual_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/annual_belt_with_zone.csv")



perennial_belt <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/perennial_belt_with_zone.csv")


all_metrics <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/all_metrics_by_zone.csv")

annual_line <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/annual_line_zone_overlap_wide.csv")%>%subset(location != "landslide")
perennial_line <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/perennial_line_zone_overlap_wide.csv")%>%subset(location != "landslide")


panel_zones <- read.csv("C:/Users/ohler/Dropbox/Tim Work/Yellow_Pine/data/panel_geom_with_intervals.csv")%>%

  dplyr::select(-date)



panel_zones_long <- panel_zones %>%
  pivot_longer(
    cols = matches("^(start|end)_.*_cm$"),
    names_to = c(".value", "zone"),
    names_pattern = "(start|end)_(.*)_cm"
  ) %>%
  rename(start_cm = start, end_cm = end)%>%
  filter(zone != "ever")



zone_lengths <- panel_zones %>%
  mutate(
    always_cm     = end_always_cm - start_always_cm,
    trans_west_cm = end_trans_west_cm - start_trans_west_cm,
    trans_east_cm = end_trans_east_cm - start_trans_east_cm
  ) %>%
  dplyr::select(transect, panel_id,  always_cm, trans_west_cm, trans_east_cm)


library(dplyr)

panel_bounds <- panel_zones %>%
  transmute(transect, panel_id,
            footprint_start = start_trans_west_cm,
            footprint_end   = end_trans_east_cm) %>%
  group_by(transect) %>%
  arrange(footprint_start, .by_group = TRUE) %>%
  mutate(
    next_start = lead(footprint_start),
    next_panel = lead(panel_id),
    midpoint   = (footprint_end + next_start) / 2
  ) %>%
  ungroup()

# leading gap: before the first panel -> belongs to panel 1
leading <- panel_bounds %>%
  group_by(transect) %>%
  slice_min(footprint_start, n = 1) %>%
  ungroup() %>%
  transmute(transect, panel_id, zone = "never", start_cm = 0, end_cm = footprint_start)

# trailing gap: after the last panel -> belongs to the last panel
trailing <- panel_bounds %>%
  group_by(transect) %>%
  slice_max(footprint_start, n = 1) %>%
  ungroup() %>%
  transmute(transect, panel_id, zone = "never", start_cm = footprint_end, end_cm = 10000)

# middle gaps: split at the midpoint between each pair of neighboring panels
middle_a <- panel_bounds %>%   # first half -> belongs to the EARLIER panel
  filter(!is.na(next_panel)) %>%
  transmute(transect, panel_id, zone = "never", start_cm = footprint_end, end_cm = midpoint)

middle_b <- panel_bounds %>%   # second half -> belongs to the LATER panel
  filter(!is.na(next_panel)) %>%
  transmute(transect, panel_id = next_panel, zone = "never", start_cm = midpoint, end_cm = next_start)

never_segments <- bind_rows(leading, middle_a, middle_b, trailing) %>%
  mutate(length_cm = end_cm - start_cm) %>%
  filter(length_cm > 0)

# combine with the panel-tied zones to get one complete segmentation
full_segments <- bind_rows(
  panel_zones_long %>% select(transect, panel_id, zone, start_cm, end_cm),
  never_segments   %>% select(transect, panel_id, zone, start_cm, end_cm)
) %>%
  mutate(length_cm = end_cm - start_cm)

# fake 1m zones for control transects (no panels, so no real zone structure exists)
transect_location <- annual_line %>%
  distinct(transect, location)

zone_cycle <- c("never", "west", "always", "east")

fake_zones <- transect_location %>%
  filter(location == "control") %>%
  select(transect) %>%
  tidyr::crossing(zone_num = 1:100) %>%
  transmute(
    transect,
    panel_id = ((zone_num - 1) %/% 4) + 1,
    zone     = zone_cycle[((zone_num - 1) %% 4) + 1],
    start_cm = (zone_num - 1) * 100,
    end_cm   = zone_num * 100
  )

full_segments <- bind_rows(
  panel_zones_long %>% select(transect, panel_id, zone, start_cm, end_cm),
  never_segments   %>% select(transect, panel_id, zone, start_cm, end_cm),
  fake_zones
) %>%
  mutate(length_cm = end_cm - start_cm)

# consolidated zone length per transect/panel_id/zone (some panels get 2 never pieces,
# a leading/trailing edge plus one half-gap from a neighbor, so lengths get summed)
zone_len_full <- full_segments %>%
  group_by(transect, panel_id, zone) %>%
  summarise(zone_length_cm = sum(length_cm), .groups = "drop")



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




annual_long <- annual_line %>%
  filter(!is.na(start_canopy_cm)) %>%          # drop no_intercept rows (nothing to expand)
  mutate(cm = map2(start_canopy_cm, stop_canopy_cm - 1, seq)) %>%
  unnest(cm) %>%
  select(-start_canopy_cm, -stop_canopy_cm, -always_under_cm, -transitional_west_cm, -transitional_east_cm,  -line_status, -sample_line_len_cm, -cover_cm_total, -cover_prop_total)     # optional: drop now-redundant bounds


annual_long_zoned <- annual_long %>%
  left_join(
    full_segments,
    by = join_by(transect,
                 cm >= start_cm,
                 cm < end_cm)
  )%>%
  mutate(zone = coalesce(zone, "never"))


perennial_long <- perennial_line %>%
  filter(!is.na(start_canopy_cm)) %>%          # drop no_intercept rows (nothing to expand)
  mutate(cm = map2(start_canopy_cm, stop_canopy_cm - 1, seq)) %>%
  unnest(cm) %>%
  select(-start_canopy_cm, -stop_canopy_cm, -always_under_cm, -transitional_west_cm, -transitional_east_cm,  -line_status, -sample_line_len_cm, -cover_cm_total, -cover_prop_total)     # optional: drop now-redundant bounds


perennial_long_zoned <- perennial_long %>%
  left_join(
    full_segments,
    by = join_by(transect,
                 cm >= start_cm,
                 cm < end_cm)
  )%>%
  mutate(zone = coalesce(zone, "never"))


both_zoned <- rbind(annual_long_zoned, perennial_long_zoned)


# 3. Count presence-cm per species/year/transect/panel/zone, then divide by zone length
abundance <- both_zoned%>%
  count(year, location, transect, panel_id, zone, spp, name = "presence_cm") %>%
  left_join(zone_len_full, by = c("transect", "panel_id", "zone")) %>%
  mutate(cover =  presence_cm / zone_length_cm)


abundance_summ <- abundance%>%
  group_by(year, location, transect, panel_id, zone)%>%
  dplyr::summarize(cover = sum(cover, na.rm = TRUE))%>%
  subset(cover != 0)
 # group_by(year, location, transect, zone)%>%
  #dplyr::summarize(cover = mean(cover, na.rm = TRUE))

abundance_summ$zone2 <- ifelse(abundance_summ$location == "control", "control", abundance_summ$zone)

abundance_summ%>%
  group_by(year, location, zone2)%>%
  dplyr::summarize(cover_mean = mean(cover, na.rm = TRUE),
                   cover_se   = sd(cover, na.rm = TRUE) /
                     sqrt(sum(!is.na(cover))),
                   .groups = "drop"
  )%>%
ggplot(aes(x = factor(year), y = cover_mean, color = location, shape = zone2,
          group = interaction(location, zone2))) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_pointrange(
    aes(ymin = cover_mean - cover_se, ymax = cover_mean + cover_se),
    position = position_dodge(width = 0.2)
  ) +
  #  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Cover", color = "Location") +
  scale_color_manual(
    values = c("control" = "darkorange", "solar facility" = "steelblue")
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



########################################
######Some alpha diversity stuff

abundance_comm <- abundance%>%
              subset(is.na(cover) == FALSE)%>%
                  unite("replicate", c( location, transect, panel_id, zone), sep = "::")%>%
  community_structure(time.var = "year",
                      abundance.var = "cover",
                      replicate.var = "replicate",
                      metric = "EQ")%>%
              separate("replicate", c( "location", "transect", "panel_id", "zone"), sep = "::")
    
abundance_comm$zone2 <- ifelse(abundance_comm$location == "control", "control", abundance_comm$zone)


abundance_comm%>%
  group_by(year, location, zone2)%>%
  dplyr::summarize(richness_mean = mean(richness, na.rm = TRUE),
                   richness_se   = sd(richness, na.rm = TRUE) /
                     sqrt(sum(!is.na(richness))),
                   .groups = "drop"
  )%>%
  ggplot(aes(x = factor(year), y = richness_mean, color = location, shape = zone2,
             group = interaction(location, zone2))) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_pointrange(
    aes(ymin = richness_mean - richness_se, ymax = richness_mean + richness_se),
    position = position_dodge(width = 0.2)
  ) +
  #  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Species richness", color = "Location") +
  scale_color_manual(
    values = c("control" = "darkorange", "solar facility" = "steelblue")
  ) +
  theme_base()



abundance_comm%>%
  group_by(year, location, zone2)%>%
  dplyr::summarize(EQ_mean = mean(EQ, na.rm = TRUE),
                   EQ_se   = sd(EQ, na.rm = TRUE) /
                     sqrt(sum(!is.na(EQ))),
                   .groups = "drop"
  )%>%
  drop_na()%>%
  ggplot(aes(x = factor(year), y = EQ_mean, color = location, shape = zone2,
             group = interaction(location, zone2))) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_pointrange(
    aes(ymin = EQ_mean - EQ_se, ymax = EQ_mean + EQ_se),
    position = position_dodge(width = 0.2)
  ) +
  #  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Year", y = "Evenness", color = "Location") +
  scale_color_manual(
    values = c("control" = "darkorange", "solar facility" = "steelblue")
  ) +
  theme_base()



########################################
###### INDICATOR SPECIES ANALYSIS ######
########################################
# Two analyses:
#   (1) solar facility vs control   -> replicate = transect
#   (2) panel zones                 -> replicate = zone within panel within transect
#
# Assumes `abundance`, `both_zoned`, and `zone_len_full` already exist
# from the upstream script.

library(indicspecies)
library(permute)

set.seed(1)


# long -> wide species matrix, zeros filled in
make_wide <- function(dat, id_cols) {
  dat %>%
    filter(!is.na(cover), cover > 0) %>%
    group_by(across(all_of(c(id_cols, "spp")))) %>%
    dplyr::summarize(cover = sum(cover), .groups = "drop") %>%
    pivot_wider(names_from = spp, values_from = cover, values_fill = 0)
}

# run multipatt; drops empty rows and absent species first
run_indval <- function(wide, id_cols, group_var, blocks = NULL,
                       nperm = 999, duleg = FALSE) {
  
  spp_cols <- setdiff(names(wide), id_cols)
  x   <- as.data.frame(wide[, spp_cols, drop = FALSE])
  grp <- wide[[group_var]]
  
  keep <- rowSums(x) > 0 & !is.na(grp)        # multipatt chokes on all-zero rows
  x    <- x[keep, , drop = FALSE]
  grp  <- grp[keep]
  if (!is.null(blocks)) blocks <- factor(blocks[keep])
  
  x <- x[, colSums(x) > 0, drop = FALSE]      # species absent from this subset
  
  ctrl <- if (is.null(blocks)) {
    how(nperm = nperm)
  } else {
    how(nperm = nperm, blocks = blocks)       # permute only within transect
  }
  
  multipatt(x, grp, func = "IndVal.g", duleg = duleg, control = ctrl)
}

# significant results as a tidy data frame
tidy_indval <- function(iv, alpha = 0.05) {
  s         <- iv$sign
  grp_cols  <- grep("^s\\.", colnames(s), value = TRUE)
  grp_names <- sub("^s\\.", "", grp_cols)
  
  data.frame(
    spp     = rownames(s),
    group   = apply(s[, grp_cols, drop = FALSE], 1,
                    function(r) paste(grp_names[r == 1], collapse = " + ")),
    stat    = s$stat,
    p.value = s$p.value,
    row.names = NULL,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(p.value), p.value <= alpha) %>%
    arrange(group, desc(stat))
}

split_by_year <- function(wide) {
  pieces <- wide %>% group_by(year) %>% group_split()
  rlang::set_names(pieces, purrr::map_chr(pieces, ~ as.character(.x$year[1])))
}


# ==========================================================================
# ANALYSIS 1 -- SOLAR FACILITY vs CONTROL
# Cover rolled up to the whole transect so transect is the replicate.
# ==========================================================================

transect_len <- zone_len_full %>%
  group_by(transect) %>%
  dplyr::summarize(transect_len_cm = sum(zone_length_cm), .groups = "drop")

abundance_transect <- both_zoned %>%
  count(year, location, transect, spp, name = "presence_cm") %>%
  left_join(transect_len, by = "transect") %>%
  mutate(cover = presence_cm / transect_len_cm)

loc_ids   <- c("year", "location", "transect")
wide_loc  <- make_wide(abundance_transect, loc_ids)

iv_loc <- wide_loc %>%
  split_by_year() %>%
  purrr::map(~ run_indval(.x, loc_ids, group_var = "location"))

purrr::iwalk(iv_loc, function(iv, yr) {
  cat("\n===============  facility vs control,", yr, " ===============\n")
  print(summary(iv))
})

iv_loc_tbl <- purrr::imap_dfr(iv_loc,
                              ~ tidy_indval(.x) %>% mutate(year = .y, .before = 1))
iv_loc_tbl


# ==========================================================================
# ANALYSIS 2 -- PANEL ZONES
# Replicate = one zone within one panel within one transect, facility only.
# ==========================================================================

zone_ids  <- c("year", "location", "transect", "panel_id", "zone")
wide_zone <- make_wide(abundance, zone_ids)

iv_zone <- wide_zone %>%
  filter(location == "solar facility") %>%
  split_by_year() %>%
  purrr::map(~ run_indval(.x, zone_ids, group_var = "zone", blocks = .x$transect))

purrr::iwalk(iv_zone, function(iv, yr) {
  cat("\n===============  zone indicators,", yr, " ===============\n")
  print(summary(iv))
})

iv_zone_tbl <- purrr::imap_dfr(iv_zone,
                               ~ tidy_indval(.x) %>% mutate(year = .y, .before = 1))
iv_zone_tbl


# --- single year on its own
iv_zone_2024 <- wide_zone %>%
  filter(year == 2024, location == "solar facility") %>%
  run_indval(zone_ids, group_var = "zone")

summary(iv_zone_2024)

iv_zone_2024_tbl <- tidy_indval(iv_zone_2024) %>%
  mutate(year = 2024, .before = 1)

iv_zone_2024_tbl






wide_zone2 <- wide_zone %>%
mutate(zone2 = ifelse(location == "control", "control", zone), .after = zone)

zone2_ids <- c(zone_ids, "zone2")

iv_zone2 <- wide_zone2 %>%
  split_by_year() %>%
  purrr::map(~ run_indval(.x, zone2_ids, group_var = "zone2"))

purrr::iwalk(iv_zone2, function(iv, yr) {
  cat("\n===============  control + zone indicators,", yr, " ===============\n")
  print(summary(iv))
})

iv_zone2_tbl <- purrr::imap_dfr(iv_zone2,
                                ~ tidy_indval(.x) %>% mutate(year = .y, .before = 1))
iv_zone2_tbl



































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
