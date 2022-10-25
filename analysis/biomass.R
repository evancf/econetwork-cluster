# Packages
devtools::source_gist("71d758f65261a72ab7dc") # gist for ipak

ipak(c("tidyverse", "BIEN"))


# Pull data from BIEN
stem1 <- BIEN_stem_sampling_protocol("0.1 ha  transect, stems >= 2.5 cm dbh")
stem2 <- BIEN_stem_sampling_protocol("0.1 ha  transect, stems >= 2.5 cm dbh (Warning: species + count of individuals + stem DBHs recorded once per subplot. Individuals not linked to stems)")

stem_dat <- bind_rows(stem1, stem2)


# Get the original plot data
plot_data_trait <- read.csv("./data/plot_data_trait.csv", 
                            header = T, 
                            row.names = 1)


# Fill in missing trait data ---------------------------------------------------
trait_dat <- plot_data_trait %>% 
  filter(!duplicated(scrubbed_species_binomial))

# Get SSD for every species

trait_dat$sp_med_ssd %>% is.na() %>% table()

ipak("BIOMASS")
data("wdData")

# missing_ssd <- trait_dat %>% 
#   filter(is.na(sp_med_ssd))
# gen_in_wd <- (missing_ssd$genus[!duplicated(missing_ssd$genus)]) %in% wdData$genus
# gen_data_already <- missing_ssd$gen_med_ssd[!duplicated(missing_ssd$genus)] %>% is.na() # [!duplicated(missing_ssd$genus)]
# cbind(gen_in_wd, !gen_data_already)

# We will use the existing genus median if it is available

trait_dat <- trait_dat %>% 
  mutate(WD = ifelse(!is.na(sp_med_ssd), sp_med_ssd, NA)) %>% 
  mutate(WD = ifelse(is.na(WD), gen_med_ssd, WD))

# Need to get family info and join
missing_gen_to_fam <- trait_dat %>% 
  filter(is.na(WD)) %>% 
  pull(genus) %>% 
  unique() %>% 
  TNRS::TNRS() %>% 
  dplyr::select(Name_submitted, Accepted_family) %>% 
  rename("genus" = "Name_submitted",
         "family" = "Accepted_family")

trait_dat <- trait_dat %>% 
  left_join(missing_gen_to_fam)

# Get family averages and join these to trait data
fam_wd <- wdData %>% group_by(family) %>% dplyr::summarise(fam_med_wd = mean(wd))
trait_dat <- trait_dat %>% 
  left_join(fam_wd)

# Now put these family averages in as WD
trait_dat <- trait_dat %>% 
  mutate(WD = ifelse(is.na(WD), fam_med_wd, WD))


# Get together the stem data ---------------------------------------------------
stem_dat <- stem_dat %>% 
  left_join(dplyr::select(trait_dat,
                          -c(1:6,8:17)), by = "scrubbed_species_binomial")


# Finally, use plot level averages for WD if the species, genus, or family averages
# are not available
plot_wd <- stem_dat %>% 
  filter(sp_med_whole.plant.growth.form == "tree") %>%
  dplyr::select(scrubbed_species_binomial,
                plot_name, 
                WD) %>% 
  group_by(plot_name) %>% 
  dplyr::summarise(plot_med_wd = mean(WD, na.rm = T))

stem_dat <- stem_dat %>% 
  left_join(plot_wd) %>% 
  mutate(WD = ifelse(is.na(WD), plot_med_wd, WD))


# Calculate Aboveground Biomass of each individual with a DBH ------------------

# Keep only those trees with a known DBH and species name
short_stem_dat <- stem_dat %>%
  filter(sp_med_whole.plant.growth.form == "tree") %>%
  #filter(!is.na(scrubbed_species_binomial)) %>% 
  filter(!is.na(stem_dbh_cm))

# There's an island plots that won't work with a height model raster
short_stem_dat <- short_stem_dat %>% 
  filter(!plot_name %in% c("PROVIDEN"))

# bad_indices <- FILL IN INDICES IF THERE ARE ERRORS IN THE computeAGB function
# short_stem_dat$plot_name[bad_indices]

short_stem_dat <- short_stem_dat %>% 
  mutate(agb = computeAGB(D = short_stem_dat$stem_dbh_cm, 
                          WD = short_stem_dat$WD,
                          coord = short_stem_dat[, c("longitude", "latitude")])) %>% 
  mutate(agb = agb * individual_count)


agb_summary <- short_stem_dat %>% 
  filter(!is.na(scrubbed_species_binomial)) %>% 
  group_by(plot_name, fleshy) %>% 
  dplyr::summarise(sum_agb = sum(agb),
                   plot_area_ha = dplyr::first(plot_area_ha)) %>% 
  pivot_wider(names_from = fleshy, 
              values_from = sum_agb) %>% 
  rename("nonfleshy_agb" = "0",
         "fleshy_agb" = "1") %>% 
  mutate(nonfleshy_agb = ifelse(is.na(nonfleshy_agb), 0, nonfleshy_agb)) %>%  # This is because if there's no (non)fleshy agb, it doesn't appear as a row after pivoting
  mutate(fleshy_agb = ifelse(is.na(fleshy_agb), 0, fleshy_agb)) %>% 
  mutate(prop_agb_fleshy = fleshy_agb / (nonfleshy_agb + fleshy_agb)) %>% 
  mutate(nonfleshy_agb_per_ha = nonfleshy_agb / plot_area_ha,
         fleshy_agb_per_ha = fleshy_agb / plot_area_ha) %>% 
  mutate(total_agb_per_ha = nonfleshy_agb_per_ha + fleshy_agb_per_ha)

mean(agb_summary$prop_agb_fleshy, na.rm = T)
agb_summary %>% ungroup() %>% dplyr::summarize(across(3:4, function(x) sum(x, na.rm = T)))

portion_fleshy_summary <- short_stem_dat %>% 
  filter(!is.na(scrubbed_species_binomial)) %>% 
  group_by(plot_name) %>% 
  dplyr::summarise(prop_individuals_fleshy = mean(fleshy))

#

# Map these plots
ipak(c("rnaturalearth","rnaturalearthdata",
       "sf"))
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")


projcrs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

plot_summary <- short_stem_dat %>% 
  group_by(plot_name) %>% 
  summarise(latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T)) %>% 
  left_join(agb_summary) %>% 
  left_join(portion_fleshy_summary)

ipak("sp")
ipak("rworldmap")
coords2continent = function(points) {  
  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

plot_summary <- plot_summary %>% 
  mutate(continent = as.factor(as.character(coords2continent(plot_summary[,c("longitude", "latitude")]))))

plot_coords <- st_as_sf(plot_summary,
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot_coords <- plot_coords  %>% 
  st_transform(crs = projcrs)

ggplot(data = world) +
  geom_sf(lwd = 0) +
  theme_void() +
  geom_sf(data = plot_coords, 
          aes(color = prop_agb_fleshy),
          alpha = 0.5,
          stroke = 0,
          size = 2) + #(plot_coords$prop_agb_fleshy + 1))  + #(plot_coords$prop_agb_fleshy + 0.5)
  scale_color_distiller(palette = "Spectral",
                        name = "Proportion\nof AGB from\nfleshy-fruited\nplants")

plot(plot_summary$prop_individuals_fleshy, plot_summary$prop_agb_fleshy,
     col = rgb(sqrt(abs(plot_summary$latitude) / max(abs(plot_summary$latitude))), 
               0, 0, 1),
     pch = 14 + as.numeric(plot_summary$continent))
legend("topleft",legend = levels(plot_summary$continent),
       pch = 14 + as.numeric(1:length(plot_summary$continent)))


#


library(rgee)
library(sf)

ee_Initialize("evanfricke", gcs = TRUE, drive = TRUE)

ghm <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$
  select(c("gHM"))

plot_coords$latitude <- plot_summary$latitude
plot_coords$abs_latitude <- abs(plot_summary$latitude)


# Want a 5km buffer (using appropriate projection for the study site)
utm_prj4 <- function(x) {
  
  coords <- cbind(x, st_coordinates(x))
  
  long <- coords$X
  lat <- coords$Y
  
  zone <- if(lat >= 56 && lat < 64 && long >= 3 && long < 12){x <- 32} else if(
    lat >= 72 && lat < 84 && long >= 0 && long < 9) {x <- 31} else if(
      lat >= 72 && lat < 84 && long >= 9 && long < 21) {x <- 33} else if(
        lat >= 72 && lat < 84 && long >= 21 && long < 33) {x <- 35} else if(
          lat >= 72 && lat < 84 && long >= 33 && long < 42) {x <- 37} else{
            x <- (floor((long + 180)/6) %% 60) + 1
          }
  prj <- purrr::map2_chr(zone, lat, function(y, z){
    if (z >= 0){
      paste0("+proj=utm +zone=", y, " +datum=WGS84 +units=m +no_defs")
    } else{
      paste0("+proj=utm +zone=", y, " +south", " +datum=WGS84 +units=m +no_defs")
    }})
  prj
}

plot_coords <- purrr::map2(1:nrow(plot_coords), utm_prj4(plot_coords), function(x, y){
  st_transform(plot_coords[x,], y)
})
plot_coords <- map(plot_coords, ~ st_buffer(., 5000))
plot_coords <- map(plot_coords, ~ st_transform(., projcrs)) # Go back to orig projection
plot_coords <- do.call(rbind, plot_coords) # Go from list back to df

plot_coords

plot_coords <- plot_coords %>% filter(!st_is_empty(plot_coords))

ee_ghm <- ee_extract(
  x = ghm,
  y = plot_coords,
  #scale = 250,
  fun = ee$Reducer$mean(),
  sf = TRUE
)

plot(ee_ghm$prop_agb_fleshy ~ ee_ghm$X2016_gHM)

ee_ghm <- ee_ghm %>% 
  left_join(dplyr::select(plot_summary, plot_name, latitude))

lm(prop_agb_fleshy ~ X2016_gHM + latitude,
   data = ee_ghm) %>% summary()

plot(total_agb_per_ha ~ X2016_gHM, log = "y",
     data = ee_ghm,
     col = rgb(abs(latitude)/66, 0,0),
     pch = 16)

ee_ghm$total_agb_per_ha_log <- log(ee_ghm$total_agb_per_ha)

trop_ee_ghm <- ee_ghm %>% filter(abs(latitude) < 23.5)
plot(total_agb_per_ha ~ X2016_gHM, log = "y",
     col = rgb(abs(latitude)/66, 0,0),
     data = trop_ee_ghm,
     pch = 16)

plot(prop_agb_fleshy ~ prop_individuals_fleshy, #log = "y",
     col = rgb(X2016_gHM, 0,0),
     data = ee_ghm,
     pch = 16)

prop_agb_fleshy_range <- ee_ghm$prop_agb_fleshy[!ee_ghm$prop_agb_fleshy %in% c(0, 1)] %>% range()

ee_ghm$prop_agb_fleshy_logit <- ifelse(ee_ghm$prop_agb_fleshy > (1-prop_agb_fleshy_range[1]),
                                       (1-prop_agb_fleshy_range[1]), ee_ghm$prop_agb_fleshy)
ee_ghm$prop_agb_fleshy_logit <- ifelse(ee_ghm$prop_agb_fleshy_logit < prop_agb_fleshy_range[1],
                                       prop_agb_fleshy_range[1], ee_ghm$prop_agb_fleshy_logit)
ee_ghm$prop_agb_fleshy_logit <-   ee_ghm$prop_agb_fleshy_logit %>% 
  gtools::logit()

hist(ee_ghm$prop_agb_fleshy_logit)


mod1 <- lm(prop_agb_fleshy_logit ~ X2016_gHM,
          data = ee_ghm)
mod2 <- lm(prop_agb_fleshy_logit ~ abs_latitude,
           data = ee_ghm)
mod3 <- lm(prop_agb_fleshy_logit ~ X2016_gHM + abs_latitude,
           data = ee_ghm)
mod4 <- lm(prop_agb_fleshy_logit ~ X2016_gHM * abs_latitude,
           data = ee_ghm)
AIC(mod1, mod2, mod3, mod4)

sjPlot::plot_model(mod3, type = "pred")
sjPlot::plot_model(mod4, type = "int")

mod5 <- lm(prop_agb_fleshy_logit ~ latitude + I(latitude^2),
           data = ee_ghm)
summary(mod5)

ggplot(plot_summary,
       aes(x = abs(latitude), y = fleshy_agb)) +
  theme_classic() +
  geom_point(aes(size = total_agb_per_ha), alpha = 0.5, stroke = 0, position = "jitter") +
  scale_y_log10()

# Make some other plots

binomial_smooth <- function(...) {
  geom_smooth(method = "lm",  ...)
}

ggplot(plot_summary,
       aes(x = latitude, y = prop_agb_fleshy)) +
  theme_classic() +
  geom_point(aes(size = total_agb_per_ha), alpha = 0.5, stroke = 0, position = "jitter") +
  #binomial_smooth(formula = y ~ splines::ns(x, 2)) + 
  geom_smooth(data = short_stem_dat, 
              aes(x = latitude, y = fleshy),
              method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ splines::ns(x, 2))

png(filename = "./visualization/proportion fleshy.png",
    width = 4,
    height = 3,
    res = 440,
    units = "in")

ggplot(plot_summary,
       aes(x = latitude, y = prop_agb_fleshy)) +
  theme_classic() +
  # geom_point(#aes(size = total_agb_per_ha), 
  #            alpha = 0.2, 
  #            stroke = 0, 
  #            position = "jitter") +
  geom_jitter(size = 0.8,
              alpha = 0.2,
              stroke = 0,
              height = 0.02,
              width = 3
              ) +
  #binomial_smooth(formula = y ~ splines::ns(x, 2)) + 
  geom_smooth(data = short_stem_dat, 
              aes(x = latitude, y = fleshy),
              method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ splines::ns(x, 2)) +
  ylab("Proportion of aboveground\nbiomass fleshy-fruited") +
  xlab("Latitude")

dev.off()




short_stem_dat <- short_stem_dat %>% 
  left_join(dplyr::select(ee_ghm,
                          plot_name,
                          X2016_gHM,
                          continent) %>% st_drop_geometry())
short_stem_dat <- short_stem_dat %>% 
  mutate(abs_latitude = abs(latitude))

short_stem_dat_trop <- short_stem_dat %>% 
  filter(#abs_latitude < 23.5,
         continent %in% c("South America and the Caribbean","North America"))

mod <- glm(fleshy ~ splines::ns(latitude, 2) * X2016_gHM, data = short_stem_dat_trop,
    family = "binomial")
mod <- glm(fleshy ~ abs_latitude + X2016_gHM, data = short_stem_dat_trop,
           family = "binomial")
mod1 <- glm(fleshy ~ abs_latitude * X2016_gHM, data = short_stem_dat_trop,
           family = "binomial")
AIC(mod, mod1)
summary(mod)

sjPlot::plot_model(mod, type = "pred")
sjPlot::plot_model(mod1, type = "int")









#

ggplot(short_stem_dat,
       aes(x = stem_dbh_cm, y = sp_med_ssd, color = fleshy)) +
  geom_point(size = 0.3, position = "jitter") +
  scale_x_log10()

mod <- lm(sp_med_ssd ~ stem_dbh_cm * fleshy, data = short_stem_dat)

summary(mod)

sjPlot::plot_model(mod, type = "int")



ggplot(trait_dat,
       aes(x = sp_med_plant.height.vegetative, y = sp_med_ssd, color = fleshy)) +
  geom_point(size = 0.8, position = "jitter")

trait_dat$fleshy <- as.factor(trait_dat$fleshy)
trait_dat$sp_med_lma <- 1/trait_dat$sp_med_sla

trop_trait_dat <- short_stem_dat %>% 
  filter(latitude > -23.5,
         latitude < 23.5,
         !duplicated(scrubbed_species_binomial))
dim(trop_trait_dat)

trop_trait_dat$fleshy <- as.factor(trop_trait_dat$fleshy)
trop_trait_dat$sp_med_lma <- 1/trop_trait_dat$sp_med_sla
trop_trait_dat$sp_med_seed.dry.mass.log <- log(trop_trait_dat$sp_med_seed.dry.mass + min(trop_trait_dat$sp_med_seed.dry.mass[which(trop_trait_dat$sp_med_seed.dry.mass > 0)],
                                                                                         na.rm = T))

mod <- lm(WD ~ sp_med_plant.height.vegetative * fleshy, data = trop_trait_dat) # sp_med_seed.dry.mass
summary(mod)
sjPlot::plot_model(mod, type = "int")

plot(jitter(short_stem_dat$stem_dbh_cm) ~ jitter(short_stem_dat$sp_med_plant.height.vegetative, 100),
     cex = 0.5, col = rgb(0,0,0,0.1), log = "y")

library("lme4")
mod <- lm(sp_med_ssd ~ fleshy, data = trait_dat)
summary(mod)

sjPlot::plot_model(mod, type = "int")

# Other covariates

library(raster)
library(sp)

r <- getData("worldclim", var="bio", res=2.5)

values <- extract(r, plot_coords)

values <- values / 10

plot_coords <- cbind(plot_coords, values)


plot(plot_coords$prop_agb_fleshy ~ plot_coords$bio1)
plot(plot_coords$prop_agb_fleshy ~ plot_coords$bio19)


plot(plot_summary$prop_agb_fleshy ~ abs(plot_summary$latitude))





# THink I can delete below

# Subset to have stems identified to individual

short_stem_dat <- stem_dat %>%
  filter(!is.na(scrubbed_species_binomial)) %>% 
  filter(is.na(stem_dbh_cm))

stem_dat %>% 
  #filter(!is.na(scrubbed_species_binomial)) %>% 
  pull(fleshy) %>% 
  mean()

# 


plot_summary <- stem_dat %>% 
  group_by(plot_name) %>% 
  summarise(latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T))

dim(plot_summary)










# # Note that there are no additional data available from the BIOMASS package
# ipak("BIOMASS")
# data("wdData")
# wdData$scrubbed_species_binomial <- paste(wdData$genus, wdData$species)
# (missing_ssd %in% wdData$scrubbed_species_binomial) %>% table()


# Will use the approach outlined here: https://onlinelibrary.wiley.com/doi/pdf/10.1111/geb.13309

#



stem_dat$plot_area_ha %>% table()


stem_dat$stem_dbh_cm %>% hist()

stem_dat %>% 
  filter(!is.na(scrubbed_species_binomial)) %>% ggplot(aes(x = as.factor(fleshy), y = stem_dbh_cm)) + 
  geom_boxplot(notch = T) + 
  scale_y_log10()




stem_dat2 %>% filter(!is.na(scrubbed_species_binomial)) %>% arrange(plot_name, subplot) %>% View()



gentry_dat <- BIEN_plot_dataset("Gentry Transect Dataset")

dim(gentry_dat)

gentry_dat$sampling_protocol %>% table()


stem_data <- bind_rows(stem_dat, stem_dat2)
dim(stem_data)


