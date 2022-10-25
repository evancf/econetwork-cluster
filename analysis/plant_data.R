# Packages and functions
# install.packages("BIEN")
library("BIEN")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

devtools::source_gist("71d758f65261a72ab7dc") # gist for ipak



# Plot approach ----------------------------------------------------------------

# BIEN_plot_list_sampling_protocols() %>% View()

# # For some reason, this misses a few plots...
# plot_data_old <- BIEN_plot_sampling_protocol(sampling_protocol = BIEN_plot_list_sampling_protocols()[[1]][7],
#                                             new.world = T)
# plot_data_old2 <- BIEN_plot_sampling_protocol(sampling_protocol = BIEN_plot_list_sampling_protocols()[[1]][7])
# plot_dat_old <- bind_rows(plot_data_old, plot_data_old2)

#
md <- BIEN_plot_metadata()

plots_to_use <- md %>% filter(sampling_protocol %in% c("0.1 ha  transect, stems >= 2.5 cm dbh",
                                       "0.1 ha  transect, stems >= 2.5 cm dbh (Warning: species + count of individuals + stem DBHs recorded once per subplot. Individuals not linked to stems)")) %>% 
  pull(plot_name)

plot_data <- BIEN_plot_name(plots_to_use)

# # Just making sure these are all the plots available...
# unique(plot_data$plot_name) %>% length()
# missed_plots <- plots_to_use[!plots_to_use %in% unique(plot_data$plot_name)]
# missed_metadata <- md %>% filter(plot_name %in% missed_plots)
# missed_datasources <- missed_metadata %>% pull(datasource) %>% unique()
# missed_plot_data <- BIEN_plot_datasource(missed_datasources)
# missed_plot_data <- missed_plot_data %>% 
#   filter(sampling_protocol == "0.1 ha  transect, stems >= 2.5 cm dbh")
# table(missed_plot_data$plot_name %in% plot_data$plot_name)

# # Could try pulling in other / more data
# plot_data2 <- BIEN_plot_datasource(datasource = "SALVIAS")
# plot_data2 <- plot_data2 %>% filter(sampling_protocol == "0.1 ha  transect, stems >= 2.5 cm dbh")
# 
# #md %>% filter(sampling_protocol == "0.1 ha  transect, stems >= 2.5 cm dbh (Warning: species + count of individuals + stem DBHs recorded once per subplot. Individuals not linked to stems)")
# 
# dim(plot_data2)
# unique(plot_data2$plot_name) %>% length()
# 
# table(plot_data2$plot_name %in% plot_data$plot_name)
# 
# md <- BIEN_plot_metadata()
# 
# table(md$sampling_protocol)
# 
# my_plots <-  md %>% filter(sampling_protocol %in% c("0.1 ha  transect, stems >= 2.5 cm dbh",
#                                        "0.1 ha  transect, stems >= 2.5 cm dbh (Warning: species + count of individuals + stem DBHs recorded once per subplot. Individuals not linked to stems)"))
# 
# write.csv(my_plots %>% dplyr::select(plot_name), file = "./data/my_plots.csv")
# 
# plot_data3 <- BIEN_plot_name(plot.name = my_plots$plot_name,
#                              new.world = T)
# 
# dim(plot_data3)
# unique(plot_data3$plot_name) %>% length()

plot_summary <- plot_data %>% 
  group_by(plot_name) %>% 
  summarise(latitude = mean(latitude, na.rm = T),
            longitude = mean(longitude, na.rm = T))

dim(plot_summary) # I thought there were 620 plots?


# Map these plots
world <- ne_countries(scale = "medium", 
                      returnclass = "sf")

# americas <- world %>% 
#   filter(region_un == "Americas") %>% 
#   st_cast("POLYGON")
# 
# polys_to_rm <- st_coordinates(americas) %>% 
#   as.data.frame() %>% group_by(L2) %>% 
#   summarize(long_test = any(X > 80)) %>% 
#   pull(long_test)
# 
# americas <- americas[!polys_to_rm,]
# 
# americas <- americas %>% 
#   st_transform(crs = projcrs)

projcrs <- "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"

plot_coords <- st_as_sf(plot_summary,
                             coords = c("longitude", "latitude"),
                             crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot_coords <- plot_coords  %>% 
  st_transform(crs = projcrs)


png(file = "./visualization/392 sites.png", width = 4, height = 4, units = "in",
    res = 440)

#ggplot(data = americas) +
ggplot(data = world) +
  geom_sf(lwd = 0) +
  theme_void() +
  #coord_sf(xlim = c(-180, 80), ylim = c(-60, 32), expand = FALSE) +
  geom_sf(data = plot_coords, size = 0.15) #+
  #scale_x_continuous(limits = c(-180, 60))

dev.off()


# # Species range approaches -----------------------------------------------------
# 
# # Could get species lists for individual points on the planet. BUT I think
# # these ranges aren't super great.
# test_box <- BIEN_ranges_box(
#   min.lat = 47,
#   max.lat = 48,
#   min.long = -123,
#   max.long = -122,
#   species.names.only = T)


# Prepare to join with BIEN data. First want to reshape the other data

# Here's a previous assembly of both TRY data and fruit trait data 
# from Sinnott-Armstrong fruit trait database from another project
plant_trait_wide <- read.csv("/Users/evanfricke/Dropbox/*Science/*Research/*SESYNC/1 Predicting interactions/Data/Traits/tidy/plant.trait.wide.out.csv", 
                             header = T, row.names = 1)

# Pivot longer for tidy format
plant_traits <- plant_trait_wide %>% 
  dplyr::rename(scrubbed_species_binomial = species) %>% 
  pivot_longer(cols = 3:13,
               names_to = "trait_name",
               values_to = "trait_value") %>% 
  filter(!is.na(trait_value))


# Lets make sure the units are the same. Will go back to an original TRY 
# release to make sure the units are what we expect.

# try.db <- data.table::fread(file = "/Users/evanfricke/Dropbox/*Science/*Research/*SESYNC/1 Predicting interactions/Data/Traits/raw/TRY/6743.txt",
#                        header = T, 
#                        sep = "\t")
# try.db <- subset(try.db, TraitName != "")
# 
# 
# table(try.db$TraitName)
# 
# mode_unit <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# try.db %>% group_by(TraitName) %>% 
#   summarize(unit = mode_unit(UnitName))

# # A tibble: 12 × 2
# TraitName                                                                                               unit    
# <chr>                                                                                                   <chr>   
#   1 Dispersal unit length                                                                                   mm      
# 2 Dispersal unit thickness                                                                                mm      
# 3 Dispersal unit width                                                                                    mm      
# 4 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded                        mm2 mg-1
# 5 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included                        mm2 mg-1
# 6 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded mm2 mg-1
# 7 Plant height vegetative                                                                                 m       
# 8 Seed dry mass                                                                                           mg      
# 9 Seed length                                                                                             mm      
# 10 Seed thickness                                                                                          mm      
# 11 Seed width                                                                                              mm      
# 12 Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)                       g/cm3   

# Make a little tibble to help translate between TRY and BIEN data
unit_translator <- tibble(try_trait_name = unique(plant_traits$trait_name)) %>% 
  mutate(unit = c("m2.kg-1",
                  "m",
                  "g.cm-3",
                  "mg",
                  "mm",
                  "mm",
                  "mm",
                  "mm",
                  "mm",
                  "mm",
                  "mm")) %>% 
  mutate(bien_trait_name = c("leaf area per leaf dry mass",
                             "maximum whole plant height",
                             "stem wood density",
                             "seed mass",
                             "seed length",
                             NA,
                             NA,
                             NA,
                             NA,
                             NA,
                             "fruit length"))


# Download the BIEN data -------------------------------------------------------

BIEN_trait_list()

b_leaf_area_per_leaf_dry_mass <- BIEN_trait_trait(trait = "leaf area per leaf dry mass") %>% 
  filter(log(as.numeric(trait_value)) > 0) # There are some problem low measurements in here
# Note that it's represented in m2.kg-1, but that's equivalent to mm2 mg-1
b_maximum_whole_plant_height <- BIEN_trait_trait(trait = "maximum whole plant height")
b_stem_wood_density <- BIEN_trait_trait(trait = "stem wood density") %>% 
  filter(log(as.numeric(trait_value)) > -3) # There are some problem low measurements in here
b_seed_mass <- BIEN_trait_trait(trait = "seed mass")
b_seed_length <- BIEN_trait_trait(trait = "seed length")
b_fruit_length <- BIEN_trait_trait(trait = "maximum fruit length") %>% 
  bind_rows(BIEN_trait_trait(trait = "minimum fruit length")) %>% 
  mutate(trait_name = "fruit length")
b_whole_plant_growth_form <- BIEN_trait_trait(trait = "whole plant growth form")

# Fix up whole plant growth form

# First, just clean up little odd things
b_whole_plant_growth_form$trait_value <- b_whole_plant_growth_form$trait_value %>% 
  trimws() %>% 
  tolower() %>% 
  gsub("_", " ", ., fixed = T) %>% 
  gsub("*", " ", ., fixed = T) %>% 
  gsub(",", " ", ., fixed = T) %>%
  gsub("(", " ", ., fixed = T) %>%
  gsub(")", " ", ., fixed = T) %>%
  gsub("/", " ", ., fixed = T) %>%
  gsub("-", " ", ., fixed = T) %>%
  gsub("  ", " ", ., fixed = T) %>% 
  gsub("  ", " ", ., fixed = T) %>% 
  gsub(" plant", " ", ., fixed = T) %>%
  trimws()

# Note that this project focuses on species that are >2.5 cm dbh, so
# I will not do much on growth forms that are typically smaller than that
b_whole_plant_growth_form$trait_value %>% table() %>% sort()

# Combine some common categories
b_whole_plant_growth_form$trait_value <- b_whole_plant_growth_form$trait_value %>% 
  recode(shub = "shrub",
         shurb = "shrub",
         shrug = "shrub",
         shrublet = "shrub",
         treelet = "tree",
         epiphytic = "epiphyte",
         hemiepiphyte = "epiphyte", # Same principle as below
         grass = "graminoid",
         sedge = "graminoid",
         forb = "herb",
         "woody climber" = "liana",
         "woody vine" = "liana",
         "herbaceous climber" = "vine")

# Next principle will be to take the last word as the most basic descriptor
b_whole_plant_growth_form$trait_value <- b_whole_plant_growth_form$trait_value %>% 
  word(-1)

# Now subset just to the common ones
b_whole_plant_growth_form <- b_whole_plant_growth_form %>% 
  filter(trait_value %in% c("tree", "herb", "liana",
                           "shrub", "graminoid", "epiphyte",
                           "vine", "climber", "fern",
                           "parasite"))


# Tree, shrub, herb, epiphyte, vine, liana,
# Herb


# Checking that all the units are the same here - yes they are!
b_leaf_area_per_leaf_dry_mass$unit %>% table()
b_maximum_whole_plant_height$unit %>% table()
b_stem_wood_density$unit %>% table()
b_seed_mass$unit %>% table()
b_seed_length$unit %>% table()
b_fruit_length$unit %>% table()
b_fruit_length$unit %>% table()

# Get a sense for outliers
b_leaf_area_per_leaf_dry_mass$trait_value %>% as.numeric()  %>% log()  %>% hist()
b_maximum_whole_plant_height$trait_value %>% as.numeric()  %>% log()  %>% hist()
b_stem_wood_density$trait_value %>% as.numeric()  %>% log()  %>% hist()
b_seed_mass$trait_value %>% as.numeric()  %>% log()  %>% hist()
b_seed_length$trait_value %>% as.numeric()  %>% log()  %>% hist()
b_fruit_length$trait_value %>% as.numeric()  %>% log()  %>% hist()

# There are a few non-numeric trait values. I think we can just omit these as NAs
b_maximum_whole_plant_height$trait_value[which(is.na(as.numeric(b_maximum_whole_plant_height$trait_value)))]
b_seed_mass$trait_value[which(is.na(as.numeric(b_seed_mass$trait_value)))]
b_fruit_length$trait_value[which(is.na(as.numeric(b_fruit_length$trait_value)))]


# Put together all the Bien trait data

bien_plant_traits <- bind_rows(b_leaf_area_per_leaf_dry_mass,
                               b_maximum_whole_plant_height,
                               b_stem_wood_density,
                               b_seed_mass,
                               b_seed_length,
                               b_fruit_length) %>% 
  mutate(trait_value = as.numeric(trait_value)) %>% # Warning is expected
  filter(!is.na(trait_value)) %>% # Remove the non-numeric values from the numeric traits
  mutate(trait_value = as.character(trait_value)) %>% 
  bind_rows(b_whole_plant_growth_form) %>% 
  left_join(dplyr::select(unit_translator, -unit),
            by = c("trait_name" = "bien_trait_name")) %>% 
  mutate(try_trait_name = ifelse(trait_name == "whole plant growth form",
                                 "whole.plant.growth.form",
                                 try_trait_name)) %>% 
  dplyr::select(-trait_name) %>% 
  mutate(trait_name = try_trait_name) %>% 
  dplyr::select(-try_trait_name)


# Join TRY and BIEN data

plant_traits <- plant_traits %>% 
  mutate(trait_value = as.character(trait_value)) %>%
  left_join(dplyr::select(unit_translator, -bien_trait_name),
            by = c("trait_name" = "try_trait_name")) %>%
  bind_rows(bien_plant_traits)



# # Could consider including some others here, but there's really limited data
# fruit type
# maximum fruit length
# minimum fruit length
# plant fruiting duration
# whole plant dispersal syndrome
# 
# b_asdf <- BIEN_trait_trait(trait = "maximum fruit length")
# b_fdsa <- BIEN_trait_trait(trait = "minimum fruit length")
# 
# dim(b_asdf)
# dim(b_fdsa)
# 
# b_asdf$scrubbed_species_binomial %in% b_fdsa$scrubbed_species_binomial %>% table()
# 
# asdf <- b_asdf %>% 
#   dplyr::select(scrubbed_species_binomial, trait_value) %>% 
#   left_join(b_fdsa, by = "scrubbed_species_binomial")
# 
# plot(asdf$trait_value.x ~ asdf$trait_value.y)
# curve(x * 1, add = T, col = "blue")






# Getting a sense for plant species trait coverage -----------------------------

plant_traits <- plant_traits %>% 
  mutate(genus = word(scrubbed_species_binomial, 1))

# # There shouldn't be NAs at this point, but can use this...
# median_na_rm <- function(x){
#   if(all(is.na(x))){
#     return(NA)
#   } else{
#     return(median(x, na.rm = T))
#   }
# }

my_typical <- function(x){
  x_num <- as.numeric(x)
  if(all(is.na(x_num))){
    return(modelr::typical(x)[1]) 
    # This is a decision to simply pick the first one when there is a tie
    # for the most common category
  } else{
    return(as.character(modelr::typical(x_num)))
  }
}

# # An example of how it's necessary to pick one when there's a tie
# asdf <- filter(plant_traits, scrubbed_species_binomial == "Abelmoschus splendens")
# asdf
# asdf %>% 
#   group_by(scrubbed_species_binomial, trait_name) %>% 
#   summarise(trait_value_species_median = my_typical(trait_value))

plant_trait_species_median <- plant_traits %>% 
  group_by(scrubbed_species_binomial, trait_name) %>% 
  dplyr::summarise(trait_value_species_median = my_typical(trait_value))

head(plant_trait_species_median)

plant_trait_genus_median <- plant_traits %>% 
  group_by(genus, trait_name) %>% 
  dplyr::summarise(trait_value_genus_median = my_typical(trait_value))

head(plant_trait_genus_median)

# Join to see what's available at the species and genus level

# Need to first pivot wider

plant_trait_species_median_wide <- plant_trait_species_median %>%
  pivot_wider(id_cols = 1,
              names_from = trait_name,
              values_from = trait_value_species_median,
              names_prefix = "sp_med_")

plant_trait_genus_median_wide <- plant_trait_genus_median %>%
  pivot_wider(id_cols = 1,
              names_from = trait_name,
              values_from = trait_value_genus_median,
              names_prefix = "gen_med_")


# Join to plot data
plot_data_trait <- plot_data %>% 
  left_join(plant_trait_species_median_wide, 
            by = "scrubbed_species_binomial") %>% 
  mutate(genus = word(scrubbed_species_binomial, 1)) %>%
  left_join(plant_trait_genus_median_wide, by = "genus")

# Lastly, get fleshy fruitedness

fleshy_dat <- read.csv("./data/fleshy_data.csv", header = T, row.names = 1)

(fleshy_dat$scrubbed_species_binomial %in% plot_data_trait$scrubbed_species_binomial) %>% table()
(plot_data_trait$scrubbed_species_binomial %in% fleshy_dat$scrubbed_species_binomial) %>% table()

plot_data_trait <- plot_data_trait %>% 
  left_join(fleshy_dat)


# Get coverage that's weighted by individuals
coverage_abund_weighted <- plot_data_trait %>% 
  filter(fleshy == 1) %>% 
  summarize_all(function(x) mean(!is.na(x)))

# Or coverage just at the species level
coverage_sp_level <- plot_data_trait[!duplicated(plot_data_trait$scrubbed_species_binomial),] %>% 
  filter(fleshy == 1) %>% 
  summarize_all(function(x) mean(!is.na(x)))

colnames(coverage_abund_weighted)



# Let's get a sense for coverage visually --------------------------------------

sp_med_trait_cols <- 18:29
gen_med_trait_cols <- 31:42
trait_cols <- c(sp_med_trait_cols,
                gen_med_trait_cols)

trait_names_formatted <- colnames(coverage_abund_weighted) %>% 
  gsub("sp_med_", "", .) %>% 
  gsub("gen_med_", "", .) %>% 
  gsub(".", " ", ., fixed = T) %>% 
  gsub("_", " ", ., fixed = T)


png(file = "./visualization/trait coverage.png", width = 8, height = 8, units = "in",
    res = 440)

par()$mar
par(mar = c(7.1, 4.1,2.1, 2.1),
    mfrow = c(2,1))
plot(NA,
     xlim = c(min(sp_med_trait_cols) - 1,
              max(gen_med_trait_cols) + 1),
     ylim = c(0, 1),
     xlab = "",
     ylab = "Abundance-weighted Coverage",
     frame = F,
     xaxt = "n",
     las = 1)

segments(x0 = trait_cols,
         y0 = 0, 
         y1 = coverage_abund_weighted[trait_cols] %>% as.numeric(),
         lwd = 10,
         col = rep(c("lightgrey", "lightblue"), each = length(trait_cols)/2),
         lend = "butt")

text(trait_cols+0.5, -0.02, trait_names_formatted[trait_cols],
     srt = 45,
     pos = 2,
     xpd = T)

legend("topright",legend = c("Species-level", "Genus-level"),
       fill = c("lightgrey", "lightblue"),
       #pch = 15,
       bty = "n")



# Species coverage
plot(NA,
     xlim = c(min(sp_med_trait_cols) - 1,
              max(gen_med_trait_cols) + 1),
     ylim = c(0, 1),
     xlab = "",
     ylab = "Species Coverage",
     frame = F,
     xaxt = "n",
     las = 1)

segments(x0 = trait_cols,
         y0 = 0, 
         y1 = coverage_sp_level[trait_cols] %>% as.numeric(),
         lwd = 10,
         col = rep(c("lightgrey", "lightblue"), each = length(trait_cols)/2),
         lend = "butt")

text(trait_cols+0.5, -0.02, trait_names_formatted[trait_cols],
     srt = 45,
     pos = 2,
     xpd = T)

legend("topright",legend = c("Species-level", "Genus-level"),
       fill = c("lightgrey", "lightblue"),
       #pch = 15,
       bty = "n")

dev.off()


# Write this out

write.csv(plot_data_trait, file = "./data/plot_data_trait.csv")
