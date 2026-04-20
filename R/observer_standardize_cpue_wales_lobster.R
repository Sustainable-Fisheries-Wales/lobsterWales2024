# script for standardizing cpue from the observer sampling of the welsh stock of European lobster (inshore waters only)

# check if required packages are installed
required <- c("TMB", "readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
devtools::install_github("james-thorson/FishStatsUtils@main", INSTALL_opts="--no-staged-install")
devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")

library(VAST)
# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "compute_cpue_wales_lobster.R" )

# read in data
observer_data_lobster <- readr::read_csv("observer_data_lobster_nominal.cpue_potset.csv",
                                         col_types = readr::cols(ices_sub_rect = readr::col_character())) |> 
  dplyr::glimpse()

# reformat datasets
observer_data_orig <- observer_data_lobster
stock <- "lobster"
# select a response variable
observer_data_orig$response <- observer_data_orig$nominal.cpue_potset

observer_data_orig$ices_rect <- mapplots::ices.rect2(observer_data_orig$longitude, observer_data_orig$latitude)
observer_data <- observer_data_orig |> 
  dplyr::filter(!is.na(response)) |> 
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, quarter, latitude, longitude, fisher, ices_rect, response, areaSwept_km2) |>
  dplyr::rename(Lat = latitude,
                Lon = longitude,
                Year = year,
                season = quarter,
                vessel = fisher,
                icesRect = ices_rect,
                cpue_kg_pot = response,
                areaSwept_km2 = areaSwept_km2) |>
  dplyr::glimpse()

# density covaraites - remove NAs for analysis
covariate_data <- observer_data_orig |>
  dplyr::filter(!is.na(response)) |> 
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, latitude, longitude, fisher, month, quarter, ices_rect, nominal.cpue_potset_crab, escape_gaps,
                response, areaSwept_km2, vessel_len) |>
  dplyr::rename(Lat = latitude,
                Lon = longitude,
                Year = year,
                month = month,
                season = quarter,
                vesselSize = vessel_len,
                vessel = fisher,
                cpue_kg_pot = response,
                areaSwept_km2 = areaSwept_km2,
                icesRect = ices_rect,
                escape_gaps = escape_gaps
                ) |>
  dplyr::glimpse()

# catchability covariates - remove NAs for analysis
catchability_data <- observer_data_orig |>
  dplyr::filter(!is.na(response)) |> 
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, latitude, longitude, fisher, month, quarter, ices_rect, nominal.cpue_potset_crab, escape_gaps,
                response, areaSwept_km2, vessel_len) |>
  dplyr::rename(Lat = latitude,
                Lon = longitude,
                Year = year,
                month = month,
                season = quarter,
                vesselSize = vessel_len,
                vessel = fisher,
                cpue_kg_pot = response,
                areaSwept_km2 = areaSwept_km2,
                icesRect = ices_rect,
                escape_gaps = escape_gaps
                ) |>
  dplyr::glimpse()


# plot
# temporal variation
data <- observer_data_orig
response <- data$nominal.cpue_potset
response.name <- "nominal catch rate (kg per number of pots hauled)"

# get estimates
ifish_data_yr <- data |>
  dplyr::filter(!is.infinite(nominal.cpue_potset)) |>
  dplyr::group_by(year) |>
  dplyr::reframe(year = unique(year),
                 fleet = 2,
                 obj = mean(nominal.cpue_potset, na.rm = TRUE),
                 stdrr = sd(nominal.cpue_potset, na.rm = TRUE)/sqrt(length(nominal.cpue_potset))) |>
  dplyr::glimpse()

# plot
ifish_data_yr |> ggplot2::ggplot(ggplot2::aes(x=as.numeric(year), y=obj)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=obj-stdrr, ymax=obj+stdrr), color = "darkblue", 
                         width = 0, alpha=1, position = ggplot2::position_dodge(width = 0.3)) +
  ggplot2::scale_color_hue(l=40, c=35) +   
  ggplot2::geom_point(color = "darkblue", size = 5) + 
  ggplot2::geom_line(color = "darkblue", size = 1) + 
  ggplot2::labs(x = "year", y = "nominal catch rate (kg per N of pots hauled)") +
  ggplot2::theme_classic() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=15),
    axis.title.y = ggplot2::element_text(size=15),	
    axis.text.x = ggplot2::element_text(size=10), 
    axis.text.y = ggplot2::element_text(size=10),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 12),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 15, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))

# spatial distribution
data <- observer_data_orig
response <- data$nominal.cpue_potset
response.name <- "nominal cpue (kg per number of pots hauled)"

# select a dataset and a parameter
xlim <- c(min(data$longitude, na.rm = TRUE)*1.05, max(data$longitude, na.rm = TRUE)*0.85)
ylim <- c(min(data$latitude, na.rm = TRUE)*.995, max(data$latitude, na.rm = TRUE)*1.01)

# wales shape file
wales <- sf::read_sf(dsn = "wnmp_areas.shp", stringsAsFactors = FALSE)

# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)

# read in ICES rectangles for wales
ices_rec <- readr::read_delim(file = "ices_rectangles_wales.csv")
ices_rec_wales <- ices_rec |> 
  dplyr::filter(proportion != 0)
ices_rec_wales <- ices_rec_wales |> 
  janitor::clean_names()

# subset wales
shp_ices.rec_wales <- shp_ices.rec |> 
  dplyr::right_join(ices_rec_wales, by = c("ICESNAME"="ices_rectangle")) |>
  dplyr::mutate(PERCENTAGE = PERCENTAGE*proportion)

# need to make sure CRS is the same!
sf::st_crs(shp_ices.rec_wales)==sf::st_crs(wales)
ices.rec_wales <- sf::st_transform(wales, sf::st_crs(shp_ices.rec_wales))

# crop by counties
hucs_sf_crop <- sf::st_intersection(ices.rec_wales, wales) # warning is ok

xlim <- c(min(shp_ices.rec_wales$WEST, na.rm = TRUE), max(shp_ices.rec_wales$EAST, na.rm = TRUE))
ylim <- c(min(shp_ices.rec_wales$SOUTH, na.rm = TRUE), max(shp_ices.rec_wales$NORTH, na.rm = TRUE))
data <- data |> dplyr::filter((longitude >= min(shp_ices.rec_wales$WEST)) & (longitude <= max(shp_ices.rec_wales$EAST)) & 
                                (latitude >= min(shp_ices.rec_wales$SOUTH)) & (latitude <= max(shp_ices.rec_wales$NORTH)))

(plot2 <- ggplot2::ggplot() +  
   ggplot2::scale_color_manual(values = mycolors) +
   ggplot2::geom_sf(data = wales, ggplot2::aes(fill = name), alpha = 0.2,  colour = "black") +
   ggplot2::geom_sf(data = shp_ices.rec_wales, fill = NA, colour = "darkblue") +
   ggplot2::scale_fill_manual(values = mycolors) +
   ggplot2::geom_point(data=data, ggplot2::aes(x=longitude, y=latitude, size = nominal.cpue_potset), 
                       shape = 21, fill = "darkred", color="white", alpha=I(0.3)) + 
   ggplot2::theme_classic() +
   ggplot2::labs(x = "longitude", y = "latitude") +
   ggplot2::theme( 
     panel.grid.minor = ggplot2::element_blank(), 
     panel.background = ggplot2::element_blank(), 
     axis.line = ggplot2::element_line(colour = "black"),
     axis.title.x = ggplot2::element_text(size=10),
     axis.title.y = ggplot2::element_text(size=10),	
     axis.text.x = ggplot2::element_text(size=8), 
     axis.text.y = ggplot2::element_text(size=8),
     legend.background = ggplot2::element_blank(),
     legend.position = "top",
     legend.title = ggplot2::element_blank(),
     legend.text = ggplot2::element_text(colour="black", size = 8),
     plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
     legend.key = ggplot2::element_blank(),
     strip.background = ggplot2::element_blank(), 
     strip.placement = "outside",
     strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
   ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
   ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
   ggplot2::facet_wrap(~ year, strip.position = "top", ncol = 3))


# Creating an extrapolation gri
library(rgdal) 
shp <- rgdal::readOGR(dsn = "wnmp_areas/wnmp_areas.shp", stringsAsFactors = FALSE)
sps <- spTransform(shp, CRS("+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
lon <- sum(bbox(sps)[1,])/2

# convert decimal degrees to utm zone for average longitude, use for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL
LL <- readRDS('processed_data/wales/user_region.rds')
region_extent <- data.frame(long=LL$Lon+2, lat=LL$Lat)
str(region_extent)

# Create the VAST extroplation grid & Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=", utmzone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

# Construct the extroplation grid for VAST using sf package Size of grid **in meters** (since working in UTM). 
# Controls the resolution of the grid.
cell_size <- 2000
region_grid <- sf::st_make_grid(region_polygon, cellsize = cell_size, what = "centers")

# Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")

# combine shapefile data (region_polygon) with Spatial Points (region_grid_spatial) & place in SpatialPointsDataFrame 
# data (this provides you with your strata identifier (here called Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

# Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL, data.frame(Lon=coords.x1, Lat=coords.x2, fid, Area_km2=( (cell_size/1000)^2), row=1:nrow(region_grid_LL)))

# Filter out the grid that does not overlap (outside extent)
region_df <- region_df |> 
  dplyr::mutate(fid = dplyr::case_when(fid == 2 ~ NA,
                                       fid == 1 ~ 1,
                                       fid == NA ~ NA))
region <- subset(region_df, !is.na(fid))
str(region)

# Save it to be read in and passed to VAST later
saveRDS(region, file = "user_region.rds")

### Quick plots
png('user_region.png', width=7, height=7, units='in', res=200)
par(mfrow=c(2,2))
with(region_extent, plot(long, lat, main='Extent in points in LL'))
plot(region_polygon, main='Polygon in UTM', axes=TRUE)
plot(region_grid, col=ifelse(is.na(region_df$fid), 'red', 'black'),
     axes=TRUE, main='Extrapolation area UTM')
with(region, plot(Lon, Lat, main='Extrapolation region in LL', pch='.'))
dev.off()


# run VAST
dat <- observer_data
settings <- FishStatsUtils::make_settings(n_x = 1000,
                                          FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                                          RhoConfig = c("Beta1" = 4, "Epsilon1" = 2, "Beta2" = 4, "Epsilon2" = 4),
                                          ObsModel = c(4, 0), 
                                          OverdispersionConfig = c("Eta1" = 1, "Eta2" = 1), 
                                          Options = c('treat_nonencounter_as_zero' = FALSE, "report_additional_variables" = TRUE), 
                                          Region = 'User',
                                          purpose = "index2", 
                                          use_anisotropy = TRUE,
                                          bias.correct = FALSE,
                                          fine_scale = TRUE
                                          )
user_region <- readRDS('user_region.rds')
run_dir <- paste0(getwd(),"/vast/wales/", stock, "/observer/base")
dir.create(run_dir,recursive=TRUE)
fit <- NULL
try(fit <- FishStatsUtils::fit_model(settings = settings,
                 Lat_i = dat$Lat, 
                 Lon_i = dat$Lon,
                 t_i = dat$Year, 
                 b_i = dat$cpue_kg_pot,
                 a_i = dat$areaSwept_km2,
                 v_i = dat$vessel,
                 input_grid = user_region,
                 working_dir = run_dir),
    silent = TRUE)

if (exists("fit")) {
  if (!is.null(fit$Report)) {
    print(fit$Report)
    print(fit$parameter_estimates$Convergence_check)
    if (is.list(fit$Report)) {
      try(plot(fit,
               TmbData = fit$data_list,
               settings = settings,
               projargs = '+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0 ',
               n_samples = 0), 
          silent = TRUE)
      break
    }
  }
}
check_fit(fit$parameter_estimates, check_gradients = TRUE, quiet = FALSE)

# Custom maps using ggplot
ggplot2::theme_set(theme_bw())
years <- unique(dat$Year)
years <- 2019:2024
nyrs <- length(years)

# Remake map list locally for recreating plots
mdl <- FishStatsUtils::make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)

# wales shape file
wales <- sf::read_sf(dsn = "wnmp_areas/wnmp_areas.shp", stringsAsFactors = FALSE)
# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)
# read in ICES rectangles for wales
ices_rec <- readr::read_delim(file = "ices_rectangles_wales.csv")
ices_rec_wales <- ices_rec |> 
  dplyr::filter(proportion != 0)
ices_rec_wales <- ices_rec_wales |> 
  janitor::clean_names()

# subset wales
shp_ices.rec_wales <- shp_ices.rec |> 
  dplyr::right_join(ices_rec_wales, by = c("ICESNAME"="ices_rectangle")) |>
  dplyr::mutate(PERCENTAGE = PERCENTAGE*proportion)

# extract vast output
names(fit$Report)[grepl('_gc|_gct', x=names(fit$Report))]
D_gt <- fit$Report$D_gct[,1,] # drop the category
dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years)
D_gt <- D_gt |> as.data.frame() |>
  tibble::rownames_to_column(var = "cell") |>
  tidyr::pivot_longer(-cell, names_to = "Year", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
(plot <- 
    ggplot2::ggplot() +  
    ggplot2::geom_sf(data = wales, ggplot2::aes(fill = name), alpha = 0.2,  colour = "black") +
    ggplot2::geom_point(data=D, ggplot2::aes(x=Lon, y=Lat, color=log(as.vector(D))), stroke=0,shape=16, alpha=I(0.3)) +
    ggplot2::geom_sf(data = shp_ices.rec_wales, fill = NA, colour = "darkblue") +
    ggplot2::scale_color_viridis_c(option = "viridis") +
    ggplot2::theme_bw() +
    ggplot2::theme( 
      panel.grid.minor = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      axis.line = ggplot2::element_line(colour = "black"),
      axis.title.x = ggplot2::element_text(size=10),
      axis.title.y = ggplot2::element_text(size=10),	
      axis.text.x = ggplot2::element_text(size=8), 
      axis.text.y = ggplot2::element_text(size=8),
      legend.background = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour="black", size = 8),
      plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(), 
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
    ggplot2::facet_wrap(~ Year, strip.position = "top", ncol = 3)
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add density & catchability covariates
covariate_data <- covariate_data |> 
  na.omit()
catchability_data <- catchability_data |>
  na.omit()
covariate_data[,'month'] = (covariate_data[,'month'] - mean(unlist(covariate_data[,'month'])))/sd(unlist(covariate_data[,'month']))
catchability_data[,'month'] = (catchability_data[,'month'] - mean(unlist(catchability_data[,'month'])))/sd(unlist(catchability_data[,'month']))
covariate_data[,'nominal.cpue_potset_crab'] = (covariate_data[,'nominal.cpue_potset_crab'] - 
                                                 mean(unlist(covariate_data[,'nominal.cpue_potset_crab'])))/sd(unlist(covariate_data[,'nominal.cpue_potset_crab']))
catchability_data[,'nominal.cpue_potset_crab'] = (catchability_data[,'nominal.cpue_potset_crab'] - 
                                                    mean(unlist(catchability_data[,'nominal.cpue_potset_crab'])))/sd(unlist(catchability_data[,'nominal.cpue_potset_crab']))
covariate_data[,'vesselSize'] = (covariate_data[,'vesselSize'] - 
                                                 mean(unlist(covariate_data[,'vesselSize'])))/sd(unlist(covariate_data[,'vesselSize']))
catchability_data[,'vesselSize'] = (catchability_data[,'vesselSize'] - 
                                                    mean(unlist(catchability_data[,'vesselSize'])))/sd(unlist(catchability_data[,'vesselSize']))

# Define formula
# encounter rate
# catchability
Q1_formula = ~ nominal.cpue_potset_crab + escape_gaps 
# density/habitat
X1_formula = ~ month + nominal.cpue_potset_crab 
Q1config_k = c(3,3) 
X1config_cp = array(3, dim=c(1, 2))

# positive catch rate
# catchability
Q2_formula = ~ nominal.cpue_potset_crab + escape_gaps 
# density/habitat
X2_formula = ~ month + nominal.cpue_potset_crab 
Q2config_k = c(3,3) 
X2config_cp = array(3, dim=c(1, 2))

# make settings
dat <- catchability_data
settings <- FishStatsUtils::make_settings(n_x = 1000, # 1=encounter & 2=density
                                          FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                                          RhoConfig = c("Beta1" = 0, "Epsilon1" = 3, "Beta2" = 2, "Epsilon2" = 2),
                                          ObsModel = c(10, 2), 
                                          OverdispersionConfig = c("Eta1" = 1, "Eta2" = 1), 
                                          Options = c('treat_nonencounter_as_zero' = FALSE, "report_additional_variables" = TRUE), 
                                          Region='User',
                                          purpose="index2", 
                                          use_anisotropy = TRUE,
                                          bias.correct = FALSE,
                                          fine_scale = TRUE)
user_region <- readRDS('user_region.rds')
run_dir <- paste0(getwd(), "/vast/wales/", stock, "/observer/cov")
dir.create(run_dir, recursive = TRUE)

# Run model
fit <- NULL
try(fit <- 
      FishStatsUtils::fit_model(settings = settings,
                                Lat_i = dat$Lat, 
                                Lon_i = dat$Lon,
                                t_i = dat$Year, 
                                b_i = dat$cpue_kg_pot,
                                a_i = dat$areaSwept_km2,
                                v_i = dat$vessel,
                                covariate_data = covariate_data,
                                X1_formula = X1_formula,
                                X1config_cp = X1config_cp,
                                X2_formula = X2_formula,
                                X2config_cp = X2config_cp,
                                catchability_data = catchability_data,
                                Q1_formula = Q1_formula,
                                Q1config_k = Q1config_k,
                                Q2_formula = Q2_formula,
                                Q2config_k = Q2config_k,
                                input_grid = user_region,
                                working_dir = run_dir)
    , silent = TRUE)
if (exists("fit")) {
  if (!is.null(fit$Report)) {
    print(fit$Report)
    print(fit$parameter_estimates$Convergence_check)
    if (is.list(fit$Report)) {
      try(plot(fit,
               TmbData = fit$data_list,
               settings = settings,
               projargs = '+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0 ',
               check_residuals = FALSE,
               n_samples = 0), 
          silent = TRUE)
      fit_out <- c(fit_out, fit)
      fit_nx <- c(fit_nx, i)   
      print(str(fit_out, max.level = 1))
      break
    }
  }
}

# save the fit file
readr::write_rds(fit, file = paste0(run_dir, "vast_fit_cov_wales_lobster.rds")) 

# ggplot
fit <- fit
years <- unique(dat$Year)
nyrs <- length(years)
# Remake map list locally for recreating plots
mdl <- FishStatsUtils::make_map_info(Region = settings$Region,
                                     spatial_list = fit$spatial_list,
                                     Extrapolation_List = fit$extrapolation_list)

# extract vast output
names(fit$Report)[grepl('_gc|_gct', x=names(fit$Report))]
D_gt <- fit$Report$D_gct[,1,] # drop the category
dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years)
D_gt <- D_gt |> as.data.frame() |>
  tibble::rownames_to_column(var = "cell") |>
  tidyr::pivot_longer(-cell, names_to = "Year", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
(plot <- 
    ggplot2::ggplot() +  
    ggplot2::geom_sf(data = wales, ggplot2::aes(fill = name), alpha = 0.2,  colour = "black") +
    ggplot2::geom_sf(data = shp_ices.rec_wales, fill = NA, colour = "darkblue") +
    ggplot2::scale_color_viridis_c(option = "viridis") +
    ggplot2::geom_point(data=D, ggplot2::aes(x=Lon, y=Lat, color=log(as.vector(D))), stroke=0,shape=16, alpha=I(0.9)) + 
    ggplot2::theme_bw() +
    ggplot2::theme( 
      panel.grid.minor = ggplot2::element_blank(), 
      panel.background = ggplot2::element_blank(), 
      axis.line = ggplot2::element_line(colour = "black"),
      axis.title.x = ggplot2::element_text(size=10),
      axis.title.y = ggplot2::element_text(size=10),	
      axis.text.x = ggplot2::element_text(size=8), 
      axis.text.y = ggplot2::element_text(size=8),
      legend.background = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour="black", size = 8),
      plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(), 
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
    ggplot2::facet_wrap(~ Year, strip.position = "top", ncol = 3)
)

library(effects)  
covariate_data_full = fit$effects$covariate_data_full
catchability_data_full = fit$effects$catchability_data_full
pred <- VAST::Effect.fit_model( fit,
                                focal.predictors = c("nominal.cpue_potset_crab"),
                                which_formula = "X1",
                                xlevels = 100,
                                transformation = list(link=identity, inverse=identity) )
plot(pred)

# Make function to interface with pdp
pred.fun <- function( object, newdata ){
  predict( x = object,
           Lat_i = object$data_frame$Lat_i,
           Lon_i = object$data_frame$Lon_i,
           t_i = object$data_frame$t_i,
           a_i = object$data_frame$a_i,
           what = "P1_iz",
           new_covariate_data = newdata,
           do_checks = FALSE )
}

# Run partial
Partial <- pdp::partial( object = fit,
                         pred.var = "nominal.cpue_potset_crab",
                         pred.fun = pred.fun,
                         train = fit$covariate_data )
ggplot2::autoplot(Partial)


#~~~~~~~~~~~~~~~
# Seasonal model
# dataset
dat <- observer_data

# Set of years and seasons
year_set = sort(unlist(unique(dat[,'Year'])))
season_set = c("1", "2", "3", "4")

# Create a grid with all unique combinations of seasons and years and then combine these into one "year_season" variable
yearseason_grid = expand.grid("season" = season_set, "Year" = year_set)
yearseason_levels = apply(yearseason_grid[,2:1], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_labels = round(yearseason_grid[,'Year'] + (as.numeric(factor(yearseason_grid[,'season'],
                                                                        levels = season_set))-1)/length(season_set), digits=1)

# Similar process, but for the observations
yearseason_i = apply(dat[,c("Year","season")], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_i = factor(yearseason_i, levels = yearseason_levels)

# Add the year_season factor column to our sampling_data data set
dat$Year_season = yearseason_i
dat$season = factor(dat$season, levels = season_set)
dat = dat[, c("Year", "season", "Year_season", "Lat", "Lon", "areaSwept_km2", "cpue_kg_pot")]

# Make dummy observation for each season-year combination
dummy_data = data.frame(
  Year = yearseason_grid[,'Year'],
  season = yearseason_grid[,'season'],
  Year_season = yearseason_levels,
  Lat = mean(unlist(dat[,'Lat'])),
  Lon = mean(unlist(dat[,'Lon'])),
  areaSwept_km2 = mean(unlist(dat[,'areaSwept_km2'])),
  cpue_kg_pot = 0,
  dummy = TRUE)

# Combine with sampling data
full_data = rbind(cbind(dat, dummy = FALSE), dummy_data)

# Create sample data
samp_dat = data.frame(
  "year_season" = as.numeric(full_data$Year_season)-1,
  "Lat" = full_data$Lat,
  "Lon" = full_data$Lon,
  "weight" = full_data$cpue_kg_pot,
  "Swept" = full_data$areaSwept_km2,
  "Dummy" = full_data$dummy )

# Covariate data
cov_dat = data.frame(
  "Year" = as.numeric(full_data$Year_season)-1,
  "Year_Cov" = factor(full_data$Year, levels = year_set),
  "Season" = full_data$season,
  "Lat" = full_data$Lat,
  "Lon" = full_data$Lon )
table("Year_season"=cov_dat$Year, "Actual_year"=cov_dat$Year_Cov)
table("Year_season"=cov_dat$Year, "Actual_season"=cov_dat$Season)

# Make settings
settings = FishStatsUtils::make_settings(n_x = 1000,
                                         Region='User',
                                         purpose = "index2",
                                         FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                                         RhoConfig = c("Beta1" = 4, "Beta2" = 4, "Epsilon1" = 4, "Epsilon2" = 4),
                                         ObsModel = c(10, 2),
                                         OverdispersionConfig = c("Eta1" = 1, "Eta2" = 1), 
                                         Options = c('treat_nonencounter_as_zero' = FALSE, "report_additional_variables" = TRUE), 
                                         use_anisotropy = TRUE,
                                         bias.correct = FALSE,
                                         fine_scale = TRUE)

# Creating model formula
formula_use = ~ Season + Year_Cov

# Implement corner constraint for linear effect but not spatially varying effect:
# * one level for each term is 2 (just spatially varying)
# * all other levels for each term is 3 (spatialy varying plus linear effect)
X2config_cp_use = matrix( c(2, rep(3, nlevels(cov_dat$Season)-1), 2, rep(3,nlevels(cov_dat$Year_Cov)-1) ), nrow=1 )

# Model fit -- make sure to use new functions
user_region <- readRDS('user_region.rds')
run_dir <- paste0(getwd(),"/vast/wales/", stock, "/observer/season")
dir.create(run_dir,recursive=TRUE)
fit_orig <- FishStatsUtils::fit_model("settings" = settings,
                                      "Lat_i" = samp_dat[, 'Lat'],
                                      "Lon_i" = samp_dat[, 'Lon'],
                                      "t_i" = samp_dat[, 'year_season'],
                                      "b_i" = samp_dat[, 'weight'],
                                      "a_i" = samp_dat[, 'Swept'],
                                      #"X1config_cp" = X1config_cp_use,
                                      "X2config_cp" = X2config_cp_use,
                                      "covariate_data" = cov_dat,
                                      #"X1_formula" = ~ 1,
                                      "X2_formula" = formula_use,
                                      "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), 
                                                           Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
                                      "run_model" = FALSE,
                                      "PredTF_i" = samp_dat[, 'Dummy'],
                                      input_grid = user_region,
                                      working_dir = run_dir)

# Adjust mapping for log_sigmaXi and fitting final model -- pool variance for all seasons and then set year's to NA
Map_adjust = fit_orig$tmb_list$Map

# Pool variances for each term to a single value
Map_adjust$log_sigmaXi2_cp = factor(c(rep(as.numeric(Map_adjust$log_sigmaXi2_cp[1]), nlevels(cov_dat$Season)),
                                      rep(as.numeric(Map_adjust$log_sigmaXi2_cp[nlevels(cov_dat$Season)+1]), nlevels(cov_dat$Year_Cov))))

# Fit final model with new mapping
try(fit <- 
      FishStatsUtils::fit_model("settings" = settings,
                                "Lat_i" = samp_dat[, 'Lat'],
                                "Lon_i" = samp_dat[, 'Lon'],
                                "t_i" = samp_dat[, 'year_season'],
                                "b_i" = samp_dat[, 'weight'],
                                "a_i" = samp_dat[, 'Swept'],
                                #"X1config_cp" = X1config_cp_use,
                                "X2config_cp" = X2config_cp_use,
                                "covariate_data" = cov_dat,
                                #"X1_formula" = formula_use,
                                "X2_formula" = formula_use,
                                "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), 
                                                     Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
                                "newtonsteps" = 0,
                                "PredTF_i" = samp_dat[, 'Dummy'],
                                "Map" = Map_adjust,
                                input_grid = user_region,
                                working_dir = run_dir)
    , silent = TRUE)
if (exists("fit")) {
  if (!is.null(fit$Report)) {
    print(fit$Report)
    if (is.list(fit$Report)) {
      if (fit$parameter_estimates$Convergence_check == "There is no evidence that the model is not converged") {
        try(
          plot(fit,
               TmbData = fit$data_list,
               settings = settings,
               projargs = '+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0 ',
               n_samples = 0)
          , silent = TRUE)
        fit_out <- c(fit_out, fit)
        fit_nx <- c(fit_nx, i)   
        print(str(fit_out, max.level = 1))
        break
      }
    }
  }
}
