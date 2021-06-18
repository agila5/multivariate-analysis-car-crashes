# Source previous steps ---------------------------------------------------
source("R/utils.R")
source("R/packages.R")

# Load road network data --------------------------------------------------
leeds_road_network_merged_nomis <- st_read(
  "data/leeds_road_network_merged_nomis.gpkg",
  quiet = TRUE,
  stringsAsFactors = FALSE
)

# Contract the road network using dodgr -----------------------------------
leeds_road_network_dodgr <- leeds_road_network_merged_nomis %>%
  st_transform(crs = 4326) %>%
  weight_streetnet(wt_profile = 1, keep_cols = c("CLASSIFICA", "is_dual_carriageway")) %>%
  dodgr_centrality(contract = TRUE)
clear_dodgr_cache()
leeds_road_network_contracted <- leeds_road_network_dodgr %>%
  dodgr_to_sf() %>%
  st_transform(crs = 27700)
leeds_road_network_contracted$edge_betweenness <- as.vector(scale(leeds_road_network_contracted$centrality))

# Keep only relevant columns
leeds_road_network_contracted <- leeds_road_network_contracted[, c(12, 13, 17, 20)]
leeds_road_network_contracted$road_length <- units::drop_units(st_length(leeds_road_network_contracted))

# For simplicity, we just load the other covariates
contracted_covariates <- read.table("data/contracted-covariates.txt", header = TRUE)
leeds_road_network_contracted_nomis <- st_sf(
  cbind(leeds_road_network_contracted %>% st_drop_geometry(), contracted_covariates),
  geometry = st_geometry(leeds_road_network_contracted)
)

# Prepare data for INLA ---------------------------------------------------
# Build W
W_contracted <- as_adjacency_matrix(graph_from_adj_list(st_touches(leeds_road_network_contracted_nomis)))

# Build my_data v2
my_data_v2 <- data.frame(
  # y
  number_of_car_crashes = c(
    leeds_road_network_contracted_nomis[["number_severe_crashes"]],
    leeds_road_network_contracted_nomis[["number_slight_crashes"]]
  ),
  # offsets
  total_traffic = rep(leeds_road_network_contracted_nomis[["total_traffic"]], 2),
  road_length = rep(leeds_road_network_contracted_nomis[["road_length"]], 2),
  # intercept
  intercept_severe = c(rep(1L, nrow(W_contracted)), rep(NA_integer_, nrow(W_contracted))),
  intercept_slight = c(rep(NA_integer_, nrow(W_contracted)), rep(1L, nrow(W_contracted))),
  # covariates
  edge_betweenness_severe = c(
    leeds_road_network_contracted_nomis[["edge_betweenness"]],
    rep(NA, nrow(leeds_road_network_contracted_nomis))
  ),
  edge_betweenness_slight = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    leeds_road_network_contracted_nomis[["edge_betweenness"]]
  ),
  CLASSIFICA_severe_motorway = c(
    ifelse(leeds_road_network_contracted_nomis[["CLASSIFICA"]] == "Motorway", 1L, 0L),
    rep(NA_integer_, nrow(leeds_road_network_contracted_nomis))
  ),
  CLASSIFICA_severe_primary_road = c(
    ifelse(leeds_road_network_contracted_nomis[["CLASSIFICA"]] == "Primary Road", 1L, 0L),
    rep(NA_integer_, nrow(leeds_road_network_contracted_nomis))
  ),
  CLASSIFICA_slight_motorway = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    ifelse(leeds_road_network_contracted_nomis[["CLASSIFICA"]] == "Motorway", 1L, 0L)
  ),
  CLASSIFICA_slight_primary_road = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    ifelse(leeds_road_network_contracted_nomis[["CLASSIFICA"]] == "Primary Road", 1L, 0L)
  ),
  perc_lav_severe = c(
    scale(leeds_road_network_contracted_nomis[["perc_lav"]]),
    rep(NA, nrow(leeds_road_network_contracted_nomis))
  ),
  perc_lav_slight = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    scale(leeds_road_network_contracted_nomis[["perc_lav"]])
  ),
  dens_pop_severe = c(
    scale(leeds_road_network_contracted_nomis[["dens_pop"]]),
    rep(NA, nrow(leeds_road_network_contracted_nomis))
  ),
  dens_pop_slight = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    scale(leeds_road_network_contracted_nomis[["dens_pop"]])
  ),
  dual_carriageway_severe = c(
    as.numeric(leeds_road_network_contracted_nomis[["is_dual_carriageway"]]),
    rep(NA, nrow(leeds_road_network_contracted_nomis))
  ),
  dual_carriageway_slight = c(
    rep(NA, nrow(leeds_road_network_contracted_nomis)),
    as.numeric(leeds_road_network_contracted_nomis[["is_dual_carriageway"]])
  ),
  # params
  stringsAsFactors = FALSE
)

# Define ids for random effects
my_data_v2$idx <- seq_len(nrow(my_data_v2))
my_data_v2$iid2d <- seq_len(nrow(my_data_v2))

# Define the spatial random effect
my_effect <- inla.MCAR.model(k = 2, W = W_contracted, alpha.min = 0, alpha.max = 1)

model_F_contracted <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid2d, model = "iid2d", n = nrow(my_data_v2)) +
    f(idx, model = my_effect),
  family = "poisson",
  data = my_data_v2,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.inla = list(h = 0.005, stupid.search.max.iter = 2000),
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)
