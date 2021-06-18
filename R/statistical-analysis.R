# Source previous steps ---------------------------------------------------
source("R/utils.R")
source("R/packages.R")

# Load data ---------------------------------------------------------------
leeds_road_network_merged_nomis <- st_read("data/leeds_road_network_merged_nomis.gpkg", quiet = TRUE, stringsAsFactors = FALSE)

# Prepare data for INLA ---------------------------------------------------
# Define first order neighborhood matrix
W <- as_adjacency_matrix(graph_from_adj_list(st_touches(leeds_road_network_merged_nomis)))

# Prepare data for INLAMSM
my_data <- data.frame(
  # y
  number_of_car_crashes = c(
    leeds_road_network_merged_nomis[["number_severe_crashes"]],
    leeds_road_network_merged_nomis[["number_slight_crashes"]]
  ),
  # offsets
  total_traffic = rep(leeds_road_network_merged_nomis[["total_traffic"]], 2),
  road_length = rep(leeds_road_network_merged_nomis[["road_length"]], 2),
  # intercept
  intercept_severe = c(rep(1L, nrow(W)), rep(NA_integer_, nrow(W))),
  intercept_slight = c(rep(NA_integer_, nrow(W)), rep(1L, nrow(W))),
  # covariates
  edge_betweenness_severe = c(
    leeds_road_network_merged_nomis[["edge_betweenness"]],
    rep(NA, nrow(leeds_road_network_merged_nomis))
  ),
  edge_betweenness_slight = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    leeds_road_network_merged_nomis[["edge_betweenness"]]
  ),
  CLASSIFICA_severe_motorway = c(
    ifelse(leeds_road_network_merged_nomis[["CLASSIFICA"]] == "Motorway", 1L, 0L),
    rep(NA_integer_, nrow(leeds_road_network_merged_nomis))
  ),
  CLASSIFICA_severe_primary_road = c(
    ifelse(leeds_road_network_merged_nomis[["CLASSIFICA"]] == "Primary Road", 1L, 0L),
    rep(NA_integer_, nrow(leeds_road_network_merged_nomis))
  ),
  CLASSIFICA_slight_motorway = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    ifelse(leeds_road_network_merged_nomis[["CLASSIFICA"]] == "Motorway", 1L, 0L)
  ),
  CLASSIFICA_slight_primary_road = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    ifelse(leeds_road_network_merged_nomis[["CLASSIFICA"]] == "Primary Road", 1L, 0L)
  ),
  perc_lav_severe = c(
    scale(leeds_road_network_merged_nomis[["perc_lav"]]),
    rep(NA, nrow(leeds_road_network_merged_nomis))
  ),
  perc_lav_slight = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    scale(leeds_road_network_merged_nomis[["perc_lav"]])
  ),
  dens_pop_severe = c(
    scale(leeds_road_network_merged_nomis[["dens_pop"]]),
    rep(NA, nrow(leeds_road_network_merged_nomis))
  ),
  dens_pop_slight = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    scale(leeds_road_network_merged_nomis[["dens_pop"]])
  ),
  dual_carriageway_severe = c(
    as.numeric(leeds_road_network_merged_nomis[["is_dual_carriageway"]]),
    rep(NA, nrow(leeds_road_network_merged_nomis))
  ),
  dual_carriageway_slight = c(
    rep(NA, nrow(leeds_road_network_merged_nomis)),
    as.numeric(leeds_road_network_merged_nomis[["is_dual_carriageway"]])
  ),
  # params
  stringsAsFactors = FALSE
)

# Define ids for random effects
my_data$iid_severe <- c(seq_len(nrow(W)), rep(NA_real_, nrow(W)))
my_data$iid_slight <- c(rep(NA_real_, nrow(W)), seq_len(nrow(W)))
my_data$idx <- seq_len(nrow(my_data))
my_data$iid2d <- seq_len(nrow(my_data))

# Define the sum-to-zero constraints
k <- 2
A <- kronecker(Diagonal(k, 1), Matrix(1, ncol = nrow(W), nrow = 1))
e <- rep(0, k)

# Train model A -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.INDIMCAR.model(k = k, W = W)

# Run the model
model_A <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid_severe) + f(iid_slight) +
    f(
      idx,
      model = my_effect,
      extraconstr = list(A = as.matrix(A), e = e)
    ),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.mode = list(theta = c(4.5, 0.5, 1.5, 1.5), restart = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model B -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.INDMCAR.model(k = k, W = W, alpha.min = 0, alpha.max = 1)

# Run the model
model_B <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid_severe) + f(iid_slight) +
    f(idx, model = my_effect),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.inla = list(h = 0.05),
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model C -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.IMCAR.model(k = k, W = W)

# Run the model
model_C <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid_severe) + f(iid_slight) +
    f(
      idx,
      model = my_effect,
      extraconstr = list(A = as.matrix(A), e = e)
    ),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model D -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.MCAR.model(k = k, W = W, alpha.min = 0, alpha.max = 1)

# Run the model
model_D <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid_severe) + f(iid_slight) +
    f(idx, model = my_effect),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.inla = list(h = 0.005, restart = 1),
  control.mode = list(theta = c(4, 2, 5, 2, 2, 3), restart = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model E -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.IMCAR.model(k = k, W = W)

# Run the model
model_E <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid2d, model = "iid2d", n = nrow(my_data)) +
    f(
      idx,
      model = my_effect,
      extraconstr = list(A = as.matrix(A), e = e)
    ),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model F -----------------------------------------------------------
# Define the spatial random effect
my_effect <- inla.MCAR.model(k = k, W = W, alpha.min = 0, alpha.max = 1)

# Run the model
model_F <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid2d, model = "iid2d", n = nrow(my_data)) +
    f(idx, model = my_effect),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)

# Train model G -----------------------------------------------------------

# Define the spatial random effect
my.MCAR.model_a1_ak_Lambda <- function(...) {
  INLA::inla.rgeneric.define(model = my.inla.rgeneric.MCAR.model_a1_ak_Lambda, lambda_wish = 1, ...)
}
my_effect <- my.MCAR.model_a1_ak_Lambda(k = k, W = W, alpha.min = 0, alpha.max = 1)

# Run the model
model_G <- inla(
  formula = number_of_car_crashes ~ 0 +
    intercept_severe + intercept_slight +
    edge_betweenness_severe + edge_betweenness_slight +
    CLASSIFICA_severe_motorway + CLASSIFICA_severe_primary_road +
    CLASSIFICA_slight_motorway + CLASSIFICA_slight_primary_road +
    perc_lav_severe + perc_lav_slight +
    dens_pop_severe + dens_pop_slight +
    dual_carriageway_severe + dual_carriageway_slight +
    f(iid2d, model = "iid2d", n = nrow(my_data)) +
    f(idx, model = my_effect),
  family = "poisson",
  data = my_data,
  E = road_length * total_traffic,
  verbose = TRUE,
  control.compute = list(dic = TRUE, waic = TRUE),
  control.predictor = list(compute = TRUE)
)
