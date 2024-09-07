rm(list = ls())
library(data.table)
library(tidyverse)
theme_set(theme_bw())

correlation_scenarios <- data.frame(
  "med_neg_corr" =
    c(-0.5, -0.5, -0.5),
  "low_neg_corr" =
    c(-0.1, -0.1, -0.1),
  # "high_neg_corr" =    # results in Sigma that is not positive semi-definite
  #   c( -0.8, -0.8, -0.8),
  "neg_pos_neg_corr" =
    c(-0.7, 0.7, -0.7),
  "neg_neg_pos_corr" =
    c(-0.7, -0.7, 0.7),
  "pos_neg_neg_corr" =
    c(0.7, -0.7, -0.7),
  "no_corr" =
    c(0, 0, 0),
  "pos_pos_neg_corr" = # 0.7 results in non positive definite Sigma
    c(0.5, 0.5, -0.5),
  "neg_pos_pos_corr" =  # 0.7 results in non positive definite Sigma
    c(-0.5, 0.5, 0.5),
  "pos_neg_pos_corr" =
    c(0.5, -0.5, 0.5),
  "low_pos_corr" =
    c(0.1, 0.1, 0.1),
  "med_pos_corr" =
    c(0.5, 0.5, 0.5),
  "high_pos_corr" =
    c(0.9, 0.9, 0.9)
)

# soil fluxes -------------------------------------------------------------

calculate_average_fluxes <- function(corr_vec, road_area = 0) {
  source("assign_flux_rate_scripts/final_product/merge_spatial_dts_function.R")
  source("assign_flux_rate_scripts/simulate_flux_rates/flux_simulation.R")
  
  # load wetland_dominated_cells
  load("datasets/land_cover/cover/corine_dataframes/wetland_dominated_rowids.Rdata")
  
  results_list <- list()
  
  # next load classes
  class_conversion <- fread("datasets/land_class_conversion/wide_lc_classes.csv")
  class_conversion <- map(as.list(class_conversion), na.omit) %>%
    map(as.vector)
  
  # read dataframe of corine classes (and corresponding symbology) then clean dataset
  corine_classes_small_dt <- fread("datasets/land_class_conversion/uk_2018_corine_lc.csv")[, .(layer = GRID_CODE, corine_class = LABEL3)]
  
  # rast("datasets/land_cover/cover/corine_lc_raster/1km_prop_clc_2018/1km_prop_clc_2018_below_sealed2.tif")
  
  # Read spatial data -------------------------------------------------------
  
  # read inverse imperviousness data
  # generated in prop_impervious_1km.R
  # uk_inv_imperv_dt_aggr <- fread("datasets/imperviousness/imperviousness_1km_perc/uk_imd_1km.csv")
  # generated in load_imperviousness_1km.R (bilinear interpolation)
  uk_inv_imperv_dt <- fread("datasets/imperviousness/imperviousness_1km_perc/uk_imd_bilinear_1km.csv") %>%
    rowid_to_column()
  
  road_str <- "no_roads"
  
  if (road_area > 0) {
    # uk_inv_imperv_dt <- uk_inv_imperv_dt %>%
    #   rowid_to_column()
    
    set.seed(12)
    
    road_str <- ""
    
    road_sealed <- uk_inv_imperv_dt %>%
      filter(inv_imperv > 0.95 &
               !(rowid %in% wetland_dominated_cells)) %>%
      slice_sample(n = road_area) %>%
      mutate(inv_imperv = 0)
    
    road_sealed_id <- road_sealed %>% pull(rowid)
    
    uk_inv_imperv_dt <- uk_inv_imperv_dt %>% filter(!(rowid %in% road_sealed_id)) %>%
      bind_rows(road_sealed)
  }
  
  
  
  # read unsealed data
  prop_corine_1km_lc_uk_beneath <-
    fread(
      "datasets/land_cover/cover/corine_dataframes/prop_corine_1km_lc_uk_beneath2.csv"
    )[, ("prop") := 0.01 * get("prop")]
  
  ranges <- read_csv("datasets/ghg_flux_rates/ranges.csv", show_col_types = FALSE)
  # add sd to data
  ranges_sim <- ranges %>% dplyr::filter(CORINE2018class != "Sealed_CORINE") %>%
    dplyr::select(Species, Rate_range_min, Rate_range_max, CORINE2018class) %>%
    mutate(
      midpoint = (Rate_range_min + Rate_range_max) / 2,
      sd_1 = (Rate_range_max - midpoint) / 3
    )
  
  # write_csv(ranges_sim, file = "shinyr_soilg_ghgs/calculate_fluxes.csv")
  
  classes <- ranges_sim %>% distinct(CORINE2018class) %>% pull(CORINE2018class)
  sigma <- ranges_sim %>% pull(sd_1)
  
  # Simulate fluxes ---------------------------------------------------------
  
  mu_list <- list()
  for (i in 1:length(classes)) {
    mu_list[[i]] <- ranges_sim %>% filter(CORINE2018class == classes[i]) %>%
      pull(midpoint)
  }
  
  
  # load fluxes
  fluxes_dt <- full_simulation(
    mu_list = mu_list,
    classes = classes,
    corr_vec = corr_vec,
    sigma = sigma,
    n_sims = 1000
  )
  
  fluxes_dt[, ("class_code") :=
              fcase(
                broad_class == "Cropland_CORINE" ,
                1,
                broad_class == "Barren_CORINE" ,
                2,
                broad_class == "Grassland_CORINE" ,
                3,
                broad_class == "Wetland_CORINE" ,
                4,
                broad_class == "Forestland_CORINE" ,
                5,
                broad_class == "Sealed_CORINE" ,
                6,
                broad_class == "NO_SOIL_CORINE" ,
                7
              )][, ("species_code") :=
                   fcase(Species == "CO2", 1, Species == "CH4", 2, Species == "N2O", 3)][, c("Species", "broad_class") := NULL]
  
  # recode corine_classes (using integers for classes) ----------------------
  
  corine_classes_coded <- corine_classes_small_dt[, ("broad_class") :=
                                                    fcase(
                                                      corine_class %chin% class_conversion$Cropland_CORINE ,
                                                      "Cropland_CORINE",
                                                      corine_class %chin% class_conversion$Barren_CORINE ,
                                                      "Barren_CORINE",
                                                      corine_class %chin% class_conversion$Grassland_CORINE ,
                                                      "Grassland_CORINE",
                                                      corine_class %chin% class_conversion$Wetland_CORINE ,
                                                      "Wetland_CORINE",
                                                      corine_class %chin% class_conversion$Forestland_CORINE ,
                                                      "Forestland_CORINE",
                                                      corine_class %chin% class_conversion$Sealed_CORINE ,
                                                      "Sealed_CORINE",
                                                      corine_class %chin% class_conversion$NO_SOIL_CORINE ,
                                                      "NO_SOIL_CORINE"
                                                    )][, ("class_code") :=
                                                         fcase(
                                                           broad_class == "Cropland_CORINE" ,
                                                           1,
                                                           broad_class == "Barren_CORINE" ,
                                                           2,
                                                           broad_class == "Grassland_CORINE" ,
                                                           3,
                                                           broad_class == "Wetland_CORINE" ,
                                                           4,
                                                           broad_class == "Forestland_CORINE" ,
                                                           5,
                                                           broad_class == "Sealed_CORINE" ,
                                                           6,
                                                           broad_class == "NO_SOIL_CORINE" ,
                                                           7
                                                         )][, c("layer", "class_code")][!is.na(class_code)]
  
  # val_cols <- fluxes_dt %>% names() %>%
  #   str_subset("V\\d*")
  
  ####
  merged <- prop_corine_1km_lc_uk_beneath[corine_classes_coded, on  = "layer", allow.cartesian = TRUE] %>%
    merge(uk_inv_imperv_dt, by = c("x", "y")) %>%
    filter(!(class_code %in% c(6, 7)))
  
  results_list$class_areas <- merged %>%
    group_by(class_code) %>%
    summarise(class_area = sum(prop),
              mean_inv_imp = mean(inv_imperv),
              effective_area = sum(prop * inv_imperv)) %>%
    mutate(
      sum = sum(effective_area),
      prop = effective_area / sum,
      class = fcase(
        class_code == 1,
        "Cropland" ,
        class_code == 2,
        "Barren" ,
        class_code == 3,
        "Grassland" ,
        class_code == 4,
        "Wetland",
        class_code == 5,
        "Forestland"
      )
    ) %>% relocate(class) %>%
    dplyr::select(-class_code)
  
  
  rm(prop_corine_1km_lc_uk_beneath)
  avg_prop_and_imp <- merged %>%
    group_by(class_code) %>%
    summarise(
      unsealed_area = sum(prop),
      sealed_area = sum(prop * inv_imperv))
  
  rm(merged)
  final_sums_sealed <- avg_prop_and_imp %>%
    merge(fluxes_dt, by = "class_code", allow.cartesian = TRUE) %>%
    mutate(across(V1:V1000, function(X)
      X * sealed_area)) %>%
    mutate(
      GHG = fcase(
        species_code == 1,
        "CO2",
        species_code == 2,
        "CH4",
        species_code == 3,
        "N2O"
      ),
      class = fcase(
        class_code == 1,
        "Cropland" ,
        class_code == 2,
        "Barren" ,
        class_code == 3,
        "Grassland" ,
        class_code == 4,
        "Wetland",
        class_code == 5,
        "Forestland"
      )
    ) %>%
    dplyr::select(-c("class_code", "species_code")) %>%
    relocate(GHG, class) %>%
    mutate(sealed_status = "sealed")
  final_sums_unsealed <- avg_prop_and_imp %>%
    merge(fluxes_dt, by = "class_code", allow.cartesian = TRUE) %>%
    mutate(across(V1:V1000, function(X)
      X * unsealed_area)) %>%
    mutate(
      GHG = fcase(
        species_code == 1,
        "CO2",
        species_code == 2,
        "CH4",
        species_code == 3,
        "N2O"
      ),
      class = fcase(
        class_code == 1,
        "Cropland" ,
        class_code == 2,
        "Barren" ,
        class_code == 3,
        "Grassland" ,
        class_code == 4,
        "Wetland",
        class_code == 5,
        "Forestland"
      )
    ) %>%
    dplyr::select(-c("class_code", "species_code")) %>%
    relocate(GHG, class) %>%
    mutate(sealed_status = "unsealed")
  
  
  
  sums <- final_sums_sealed %>%
    bind_rows(final_sums_unsealed) %>%
    mutate(GHG_class = paste0(GHG, "_", class, "_", sealed_status)) %>%
    relocate(GHG_class) %>%
    dplyr::select(-c(GHG, class, unsealed_area, sealed_area, sealed_status)) %>% 
    rotate_df() %>%
    rownames_to_column(var = "Sim_Number") %>%
    mutate(Sim_Number = str_remove_all(Sim_Number, pattern = "V"))
  colnames(sums) <- sums[1, ]
  # sums
  results_list$long_results <- sums %>% filter(GHG_class != "GHG_class") %>%
    mutate(
      across(CH4_Cropland_sealed:CO2_Forestland_unsealed, as.double),
      CH4_Cropland_fromsealing = CH4_Cropland_unsealed - CH4_Cropland_sealed ,
      N2O_Cropland_fromsealing = N2O_Cropland_unsealed -   N2O_Cropland_sealed,
      CO2_Cropland_fromsealing = CO2_Cropland_unsealed - CO2_Cropland_sealed,
      N2O_Barren_fromsealing = N2O_Barren_unsealed -    N2O_Barren_sealed,
      CO2_Barren_fromsealing = CO2_Barren_unsealed -     CO2_Barren_sealed,
      CH4_Barren_fromsealing = CH4_Barren_unsealed -   CH4_Barren_sealed,
      CH4_Grassland_fromsealing = CH4_Grassland_unsealed - CH4_Grassland_sealed,
      N2O_Grassland_fromsealing = N2O_Grassland_unsealed - N2O_Grassland_sealed,
      CO2_Grassland_fromsealing = CO2_Grassland_unsealed - CO2_Grassland_sealed,
      CO2_Wetland_fromsealing = CO2_Wetland_unsealed -   CO2_Wetland_sealed,
      N2O_Wetland_fromsealing = N2O_Wetland_unsealed -   N2O_Wetland_sealed,
      CH4_Wetland_fromsealing = CH4_Wetland_unsealed -   CH4_Wetland_sealed,
      CH4_Forestland_fromsealing = CH4_Forestland_unsealed -   CH4_Forestland_sealed,
      N2O_Forestland_fromsealing = N2O_Forestland_unsealed -  N2O_Forestland_sealed,
      CO2_Forestland_fromsealing = CO2_Forestland_unsealed - CO2_Forestland_sealed
    ) %>%
    rename(Sim_number = GHG_class) %>%
    pivot_longer(
      cols = CH4_Cropland_sealed:last_col(),
      values_to = "value",
      names_to = c("GHG", "Landcover", "Sealed_status"),
      names_pattern = "(.*)_(.*)_(.*)"
    ) %>% mutate(source = if_else(
      condition = road_area > 0,
      true = "roads",
      false = "other"
    ))
  
  results_list$summary <- results_list$long_results %>%
    group_by(GHG, Landcover, Sealed_status) %>%
    summarise(
      sd = sd(value),
      min = min(value),
      max = max(value),
      mean = mean(value),
      lower = mean - sd * qnorm(0.975),
      upper = mean + sd * qnorm(0.975)
    ) %>%
    mutate(source = if_else(
      condition = road_area > 0,
      true = "roads",
      false = "other"
    ))
  # corr_vec <- c(0,0,0)
  if (road_area > 0) {
    corr_str <- corr_vec %>% paste(collapse = "_")
    results_list$path <- paste0("datasets/FINAL_results/summed_results/",
                                road_str,
                                "corr_",
                                corr_str,
                                ".csv")
    fwrite(results_list$long_results, results_list$path)
  }
  results_list
}