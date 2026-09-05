# carga de los datos
library(AmesHousing)
library(xgboost)

# librerías para la limpieza  y preparación de los datos
library(janitor)
library(dplyr)

# carga de los paquetes necesarios
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)

# aceleración de los cálculos con procesamiento paralelo (opcional pero útil)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# fijamos la semilla aleatoria set.seed() para que los resultados 
# sean replicables y así podemos reproducir cualquier simulación.
set.seed(1234)

# carga de los datos y limpieza de los nombres
ames_data <- make_ames() %>%
  janitor::clean_names()

# División de los datos para entrenamiento y prueba. Estratificación por el precio de venta.
ames_split <- rsample::initial_split(
  ames_data, 
  prop = 0.8, 
  strata = sale_price
)

# preprocessing "recipe"
preprocessing_recipe <- 
  recipes::recipe(sale_price ~ ., data = training(ames_split)) %>%
  # convert categorical variables to factors
  recipes::step_string2factor(all_nominal()) %>%
  # combine low frequency factor levels
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  recipes::step_nzv(all_nominal()) %>%
  prep()

ames_cv_folds <- 
  recipes::bake(
    preprocessing_recipe, 
    new_data = training(ames_split)
  ) %>%  
  rsample::vfold_cv(v = 5)

# XGBoost model specification
xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 60
  )

knitr::kable(head(xgboost_grid))

xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(sale_price ~ .)

############################################
# hyperparameter tuning
xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = ames_cv_folds,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)
########################################

xgboost_tuned %>%
  tune::show_best(metric = "rmse") %>%
  knitr::kable()
