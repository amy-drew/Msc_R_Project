library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(GGally)
library(modelr)
library(MASS)
library(tidyverse)
library(tidyr)
library(randomForest)
library(xgboost)
library(e1071)
library(glmnet)
library(neuralnet)
library(ranger)
library(catboost)


#Cleaning Data
Cropdata <- read.csv("EcoCrop_DB.csv")
Cropdata <- subset(Cropdata, select = -c(AUTH, EcoPortCode, FAMNAME, SYNO))

factor_cols <- c("LIFO","HABI","LISPA","PHYS","PLAT","LIOPMN","LIOPMX",
                 "LIMN","LIMX","DEP","DEPR","TEXT","TEXTR","FER","FERR",
                 "TOX","TOXR","SAL","SALR","DRA","DRAR","PHOTO","CLIZ",
                 "ABISUS","ABITOL","INTRI","PROSY")

Cropdata[factor_cols] <- lapply(Cropdata[factor_cols], factor)

# ---- NEW PART: keep only first CAT entry and convert to factor ----
Cropdata <- Cropdata %>%
  mutate(
    CAT = ifelse(is.na(CAT) | CAT == "", NA,
                 trimws(strsplit(CAT, ",") |> sapply(`[`, 1)))
  ) %>%
  mutate(CAT = factor(CAT))
# -------------------------------------------------------------------

Crop_noCAT <- Cropdata %>% 
  filter(is.na(CAT) | CAT == "")

Crop_withCAT <- Cropdata %>% 
  filter(!(is.na(CAT) | CAT == ""))

# Since CAT now contains only one category, no need to split/unnest
Crop_long <- Crop_withCAT %>%
  mutate(Category = CAT)

Cropsfinal <- Crop_long %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Category, values_from = value, values_fill = 0)

Cropsfinal <- subset(Cropsfinal, select = -c(Column1))


#cor(Cropdata[,-1])
#signif(Cropdata[,-1])
#ggpairs(Cropdata[,-1], cardinality_threshold = 400)

#----------------------------------------------------------------
#Assigning Yields

faostat <- read.csv("FAOSTAT_data_en_8-18-2025.csv")
category_map <- tribble(
  ~Item, ~Category,
  # Cereals & pseudocereals
  "Wheat", "cereals",
  "Rice", "cereals",
  "Maize (corn)", "cereals",
  "Barley", "cereals",
  "Oats", "cereals",
  "Rye", "cereals",
  "Millet", "cereals",
  "Sorghum", "cereals",
  "Buckwheat", "cereals",
  "Quinoa", "cereals",
  "Mixed grain", "cereals",
  
  # Pulses
  "Beans, dry", "pulses",
  "Chick peas, dry", "pulses",
  "Lentils, dry", "pulses",
  "Peas, dry", "pulses",
  "Cow peas, dry", "pulses",
  "Pigeon peas, dry", "pulses",
  "Bambara beans, dry", "pulses",
  
  # Roots & tubers
  "Potatoes", "roots_tubers",
  "Cassava, fresh", "roots_tubers",
  "Sweet potatoes", "roots_tubers",
  "Yams", "roots_tubers",
  "Taro", "roots_tubers",
  "Yautia", "roots_tubers",
  
  # Vegetables
  "Tomatoes", "vegetables",
  "Cabbages", "vegetables",
  "Carrots and turnips", "vegetables",
  "Cucumbers and gherkins", "vegetables",
  "Eggplants (aubergines)", "vegetables",
  "Lettuce and chicory", "vegetables",
  "Onions and shallots, dry (excluding dehydrated)", "vegetables",
  "Onions and shallots, green", "vegetables",
  "Peas, green", "vegetables",
  "Spinach", "vegetables",
  "Pumpkins, squash and gourds", "vegetables",
  "Other vegetables, fresh n.e.c.", "vegetables",
  
  # Fruits & nuts
  "Apples", "fruits_nuts",
  "Bananas", "fruits_nuts",
  "Grapes", "fruits_nuts",
  "Oranges", "fruits_nuts",
  "Mangoes, guavas and mangosteens", "fruits_nuts",
  "Pears", "fruits_nuts",
  "Peaches and nectarines", "fruits_nuts",
  "Strawberries", "fruits_nuts",
  "Blueberries", "fruits_nuts",
  "Watermelons", "fruits_nuts",
  "Papayas", "fruits_nuts",
  "Pineapples", "fruits_nuts",
  "Avocados", "fruits_nuts",
  
  # Materials (fibres)
  "Jute, raw or retted", "materials",
  "Sisal, raw", "materials",
  "True hemp, raw or retted", "materials",
  "Ramie, raw or retted", "materials",
  "Abaca, manila hemp, raw", "materials",
  "Agave fibres, raw, n.e.c.", "materials",
  
  # Medicinals & aromatic
  "Ginger, raw", "medicinal_aromatic",
  "Cinnamon and cinnamon-tree flowers, raw", "medicinal_aromatic",
  "Cloves (whole stems), raw", "medicinal_aromatic",
  "Peppermint, spearmint", "medicinal_aromatic",
  "Vanilla, raw", "medicinal_aromatic"
)

faostat_cat <- faostat %>%
  left_join(category_map, by = "Item")

category_yields <- faostat_cat %>%
  filter(!is.na(Category)) %>%
  group_by(Category) %>%
  summarise(
    mean_yield_kg_ha = mean(Value, na.rm = TRUE),
    n_crops = n()
  ) %>%
  mutate(mean_yield_t_ha = mean_yield_kg_ha / 1000)

forage_row <- tibble(
  Category = "forage_pasture",
  mean_yield_kg_ha = 17170,   # 17.17 t DM/ha/year
  n_crops = 1,
  mean_yield_t_ha = 17.17
)

category_yields_final <- bind_rows(category_yields, forage_row)

category_cols <- names(Cropsfinal)[51:64]

category_map <- c(
  "cereals & pseudocereals" = "cereals",
  "pulses (grain legumes)"  = "pulses",
  "roots/tubers"            = "roots_tubers",
  "vegetables"              = "vegetables",
  "fruits & nuts"           = "fruits_nuts",
  "materials"               = "materials",
  "medicinals & aromatic"   = "medicinal_aromatic",
  "forage/pasture"          = "forage_pasture"
)

yield_lookup <- setNames(category_yields_final$mean_yield_t_ha,
                         category_yields_final$Category)

Cropsfinal$assigned_yield_t_ha <- NA_real_

for (i in seq_len(nrow(Cropsfinal))) {
  
  row_vals <- Cropsfinal[i, category_cols]
  active_categories <- names(row_vals)[row_vals == 1]
  faostat_cats <- category_map[active_categories]
  faostat_cats <- faostat_cats[!is.na(faostat_cats)]

  if (length(faostat_cats) == 0) {
    Cropsfinal$assigned_yield_t_ha[i] <- NA
    next
  }
  yields <- yield_lookup[faostat_cats]
  Cropsfinal$assigned_yield_t_ha[i] <- mean(yields, na.rm = TRUE)
}

fallback_yield <- mean(category_yields_final$mean_yield_t_ha, na.rm = TRUE)
#Used fallback yields
cropsfull <- Cropsfinal %>%
  mutate(
    assigned_yield_t_ha = ifelse(
      is.na(assigned_yield_t_ha),
      fallback_yield,
      assigned_yield_t_ha
    )
  )



#--------------------------------------------------------------------------
#Multilinear Regression and Reverse
Cropsfinalnumeric <- Cropsfinal %>%
  mutate(across(all_of(factor_cols), ~ as.numeric(.)))

category_cols <- names(Cropsfinal)[51:64]
colnames(Cropsfinalnumeric)

model_data <- Cropsfinalnumeric %>%
  dplyr::select(
    -ScientificName,
    -COMNAME,
    -all_of(category_cols),
    -CAT
  )


model_data <- model_data %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

full_model <- lm(
  assigned_yield_t_ha ~ .,
  data = model_data
)

summary(full_model)

auto_backward <- step(full_model, direction = "backward")
summary(auto_backward)

auto_results <- data.frame(
  Predictor = rownames(summary(auto_backward)$coefficients),
  P_value   = summary(auto_backward)$coefficients[, "Pr(>|t|)"]
)

manual_backward_full <- function(data, response = "assigned_yield_t_ha") {
  predictors <- setdiff(names(data), response)
  kept_under_05 <- c()
  repeat {
    f <- as.formula(
      paste(response, "~", paste(predictors, collapse = " + "))
    )
    model <- lm(f, data = data)
    sm <- summary(model)
    pvals <- sm$coefficients[-1, "Pr(>|t|)"]
    sig <- names(pvals[pvals < 0.05])
    kept_under_05 <- unique(c(kept_under_05, sig))
    if (length(predictors) == 1) {
      message("Only one predictor left. Stopping.")
      break
    }
    worst <- names(which.max(pvals))
    message("Removing predictor: ", worst, " (p = ", round(max(pvals), 4), ")")
    predictors <- predictors[predictors != worst]
  }
  list(
    final_model = model,
    significant_predictors = kept_under_05
  )
}

manual_output <- manual_backward_full(model_data)
manual_model <- manual_output$final_model
summary(manual_model)

manual_results <- data.frame(
  Predictor = rownames(summary(manual_model)$coefficients),
  P_value   = summary(manual_model)$coefficients[, "Pr(>|t|)"]
)

auto_results
manual_results
manual_output$significant_predictors

#--------------------------------------------------------------------
# Machine Learning

significant_predictors <- manual_output$significant_predictors

modeldata <- Cropsfinal[, c(
  "assigned_yield_t_ha",
  "CAT",
  significant_predictors[significant_predictors %in% names(Cropsfinal)]
)]

modeldata <- na.omit(modeldata)


X <- model.matrix(~ . - 1, data = modeldata[, significant_predictors])
colnames(X) <- make.names(colnames(X), unique = TRUE)
y <- modeldata$assigned_yield_t_ha


# Random Forest
rf_model <- randomForest(
  assigned_yield_t_ha ~ .,
  data = modeldata,
  importance = TRUE
)


# SVR
svr_model <- svm(
  x = X,
  y = y,
  kernel = "radial"
)


# Lasso
lasso_model <- cv.glmnet(
  X,
  y,
  alpha = 1
)


# Elastic Net
enet_model <- cv.glmnet(
  X,
  y,
  alpha = 0.5
)

enet_coef <- coef(enet_model, s = "lambda.min")


# Neural Network
nn_data <- data.frame(assigned_yield_t_ha = y, X)

nn_formula <- as.formula(
  paste("assigned_yield_t_ha ~", paste(colnames(X), collapse = " + "))
)

nn_model <- neuralnet(
  formula = nn_formula,
  data = nn_data,
  hidden = 5,
  err.fct = "sse",
  linear.output = TRUE,
  lifesign = "full",
  rep = 2,
  algorithm = "rprop+",
  stepmax = 100000
)

output <- compute(nn_model, rep = 1, nn_data[, -1])
pred_nn <- nn_model$net.result[[1]]


# Ranger ExtraTrees


et_model <- ranger(
  dependent.variable.name = "assigned_yield_t_ha",
  data = data.frame(assigned_yield_t_ha = y, X),
  num.trees = 500,
  splitrule = "extratrees"
)

# CatBoost
train_pool <- catboost.load_pool(
  data = modeldata[, -1],                 
  label = modeldata$assigned_yield_t_ha   
)

cat_model <- catboost.train(
  train_pool,
  params = list(
    loss_function = "RMSE",
    iterations = 500,
    depth = 6,
    learning_rate = 0.05,
    logging_level = "Silent" 
  )
)


#---------------------------------------------------------------
# Predictions for all models

# Random Forest
pred_rf <- predict(rf_model, newdata = modeldata)

# SVR
pred_svr <- predict(svr_model, X)

# Lasso
pred_lasso <- predict(lasso_model, X, s = "lambda.min")

# Elastic Net
pred_enet <- predict(enet_model, X, s = "lambda.min")

# Neural Network
pred_nn <- nn_model$net.result[[1]]

# Ranger ExtraTrees
pred_et <- predict(et_model, data.frame(X))$predictions

# CatBoost
pred_cat <- catboost.predict(cat_model, train_pool)

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae  <- function(actual, predicted) mean(abs(actual - predicted))

y_true <- modeldata$assigned_yield_t_ha

results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees",
    "CatBoost"
  ),
  RMSE = c(
    rmse(y_true, pred_rf),
    rmse(y_true, pred_svr),
    rmse(y_true, pred_lasso),
    rmse(y_true, pred_enet),
    rmse(y_true, pred_nn),
    rmse(y_true, pred_et),     
    rmse(y_true, pred_cat)
  ),
  MAE = c(
    mae(y_true, pred_rf),
    mae(y_true, pred_svr),
    mae(y_true, pred_lasso),
    mae(y_true, pred_enet),
    mae(y_true, pred_nn),
    mae(y_true, pred_et),      
    mae(y_true, pred_cat)
  )
)

results

resultsdf <- data.frame(cbind(y_true, pred_rf, pred_svr, pred_lasso, pred_enet, pred_nn, pred_et,pred_cat))
# Define your colours
colors <- c(
  "SVR" = "pink",
  "Random Forest" = "red",
  "Lasso" = "orange",
  "Elastic Net" = "yellow",
  "Neural Network" = "green",
  "Ranger ExtraTrees" = "blue",
  "CatBoost" = "purple"
)

ggplot() +
  # Points
  geom_point(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            shape = 1, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  shape = 2, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          shape = 3, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    shape = 4, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), shape = 5, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), shape = 6, size = 2.5) +
  geom_point(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       shape = 7, size = 2.5) +
  
  # Trend lines
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_svr,  color = "SVR"),            method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_rf,   color = "Random Forest"),  method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_lasso,color = "Lasso"),          method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_enet, color = "Elastic Net"),    method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_nn,   color = "Neural Network"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_et,   color = "Ranger ExtraTrees"), method = lm, se = FALSE) +
  geom_smooth(data = resultsdf, aes(x = y_true, y = pred_cat,  color = "CatBoost"),       method = lm, se = FALSE) +
  
  # 1:1 reference line
  geom_smooth(data = resultsdf, aes(x = y_true, y = y_true), color = "black", method = lm, se = FALSE) +
  
  # Labels + legend
  labs(
    x = "True Value",
    y = "Predicted Value",
    color = "Model"
  ) +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 14)

#--------------------------------------------------------------------------------------
#cross validation
library(caret)

set.seed(123)

# ---------------------------------------------------------
# 1. Repeated k-fold CV setup
# ---------------------------------------------------------
cv_control <- trainControl(
  method = "repeatedcv",
  number = 10,       # 10 folds
  repeats = 5,       # repeated 5 times
  verboseIter = FALSE
)

# ---------------------------------------------------------
# 2. Prepare data for caret
# ---------------------------------------------------------
df <- data.frame(assigned_yield_t_ha = y, X)

# ---------------------------------------------------------
# 3. Train models using caret wrappers
# ---------------------------------------------------------

# Random Forest
cv_rf <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "rf",
  trControl = cv_control,
  metric = "RMSE"
)

# SVR (radial)
cv_svr <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "svmRadial",
  trControl = cv_control,
  metric = "RMSE"
)

# Lasso
cv_lasso <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 20)),
  trControl = cv_control,
  metric = "RMSE"
)

# Elastic Net
cv_enet <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0.5, lambda = seq(0.0001, 1, length = 20)),
  trControl = cv_control,
  metric = "RMSE"
)

# Neural Network (nnet)
cv_nn <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "nnet",
  trControl = cv_control,
  linout = TRUE,
  trace = FALSE,
  metric = "RMSE",
  tuneLength = 5
)

# Ranger ExtraTrees
cv_et <- train(
  assigned_yield_t_ha ~ .,
  data = df,
  method = "ranger",
  trControl = cv_control,
  metric = "RMSE",
  tuneGrid = expand.grid(
    mtry = floor(sqrt(ncol(X))),
    splitrule = "extratrees",
    min.node.size = 5
  )
)

# CatBoost (caret wrapper)
#cv_cat <- train(
 # assigned_yield_t_ha ~ .,
  #data = df,
  #method = "catboost.caret",
  #trControl = cv_control,
  #metric = "RMSE",
  #tuneLength = 3
#)

# ---------------------------------------------------------
# 4. Collect CV results
# ---------------------------------------------------------
cv_results <- data.frame(
  Model = c(
    "Random Forest",
    "SVR",
    "Lasso",
    "Elastic Net",
    "Neural Network",
    "Ranger ExtraTrees"
    #"CatBoost"
  ),
  RMSE = c(
    cv_rf$results$RMSE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$RMSE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$RMSE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$RMSE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$RMSE[which.min(cv_nn$results$RMSE)],
    cv_et$results$RMSE[which.min(cv_et$results$RMSE)]
    #cv_cat$results$RMSE[which.min(cv_cat$results$RMSE)]
  ),
  MAE = c(
    cv_rf$results$MAE[which.min(cv_rf$results$RMSE)],
    cv_svr$results$MAE[which.min(cv_svr$results$RMSE)],
    cv_lasso$results$MAE[which.min(cv_lasso$results$RMSE)],
    cv_enet$results$MAE[which.min(cv_enet$results$RMSE)],
    cv_nn$results$MAE[which.min(cv_nn$results$RMSE)],
    cv_et$results$MAE[which.min(cv_et$results$RMSE)]
    #cv_cat$results$MAE[which.min(cv_cat$results$RMSE)]
  )
)

cv_results

