# 0. 패키지 및 환경설정

##필수 패키지 설치
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("tictoc")
install.packages("doParallel")
install.packages("furrr")
install.packages("xgboost")
#대응 알고리즘
install.packages("ranger")
install.packages("glmnet")
#데이터셋
install.packages("palmerpenguins")
#테마-> 흰배경
install.packages("hrbrthemes")
#결과확인용
install.packages("finetune")

##패키지 불러오기
library(tidyverse)
library(tidymodels)
library(tictoc)
library(doParallel)
library(furrr)
library(ranger)
library(glmnet)
library(palmerpenguins)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()

##환경설정
theme_set(hrbrthemes::theme_ipsum_rc()) # --> 배경 흰색으로 설정 
plan(multicore, workers = availableCores())
cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(cores)
registerDoParallel(cores = cl)
set.seed(42)  # random결과를 seed값 42 기준으로 가져옴.시드값 가져오면 아무리 데이터 셔플링해도 동일한 셔플링 진행

# 1. 데이터 불러오기 및 전처리
penguins_data <- palmerpenguins::penguins #(패키지명 :: 안에 있는 내용) 가져와져
#penguins_data <- penguins #패키지 위에서 불러온 경우, penguins만 불러도 가져와짐
penguins_data

#NA값 부터 날리기
glimpse(penguins_data)
t(map_df(penguins_data, ~sum(is.na(.))))

penguins_df <-
  penguins_data %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)
head(penguins_df)

penguins_split <-
  rsample::initial_split(
    penguins_df,
    prop = 0.7,
    strata = species #종 기준으로 적절하게 나눠줘
  )

## 2. 기준(Baseline Experiment) 설정/ 베이스라인 구축
tic(" Baseline XGBoost training duration ")
xgboost_fit <-
  boost_tree() %>% #boost 알고리즘
  set_engine("xgboost") %>%
  set_mode("classification") %>% #분류모델을 써라
  fit(species ~ ., data = training(penguins_split)) #'species ~ .' 나머지 다 써라
toc(log = TRUE)

preds <- predict(xgboost_fit, new_data = testing(penguins_split)) 
actual <- testing(penguins_split) %>% select(species)
yardstick::f_meas_vec(truth = actual$species, estimate = preds$.pred_class)

## 3. 모델 설정
#-- model 1
ranger_model <-
  parsnip::rand_forest(mtry = tune(), min_n = tune()) %>% #'rand_forest' 학자들이 만든 것. 'min_n': 최소 몇 가지 내려갈건데?
  set_engine("ranger") %>%
  set_mode("classification")

#-- model 2
glm_model <- #제약조건을 기반으로 penalty 값을 준다. tune은 내가 할 생각이 없는 것
  parsnip::multinom_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

#-- model 3
xgboost_model <-
  parsnip::boost_tree(mtry = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")
hardhat::extract_parameter_dials(glm_model, "mixture")

## 4. Grid 검색(전역 검색)
ranger_grid <-
  hardhat::extract_parameter_set_dials(ranger_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_regular(levels = 4)
ranger_grid

ranger_grid %>% ggplot(aes(mtry, min_n)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "Ranger: Regular grid for min_n & mtry combinations")

glm_grid <-
  parameters(glm_model) %>%
  grid_random(size = 20)
glm_grid

glm_grid %>% ggplot(aes(penalty, mixture)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "GLM: Random grid for penalty & mixture combinations")

xgboost_grid <-
  parameters(xgboost_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_max_entropy(size = 20)
xgboost_grid

xgboost_grid %>% ggplot(aes(mtry, learn_rate)) +
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "XGBoost: Max Entropy grid for LR & mtry combinations")

## 5. 데이터 전처리
recipe_base <-
  recipe(species ~ ., data = training(penguins_split)) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) # Create dummy variables (which glmnet needs)/ 'step_dummy' 자기가 다 알아서 함
recipe_1 <-
  recipe_base %>%
  step_YeoJohnson(all_numeric())
recipe_1 %>%
  prep() %>%
  juice() %>%
  summary()

recipe_2 <-
  recipe_base %>%
  step_normalize(all_numeric()) #'step_normalize' 정규화 해서 실제 쓸수 있는 숫자로 변경함
recipe_2 %>%
  prep() %>%
  juice() %>%
  summary()

## 6. Metrics, 일괄처리 정의
model_metrics <- yardstick::metric_set(f_meas, pr_auc)

## 7. K-Fold CV
data_penguins_3_cv_folds <-
  rsample::vfold_cv(
    v = 5,
    data = training(penguins_split),
    strata = species
  )

## 8. 모델 학습(Model Training)
## 8 - 1. 일괄 작업 작성
ranger_r1_workflow <-
  workflows::workflow() %>%
  add_model(ranger_model) %>%
  add_recipe(recipe_1)
glm_r2_workflow <-
  workflows::workflow() %>%
  add_model(glm_model) %>%
  add_recipe(recipe_2)
xgboost_r2_workflow <-
  workflows::workflow() %>%
add_model(xgboost_model) %>%
  add_recipe(recipe_2)

## 8 - 2.  Gridsearch를 활용한 학습
tic("Ranger tune grid training duration ")
ranger_tuned <-
  tune::tune_grid(
    object = ranger_r1_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = ranger_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)

tic("GLM tune grid training duration ")
glm_tuned <-
  tune::tune_grid(
    object = glm_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = glm_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)

tic("XGBoost tune grid training duration ")
xgboost_tuned <-
  tune::tune_grid(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)

## 8 - 3. 학습 결과 확인
library(finetune)
tic("Tune race training duration ")
ft_xgboost_tuned <-
  finetune::tune_race_anova(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = control_race(verbose_elim = TRUE) # 66
  )
toc(log = TRUE)
## 8 - 4.  시각화를 통한 확인
plot_race(ft_xgboost_tuned) + labs(title = "Parameters Race by Fold")

# 결과
bind_cols(
  tibble(model = c("Ranger", "GLM", "XGBoost")),
  bind_rows(
    ranger_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arranglm_tuned %>%
      glm_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arranglm_tuned %>% 
      xgboost_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arran)
)

# 전체 모델 확인
glm_tuned %>% collect_metrics() # 20 models and 2 metrics

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>% select_best(metric = "f_meas")

# F1값 확인
glm_tuned %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  select(mean, penalty, mixture) %>%
  pivot_longer(penalty:mixture,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "F1", title = "F1 MetricEvolution")

# 모델 개선
best_f1 <-
  select_best(xgboost_tuned, metric = "f_meas")
final_model_op1 <-
  finalize_workflow(
    x = xgboost_r2_workflow,
    parameters = best_f1
  )
final_model_op1

#  Last Fit Tune Model
tic("Train final model Tune")
penguins_last_fit <-
  last_fit(final_model_op1,
           penguins_split,
           metrics = model_metrics
  )
toc(log = TRUE)

collect_metrics(penguins_last_fit) %>%
  arrange(.metric)

penguins_last_fit %>%
  collect_predictions() %>%
  conf_mat(truth = species, estimate = .pred_class)

penguins_last_fit %>%
  pull(.predictions) %>%
  as.data.frame() %>%
  filter(.pred_class != species)

# 주요 특징 분석
#install.packages("vip")
library(vip)
final_model_op1 %>%
  fit(data = penguins_df) %>%
  pull_workflow_fit() %>%
  vip(
    geom = "col",
    aesthetics = list(fill = "steelblue")
  ) +
  labs(title = "Feature Importance")

# 모델별 지표 확인
tic.log() %>%
  unlist() %>%
  tibble()

























































































































