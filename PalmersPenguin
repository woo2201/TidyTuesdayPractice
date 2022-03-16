library(tidyverse)
library(tidymodels)
library(data.table)
library(skimr)

# import datasets. 

path = "/Users/jungwonwoo/Desktop/program_file/6.Dacon/Heart_failure/dataset"
filelist = list.files(path)
for (i in filelist){
  name <- paste0("data_", str_replace(i, '.csv', ''))
  assign(name, fread(paste0(path, '/', i)) %>% as.data.frame())
}

data_train = data_train %>%
  mutate(ca = ifelse(ca == 4, NA, ca),
         thal = ifelse(thal == 0, NA, thal)) %>%
  mutate_at(c('cp','fbs','restecg','exang','slope','thal'), as.factor)

data_test = data_test %>%
  mutate(ca = ifelse(ca == 4, NA, ca),
         thal = ifelse(thal == 0, NA, thal)) %>%
  mutate_at(c('cp','fbs','restecg','exang','slope','thal','target'), as.factor)


data_train %>% glimpse()

data_recipe = 
  recipe(as.factor(target) ~ ., data = data_train) %>% 
  update_role(id, new_role = "ID") %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_knn(all_predictors()) %>%
  step_zv(all_predictors()) # 하나의 값만 존재하는 컬럼 날려버림

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

data_workflow = 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(data_recipe)

fitted_data =
  data_workflow %>% 
  fit(data = data_train)

{
# check the train dataset.
# it doesnt seem particularly different from one another.

data_train %>%
  skim()

data_test %>%
  skim()


# split into two groups and see (by target)

data_train %>%
  group_split(target) %>%
  .[[1]]

data_train %>%
  group_split(target) %>%
  .[[2]]

# do a t-test on whether 

data_train %>%
  summarise_each(funs(t.test(.[target == 0],
                             .[target == 1])$p.value), vars = age:thal) %>%
  t() %>%
  as.data.frame() %>%
  set_rownames(colnames(data_train)[2:14]) %>%
  rename(p_value = V1) %>%
  mutate(p_value = round(p_value, 5))

# since doing multiple t tests can cause the accumulation of type i errors... 
# we do anova instead.

library(rstatix)
library(lawstat)

data_train %>%
  shapiro_test(age, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)
anova_summary = summary(aov(target ~., data = data_train %>% select(-id)))
plot(aov(target ~., data = data_train %>% select(-id)), 5) # 112 needs to go.
bartlett.test(chol ~ target, data_train) 

# age, sex, cp, thalach, exang, oldpeak, ca, thal seems to have statistically significant dmg.
# the test above shows the similar result. right?
# However, it is not assumed that the dataset actually followed the basic assumptions.
# normality assumptions are met, 
# homoskedacity are violated in some cases, telling us anova result might've been disrupted.

# so let me just push in every variables in.
}
{
skim(dest, data_train)

factors = c('sex','cp','fbs','restecg','exang','slope','ca','thal')

data_train %>%
  mutate_at(factors, as.factor) %>%
  select_if(~ is.factor(.) && nlevels(.) < 5) # 대부분 5개 이상의 레벨.

data_recipe =  data_train %>%
  mutate(restecg = ifelse(restecg == 2, NA, restecg),
         ca = ifelse(ca == 4, NA, ca),
         thal = ifelse(thal == 0, NA, thal)) %>%
  filter(id != 112) %>%
  mutate_at(factors, as.factor) %>%
  recipe(target ~ .) %>%
  step_corr(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = ) %>%
  step_center(all_numeric_predictors()) %>%
  prep()

data_testing = data_recipe %>%
  bake(data_test)

data_training = data_recipe %>%
  juice()

library(randomForest)
rfmodel = rand_forest(trees=100, mode='classification') %>%
  set_engine('randomForest') %>%
  fit(as.factor(target) ~., data = data_training)

rfmodel

rfmodel %>%
  predict(data_testing) %>%
  bind_cols(data_testing)

data_sample_submission %>%
  mutate(target = rfmodel %>%
           predict(data_testing) %>% pull()) %>%
  write.csv("/Users/jungwonwoo/Desktop/program_file/6.Dacon/Heart_failure/submission.csv", row.names = FALSE)
}
