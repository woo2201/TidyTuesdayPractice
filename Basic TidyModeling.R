urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


lr_mod <- 
  linear_reg() %>%  # 모델 선택 : 먼저 모델을 선언해준다. 여기서는 linear_reg()이라고 선형귀모델을 선언했습니다.
  set_engine('lm') # set engine은 어떤 패키지나 컴퓨팅시스템을 통해서 모델을 훈련시킬 것인지 지정하는 것입니다.

lm_fit <- 
  lr_mod %>% 
  fit(width ~ initial_volume*food_regime, data = urchins) # fit()을 통해서 모델을 쉽게 fitting 가능합니다.

tidy(lm_fit)

# expand.grid함수는 data.frame을 만들어주는 함수입니다.
# 그냥 data.frame과 다른점은 factor를 기준으로 데이터를 여러 번 반복해서
# 출력해준다는 점입니다. 아래 예시를 통해서 보면 차이점이 확연해집니다.

new_points <- expand.grid(initial_volume =20,
                          food_regime = c('Initial','Low','High'))
new_points


mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred


conf_int_pred <- predict(lm_fit,
                         new_data = new_points,
                         type = 'conf_int')
conf_int_pred

plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

    
