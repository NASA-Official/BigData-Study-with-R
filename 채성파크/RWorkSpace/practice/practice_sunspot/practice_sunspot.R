getwd()
setwd("C:\\Users\\97psh\\OneDrive\\바탕 화면\\RWorkspace")
getwd()

#참고자료 : https://blog.naver.com/PostView.naver?blogId=flynt1&logNo=222461230043&parentCategoryNo=&categoryNo=16&viewDate=&isShowPopularPosts=true&from=search

install.packages("tidymodels")
install.packages("modeltime")
install.packages("timetk")
install.packages("cowplot")
install.packages("torch")
install.packages("luz")

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(cowplot)
library(lubridate)
library(torch)
library(luz)

df = sunspot.month %>% tk_tbl() %>% mutate(index= as_date(index))
df %>% tk_index() %>% tk_get_timeseries_summary() #데이터의 시계열 정보를 개괄적으로 보여준다.

# 그래프 뽑기
p1 = 
  df %>% ggplot(aes(index, value)) +
  geom_point(color="black", alpha=0.5, size=1) + 
  labs(title="1749~2013")

p2 = 
  df %>%
  filter_by_time(.date_var = index,
                 .start_date = "start",
                 .end_date = "1800") %>%
  ggplot(aes(index, value)) +
    geom_line(color="gray")+
    geom_point(color="black", size=1) +
    geom_smooth(method = "loess", color="blue", span=0.2, size=0.4, se=F) +
    labs(title="1479~1800")

plot_grid(p1, p2, ncol=1)


#ACF - 자기상관성 여부 확인하기
#lstm은 자기상관성을 이용한 예측 방식이다.
df %>%
  plot_acf_diagnostics(.date_var = index,
                       .value = value,
                       .show_white_noise_bars = T,
                       .interactive = F) + 
  geom_vline(xintercept = c(120, 240, 360, 480, 600, 720, 840, 960), color="red", size=0.3, linetype=2)


#10년치 학습 -> 10년치 예측

initial.size = 10*12 #10years
assess.size = 10*12 #10years

scale.factor = list(mean=mean(df$value), sd=sd(df$value))
scale.factor

df.cv = 
  df %>%
  mutate(value = scale(value)) %>%
  time_series_cv(date_var = index,
                 initial = initial.size,
                 assess = assess.size,
                 skip = 1,
                 cumulative = F)

df.cv %>%
  filter(id %in% c("Slice0001", "Slice0055", "Slice1003")) %>%
  tk_time_series_cv_plan() %>%
  mutate(value=value*scale.factor$sd + scale.factor$mean) %>%
  plot_time_series_cv_plan(.date_var = index,
                           .value = value,
                           .interactive=F,
                           .facet_vars = .id,
                           .facet_ncol=3,
                           .facet_scale="free_x")

df.cv$splits[[1]] %>% testing() %>% .$value %>% as.vector()

get.dataset=dataset(
  name="dataset",
  initialize= function(M, scale_factor=1){
    self$M= M
    n= nrow(M)
    self$index= sample.int(n, n*scale_factor) %>% sort()
  },
  .getitem=function(i) {
    x = self$M$splits[[i]] %>% training() %>% .$value %>% as.matrix(ncol=1) %>% torch_tensor()
    y = self$M$splits[[i]] %>% testing() %>% .$value %>% as.vector() %>% torch_tensor()
    
    list(x,y)
  },
  .length = function() {
    length(self$index)
  }
)

#6 dataloader
batch.size=512

valid.ratio= 0
n = nrow(df.cv)-1
idx = sample.int(n, n*valid.ratio)

test= df.cv %>% filter(id=="Slice0001")
valid= anti_join(df.cv[idx, ], test)
train= anti_join(df.cv, valid) %>% anti_join(test)

train.ds= get.dataset(train)
valid.ds= get.dataset(valid)
test.ds= get.dataset(test)

train.loader= dataloader(train.ds, batch_size = batch.size)
valid.loader=dataloader(valid.ds, batch_size = batch.size)
test.loader= dataloader(test.ds, batch_size = 1)

#7 network
network= nn_module(
  classname = "lstm",
  initialize= function(in.size, h.size, out.size, num_layer=1,dropout=0){
    self$lstm1= nn_lstm(input_size = in.size,
                        hidden_size = h.size,
                        num_layers = num_layer,
                        batch_first = T,
                        bidirectional = F,
                        dropout = dropout)
    self$lstm2= nn_lstm(input_size = h.size,
                        hidden_size = h.size,
                        num_layers = num_layer,
                        batch_first = T,
                        bidirectional = F,
                        dropout = dropout)
    self$fc1= nn_linear(h.size, 24)
    self$fc2= nn_linear(24, out.size)
  },
  forward= function(x){
    lstm.out1= self$lstm1(x)[[1]] #[batch,initial.size,input] -> [batch,initial.size,h.size]
    lstm.out2= self$lstm2(lstm.out1)[[1]]
    fc.in= lstm.out2[,dim(x)[2],]
    output= self$fc1(fc.in) %>% nnf_layer_norm(24) %>% nnf_leaky_relu() %>% self$fc2()
  }
)

#8. setup & fitting

model=
  network %>% 
  setup(loss= nn_mse_loss(),
        optimizer = optim_adam,
        metrics = luz_metric_mse()) %>% 
  set_hparams(in.size= 1, h.size= 48, out.size= assess.size, num_layer=2, dropout=0.7) %>% 
  set_opt_hparams(lr=0.005) %>% 
  fit(
    data= train.loader,
    epochs= 600,
    verbose=T
  )

#9. plot loss
model$ctx$get_metrics_df() %>% 
  ggplot(aes(x=epoch, y=value))+
  geom_point(aes(color=set), size=1.5, alpha=0.5)+
  geom_path(aes(color=set), size=0.2)+
  facet_wrap(~metric, scales = "free")

#10. plot prediction

pred= predict(model, test.loader)$to(device="cpu")
pred= pred %>% as_array() %>% as.vector()

df.actual=
  bind_rows(test$splits[[1]] %>% training(),
            test$splits[[1]] %>% testing()) %>% 
  mutate(value= value*scale.factor$sd+scale.factor$mean) %>% 
  add_column(remark="acutal")

df.lstm =
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(pred*scale.factor$sd+scale.factor$mean)),
         remark="lstm")

p1= 
  df.actual%>% 
  ggplot(aes(x=index, y= value))+
  geom_point(color="black", size=2, alpha=0.5)+
  geom_smooth(color="black", method="loess", color="black", span=0.2, size=1, se=F, linetype=2)+
  geom_path(color="black", size=0.1)

p1+
  geom_path(data=df.lstm, aes(index,value), color="red", size=2, alpha=0.35)

actual= df.actual$value[(nrow(df.actual)-initial.size+1):nrow(df.actual),]
lstm= pred*scale.factor$sd+scale.factor$mean

rmse_vec(actual, lstm)
#11. data prepare for other models


idx= df %>% 
  set_names(c("ds","y")) %>% 
  initial_time_split(prop = 3057/3177)

train.df= training(idx)
test.df= testing(idx)
#12. linear model

rcp.lm=
  recipe(y~., data=train.df) %>% 
  step_timeseries_signature(ds) %>% 
  step_rm(-all_outcomes(),all_predictors(),-ds_year, -ds_month) %>% 
  step_num2factor(ds_month, levels = as.character(c(1:12))) %>% #월을 수치변수가 아닌 factor로...
  step_dummy(ds_month, one_hot = T) #월을 각각 독립변수화 한다.

prep(rcp.lm) %>% bake(train.df)

#2. model
model.lm=
  linear_reg() %>% 
  set_engine(engine = "lm") %>% 
  set_mode("regression")

#3. fit
fit.lm=
  workflow() %>% 
  add_model(model.lm) %>% 
  add_recipe(rcp.lm) %>% 
  fit(data= train.df)

#13. other models

#1. recipe
rcp.ets= recipe(y~ds, data=train.df)

prep(rcp.ets) %>% bake(train.df)

#df.prophet= df %>% set_names(c("ds","y"))
#rcp.prophet= recipe(y~ds, data=train.df)

#2. model
model.ets=
  exp_smoothing() %>% 
  set_engine("ets") %>% 
  set_mode("regression")

model.arima= arima_reg() %>% 
  set_engine("auto_arima") %>% 
  set_mode("regression")

model.prophet=
  prophet_reg() %>% 
  set_engine("prophet") %>% 
  set_mode("regression")

#3. fitting
fit.ets=
  workflow() %>% 
  add_model(model.ets) %>% 
  add_recipe(rcp.ets) %>% 
  fit(data= train.df)

fit.arima= workflow() %>% 
  add_model(model.arima) %>% 
  add_recipe(rcp.ets) %>% 
  fit(data=train.df)

fit.prophet=
  workflow() %>% 
  add_model(model.prophet) %>% 
  add_recipe(rcp.ets) %>% 
  fit(data=train.df)

#4. model-time
model.tbl= 
  modeltime_table(
    ets= fit.ets,
    lm= fit.lm,
    arima= fit.arima,
    prophet= fit.prophet)

model.tbl %>% 
  modeltime_accuracy(new_data = test.df)

#14. plot prediction-all

df.actual=
  bind_rows(test$splits[[1]] %>% training(),
            test$splits[[1]] %>% testing()) %>% 
  mutate(value= value*scale.factor$sd+scale.factor$mean) %>% 
  add_column(remark="acutal")

df.lstm =
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(pred*scale.factor$sd+scale.factor$mean)),
         remark="lstm")

df.lm= 
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(predict(fit.lm, test.df) %>% .$.pred)),
         remark="lm")

df.ets= 
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(predict(fit.ets, test.df) %>% .$.pred)),
         remark="ets")

df.arima= 
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(predict(fit.arima, test.df) %>% .$.pred)),
         remark="arima")

df.prophet= 
  df.actual %>% 
  mutate(value= c(rep(NA,initial.size),(predict(fit.prophet, test.df) %>% .$.pred)),
         remark="prophet")

p1=
  bind_rows(df.actual) %>% 
  ggplot(aes(index, value))+
  geom_point(color="black", size=2, alpha=0.5)+
  geom_path(color="black", size=0.1)+
  geom_smooth(color="black", span=0.3, size=0.8, alpha=1, se=F, linetype=2)

p1+
  geom_line(data=
              bind_rows(df.lm, df.ets, df.arima, df.prophet, df.lstm) %>%
              rename(Model=remark),
            aes(x=index,y=value, color=Model), alpha=0.4, size=2)+
  theme(legend.position = "bottom")

