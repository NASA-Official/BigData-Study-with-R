
# install.packages("modeltime", repos = "http://cran.us.r-project.org")
# install.packages("timetk", repos = "http://cran.us.r-project.org")
# install.packages("tidymodels", repos = "http://cran.us.r-project.org")
# install.packages("cowplot", repos = "http://cran.us.r-project.org")
# install.packages("torch", repos = "http://cran.us.r-project.org")
# install.packages("luz", repos = "http://cran.us.r-project.org")
# install.packages("httpgd", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(cowplot)
library(lubridate)
library(torch)
library(luz)




df = sunspot.month %>% tk_tbl() %>% mutate(index = as_date(index))
df %>%
  tk_index() %>%
  tk_get_timeseries_summary()


p1=
  df %>% 
  ggplot(aes(index, value))+
  geom_point(color= "black", alpha=0.5, size=1)+
  labs(title = "1749~2013")

p2=
  df %>% 
  filter_by_time(.date_var = index,
                 .start_date = "start",
                 .end_date= "1800") %>% 
  ggplot(aes(index, value))+
    geom_line(color="gray")+
    geom_point(color="black", size=1)+
    geom_smooth(method="loess", color="blue", span=0.2, size=0.4, se=F)+
    labs(title = "1479~1800")


plot_grid(p1,p2,ncol=1)


df %>% 
  plot_acf_diagnostics(.date_var = index,
                       .value = value,
                       #.lags = 120,
                       .show_white_noise_bars = T,
                       .interactive = F)+
  geom_vline(xintercept = c(120,240,360,480,600,720,840,960), color="red", size=0.3, linetype=2)


scale.factor = list(mean = mean(df$value), sd = sd(df$value))

# $mean
# [1] 51.96481

# $sd
# [1] 44.12524

head(df)
tail(df)


# 30년치 데이터 학습
initial.size = 30 * 12
assess.size = 30 * 12

df.cv = df %>% mutate(value = scale(value)) %>%
  time_series_cv(
    date_var = index,
    initial = initial.size,
    assess = assess.size,
    skip = 1,
    cumulative = F
  )

df.cv %>% 
  filter(id %in% c("Slice0001", "Slice0055", "Slice1003")) %>% 
  tk_time_series_cv_plan() %>% 
  mutate(value= value*scale.factor$sd+scale.factor$mean) %>% 
  plot_time_series_cv_plan(.date_var = index,
                           .value= value,
                           .interactive=F,
                           .facet_vars= .id,
                           .facet_ncol=3,
                           .facet_scale="free_x")

df.cv$splits[[1]] %>% testing() %>% .$value %>% as.vector()

# network
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
# setup & fitting

model= network %>% 
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



# predict

pred = predict(

)