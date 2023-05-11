#### EM件数の基本構造時系列モデルでの推定 ####

# ファイル読込
MF3_maintenance <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")　    # 保守データ
MF3_machine <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")          # 機器データ

# 現象項目
MF3_maintenance %>% distinct(Phenomenon) %>% dput()
# 周辺機名称
MF3_maintenance$Peripheral_name
# 現象項目の中で上位10項目を抽出
Phe10.MF3 <- 
  head(levels(fct_infreq(MF3_maintenance$Phenomenon)), n=10)
Phe10.MF3[8]
# names(MF3_maintenance)
# 保守データから日付ごとのEM件数を取得
Phenom.MF3 <- Phe10.MF3[8]
EM.count.MF3 <- 
  MF3_maintenance %>% 
  dplyr::filter(is.na(Peripheral_name)) %>%
  dplyr::filter(Phenomenon == Phenom.MF3) %>%
  group_by(Maintenance_date) %>%
  summarise(
    EM.count = n()
  ) %>% 
  arrange(-EM.count) %>%
  ungroup()

# 市場機台数
MIF.count.MF3 <- 
  MF3_machine %>% 
  group_by(機種機番,納品年月日) %>% 
  summarise(
    num = n()
  ) %>% 
  group_by(納品年月日) %>% 
  summarise(
    N.date = n()
  ) %>% 
  ungroup()

# 結合
X.date.MF3 <- seq(as.POSIXct("2019-01-01"), as.POSIXct("2022-11-30"), by = "day")
MIF.date.MF3 <- tibble(X.date = X.date.MF3)
MF3.MIF_by.date <- 
  MIF.date.MF3 %>% 
  full_join(MIF.count.MF3, by=c("X.date" = "納品年月日")) %>% 
  full_join(EM.count.MF3, by=c("X.date" = "Maintenance_date")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    MIF.cumsum = cumsum(N.date)
  )

# ファイル保存
# write_tsv(MF3.MIF_by.date,"./tsv_data/MF3.MIF_by.date.tsv")

# 基本構造時系列モデルの推定 --------------------------------------------------------

# データ長 #1430
length(MF3.MIF_by.date$X.date)
MF3.MIF_by.date$X.date[32]

# データの準備
end_day = 1430
start_day = 32
# データ
data_list <- list(
  y = MF3.MIF_by.date$EM.count[start_day:end_day],
  Num = MF3.MIF_by.date$MIF.cumsum[start_day:end_day]/max(MF3.MIF_by.date$MIF.cumsum[start_day:end_day]), # 正規化が必要
  T = end_day-start_day+1,
  N_max = max(MF3.MIF_by.date$MIF.cumsum[start_day:end_day]),
  grainsize = 250
)
# サンプリング開始日/終了日
MF3.MIF_by.date$X.date[start_day]
MF3.MIF_by.date$X.date[end_day]

# 自己回帰成分
AR <- TRUE

# コンパイル
if(AR ==TRUE){
  mod <- cmdstan_model("./stan/basic-structual-time-series-AR-tvc-rd-poisson-remod.stan", cpp_options = list(stan_threads = TRUE))
}else{
  mod <- cmdstan_model("./stan/bsts-tvc-rd-poisson.stan", cpp_options = list(stan_threads = TRUE)) # 自己回帰成分なし
}

# サンプリング関数
mod.sample <- function(
  end_day = 1188,
  start_day = 62,
  df = C1.MIF_by.date,
  data = data_list,
  chains = 6, 
  parallel_chains = getOption("mc.cores", 6),
  threads_per_chain = 3,
  iter_warmup = 3000,
  iter_sampling = 2000,
  thin = 4,
  adapt_delta = NULL, # 0.80
  max_treedepth = NULL, # 10
  refresh = 100,
  AR = FALSE,
  output_basename = NULL,
  output_dir ="./csv",
  show_messages = FALSE,
  object.path = "./Cmdstan_files/C1.system.EM.fit.rds",
  graph.path = "./PDF/C1_EM_周期性_自己回帰モデル_AR_tvc_poisson.pdf"
  ){
  print(Sys.time(), quote=F)
  # マルチコア対応
  options(mc.cores = parallel::detectCores())
  # csv保存ディレクトリーの作成
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  # MCMCサンプリング
  fit <- mod$sample(data = data, 
                    seed = 1234,
                    chains = chains, 
                    parallel_chains = parallel_chains,
                    threads_per_chain = threads_per_chain,
                    iter_warmup = iter_warmup,
                    iter_sampling = iter_sampling,
                    thin = thin,
                    adapt_delta = adapt_delta,
                    max_treedepth = max_treedepth,
                    refresh = refresh,
                    output_dir = output_dir,
                    output_basename = output_basename,
                    show_messages = show_messages)
  
  fit$save_object(file = object.path)
  if(AR == TRUE){
    fit$print(c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"))
  }else{
    fit$print(c("s_w", "s_s", "s_r", "s_t","b_ope[1]", "Intercept", "lp__"))
  }
  fit %>% bayesplot::rhat() %>% hist
  
  # 推定結果の図示
  # x軸の年月日設定
  date_plot <- 
    seq(
      from = as.POSIXct(df$X.date[start_day]),
      by = "days",
      len = end_day-start_day+1
    )
  # すべての成分を含んだ状態推定値の図示
  p_lambda_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date_plot,
                                    obs_vec = df$EM.count[start_day:end_day],
                                    state_name = "lambda_exp", 
                                    graph_title = "λ(μ + γ + tvc + r)：すべての成分を含んだ状態推定値", 
                                    y_label = "件数",
                                    date_labels = "%Y/%m",
                                    date_breaks = "2 month") 
  # 水準成分＋周期成分
  p_mu_gamma_pois <- plotSSM.CmdStanr(fit = fit, 
                                      time_vec = date_plot,
                                      # obs_vec = df$EM.count[start_day:end_day],
                                      state_name = "mu_gamma_exp", 
                                      graph_title = "μ + γ：水準成分＋周期成分", 
                                      y_label = "件数",
                                      date_labels = "%Y/%m",
                                      date_breaks = "2 month") 
  # 水準成分＋ランダム成分
  p_mu_r_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date_plot,
                                  # obs_vec = df$EM.count[start_day:end_day],
                                  state_name = "mu_r_exp", 
                                  graph_title = "μ + r：水準成分＋ランダム成分", 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = "2 month") 
  # 水準成分
  if(AR ==TRUE){
    p_mu_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date_plot,
                                  # obs_vec = df$EM.count[start_day:end_day],
                                  state_name = "mu_exp", 
                                  graph_title = "μ：水準成分/自己回帰", 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = "2 month")
  }else{
    p_mu_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date_plot,
                                  # obs_vec = df$EM.count[start_day:end_day],
                                  state_name = "mu_exp", 
                                  graph_title = "μ：水準成分", 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = "2 month")
  }
   
  # 事後予測区間
  p_pred_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date_plot,
                                  obs_vec = df$EM.count[start_day:end_day],
                                  state_name = "pred_y", 
                                  graph_title = "95%予測区間", 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = "2 month")
  # ドリフト成分
  p_drift_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date_plot[32:(end_day-start_day+1)],
                                   # time_vec = date_plot[(start_day+31):end_day],
                                   state_name = "delta_exp",
                                   graph_title = "δ：ドリフト成分",
                                   y_label = "delta",
                                   date_labels = "%Y/%m",
                                   date_breaks = "2 month")
  # 周期成分
  p_cycle_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date_plot,
                                   state_name = "gamma_exp", 
                                   graph_title = "γ：周期成分", 
                                   y_label = "gamma",
                                   date_labels = "%Y/%m",
                                   date_breaks = "2 month") 
  # ランダム成分
  p_random_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date_plot,
                                    state_name = "r_exp", 
                                    graph_title = "r：ランダム成分", 
                                    y_label = "r",
                                    date_labels = "%Y/%m",
                                    date_breaks = "2 month")
  # 水準成分＋時変係数成分
  p_mu_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date_plot,
                                    # obs_vec = df$EM.count[start_day:end_day],
                                    state_name = "mu_tvc_exp", 
                                    graph_title = "μ + tvf：水準成分＋時変係数成分", 
                                    y_label = "件数",
                                    date_labels = "%Y/%m",
                                    date_breaks = "2 month") 
  # 時変係数ｘ稼働台数
  p_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                                 time_vec = date_plot,
                                 state_name = "tvc_exp", 
                                 graph_title = "tvf：時変係数ｘ稼働台数", 
                                 y_label = "件数",
                                 date_labels = "%Y/%m",
                                 date_breaks = "2 month")
  # 水準成分+時変係数×1台稼働
  p_mu_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                                      time_vec = date_plot,
                                      # obs_vec = df$EM.count[start_day:end_day],
                                      state_name = "mu_tvc_1_exp", 
                                      graph_title = "μ + tvf：水準成分+時変係数ｘ1台稼働", 
                                      y_label = "件数",
                                      date_labels = "%Y/%m",
                                      date_breaks = "2 month") 
  # 時変係数×1台稼働
  p_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date_plot,
                                   # obs_vec = df$EM.count[start_day:end_day],
                                   state_name = "tvc_1", 
                                   graph_title = "tvc：時変係数", 
                                   y_label = "tvc",
                                   date_labels = "%Y/%m",
                                   date_breaks = "2 month")
  
  # グラフ描画
  plot <- plot_grid(p_lambda_pois, 
                    p_pred_pois,
                    p_mu_tvc_pois,
                    p_tvc_pois,
                    p_tvc_1_pois,
                    p_mu_pois,
                    p_drift_pois,
                    p_random_pois,
                    p_cycle_pois,
                    ncol = 1, 
                    align = "v")
  # グラフタイトル
  title <- ggdraw() + 
    draw_label(
      str_c("Metis-MF3　　EM現象：", Phenom.MF3),
      fontface = 'bold',
      color = "darkblue",
      size = 30,
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 100)
    )
  
  # タイトル合成
  plot.title <- 
    plot_grid(
      title, plot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.03, 1)
    )
  
  # グラフ保存
  ggsave(file=graph.path, plot=plot.title, device=cairo_pdf, dpi=300, width=40, height=30)
  
  print(Sys.time(), quote=F)
  return(fit)
}

if(AR == TRUE){
  output_dir = paste0("./csv/MF3/",Phenom.MF3)
  object.path = str_c("./Cmdstan_files/Metis-MF3.system.EM.fit.",Phenom.MF3,".rds")
  graph.path = str_c("./PDF/Metis-MF3-EM_AR.tvc.period_poisson.",Phenom.MF3,".pdf")
}else{
  output_dir = paste0("./csv/MF3/notAR/",Phenom.MF3)
  object.path = str_c("./Cmdstan_files/Metis-MF3.system.EM.notAR.fit.",Phenom.MF3,".rds")
  graph.path = str_c("./PDF/Metis-MF3-EM_tvc.period_poisson.",Phenom.MF3,".pdf")
}

# MCMCサンプリング
fit <- mod.sample(
  end_day = end_day,
  start_day = start_day,
  df = MF3.MIF_by.date, 
  data = data_list,
  chains = 6, 
  parallel_chains = getOption("mc.cores", 24),
  threads_per_chain = 2,
  iter_warmup = 60000, #60000
  iter_sampling = 20000, #20000
  thin = 20,
  adapt_delta = NULL,
  max_treedepth = 15,
  refresh = 500,
  AR = AR,
  output_basename = paste0(Phenom.MF3,"-",format(Sys.time(), "%H:%M:%S")),
  output_dir = output_dir,
  object.path = object.path,
  graph.path = graph.path
)

# fit <- mod$sample(data = data_list, 
#                   seed = 1234,
#                   chains = 8, 
#                   parallel_chains = 8,
#                   threads_per_chain = 3,
#                   iter_warmup = 2000,
#                   iter_sampling = 2000,
#                   thin = 4,
#                   adapt_delta = 0.90,
#                   max_treedepth = 15,
#                   refresh = 100)

# fit$save_object(file = "./Cmdstan_files/Metis-MF3.system.EM.fit.rds")
fit <- read_rds(str_c("./Cmdstan_files/Metis-MF3.system.EM.fit.",Phenom.MF3,".rds"))
# fit$print(c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"))
fit$print(c("s_w", "s_s", "s_r", "s_t", "b_ope[1]", "Intercept", "lp__"))
fit %>% bayesplot::rhat() %>% hist

# 推定結果の図示 -----------------------------------------------------------------
# フォント設定
par(family="Noto Sans")
# x軸の年月日設定
date_plot <- 
  seq(
    from = as.POSIXct(MF3.MIF_by.date$X.date[start_day]),
    by = "days",
    len = end_day-start_day+1
  )

# すべての成分を含んだ状態推定値の図示
p_lambda_pois <- plotSSM.CmdStanr(fit = fit, 
                         time_vec = date_plot,
                         obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                         state_name = "lambda_exp", 
                         graph_title = "λ(μ + γ + tvc + r)：すべての成分を含んだ状態推定値", 
                         y_label = "件数",
                         date_labels = "%Y/%m",
                         date_breaks = "2 month") 
# 水準成分＋周期成分
p_mu_gamma_pois <- plotSSM.CmdStanr(fit = fit, 
                           time_vec = date_plot,
                           # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                           state_name = "mu_gamma_exp", 
                           graph_title = "μ + γ：水準成分＋周期成分", 
                           y_label = "件数",
                           date_labels = "%Y/%m",
                           date_breaks = "2 month") 
# 水準成分＋ランダム成分
p_mu_r_pois <- plotSSM.CmdStanr(fit = fit, 
                       time_vec = date_plot,
                       # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                       state_name = "mu_r_exp", 
                       graph_title = "μ + r：水準成分＋ランダム成分", 
                       y_label = "件数",
                       date_labels = "%Y/%m",
                       date_breaks = "2 month") 
# 水準成分
p_mu_pois <- plotSSM.CmdStanr(fit = fit, 
                     time_vec = date_plot,
                     # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                     state_name = "mu_exp", 
                     graph_title = "μ：水準成分", 
                     # graph_title = "μ：水準成分/自己回帰",
                     y_label = "件数",
                     date_labels = "%Y/%m",
                     date_breaks = "2 month") 
# 事後予測区間
p_pred_pois <- plotSSM.CmdStanr(fit = fit, 
                       time_vec = date_plot,
                       obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                       state_name = "pred_y", 
                       graph_title = "95%予測区間", 
                       y_label = "件数",
                       date_labels = "%Y/%m",
                       date_breaks = "2 month")
# ドリフト成分
p_drift_pois <- plotSSM.CmdStanr(fit = fit, 
                        time_vec = date_plot[32:(end_day-start_day+1)],
                        # time_vec = date_plot[(start_day+31):end_day],
                        state_name = "delta_exp",
                        graph_title = "δ：ドリフト成分",
                        y_label = "delta",
                        date_labels = "%Y/%m",
                        date_breaks = "2 month")
# 周期成分
p_cycle_pois <- plotSSM.CmdStanr(fit = fit, 
                        time_vec = date_plot,
                        state_name = "gamma_exp", 
                        graph_title = "γ：周期成分", 
                        y_label = "gamma",
                        date_labels = "%Y/%m",
                        date_breaks = "2 month") 
# ランダム成分
p_random_pois <- plotSSM.CmdStanr(fit = fit, 
                         time_vec = date_plot,
                         state_name = "r_exp", 
                         graph_title = "r：ランダム成分", 
                         y_label = "r",
                         date_labels = "%Y/%m",
                         date_breaks = "2 month")
# 水準成分＋時変係数成分
p_mu_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                         time_vec = date_plot,
                         # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                         state_name = "mu_tvc_exp", 
                         graph_title = "μ + tvc：水準成分＋時変係数成分", 
                         y_label = "件数",
                         date_labels = "%Y/%m",
                         date_breaks = "2 month") 
# 時変係数ｘ稼働台数
p_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                      time_vec = date_plot,
                      state_name = "tvc_exp", 
                      graph_title = "tvc：時変係数ｘ稼働台数", 
                      y_label = "件数",
                      date_labels = "%Y/%m",
                      date_breaks = "2 month")
# 水準成分+時変係数×1台稼働
p_mu_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                           time_vec = date_plot,
                           # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                           state_name = "mu_tvc_1_exp", 
                           graph_title = "μ + tvc：水準成分+時変係数ｘ1台稼働", 
                           y_label = "件数",
                           date_labels = "%Y/%m",
                           date_breaks = "2 month") 
# 時変係数×1台稼働
p_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                        time_vec = date_plot,
                        # obs_vec = MF3.MIF_by.date$EM.count[start_day:end_day],
                        state_name = "tvc_1", 
                        graph_title = "tvc：時変係数", 
                        y_label = "tvc",
                        date_labels = "%Y/%m",
                        date_breaks = "2 month")


# グラフ描画
plot <- plot_grid(p_lambda_pois, 
                  p_pred_pois,
                  p_mu_tvc_pois,
                  p_tvc_pois,
                  p_tvc_1_pois,
                  p_mu_pois,
                  p_drift_pois,
                  p_random_pois,
                  p_cycle_pois,
                  ncol = 1, 
                  align = "v")
# now add the title
title <- ggdraw() + 
  draw_label(
    str_c("Metis-MF3　　EM現象：", Phenom.MF3),
    fontface = 'bold',
    color = "blue",
    size = 30,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 100)
  )


plot.title <- 
  plot_grid(
    title, plot,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.03, 1)
  )

# グラフ保存
# ggsave(str_c("./PDF/Metis-MF3-EM_AR.tvc.period_poisson.",Phenom.MF3,".pdf"), 
#        plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)

ggsave(str_c("./PDF/Metis-MF3-EM_tvc.period_poisson.",Phenom.MF3,".pdf"), 
       plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)

