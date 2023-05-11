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
Phe10.MF3[1]
names(MF3_maintenance)
# 保守データから日付ごとのEM件数を取得
Phenom.MF3 <- Phe10.MF3[1]
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

# 基本構造時系列モデルの推定 --------------------------------------------------------

# データ長 #1430
length(MF3.MIF_by.date$X.date)
MF3.MIF_by.date$X.date[1066]

# データの準備
end_day = 1430
start_day = 1066
pred_term = 30 # 予測期間
# データ
data_list <- list(
  y = MF3.MIF_by.date$EM.count[start_day:end_day],
  Num = MF3.MIF_by.date$MIF.cumsum[start_day:end_day]/max(MF3.MIF_by.date$MIF.cumsum[start_day:end_day]), # 正規化が必要
  T = end_day-start_day+1,
  N_max = max(MF3.MIF_by.date$MIF.cumsum[start_day:end_day]),
  grainsize = 250,
  pred_term = pred_term,
  Num_gain = 200/max(MF3.MIF_by.date$MIF.cumsum[start_day:end_day])
)
# サンプリング開始日/終了日
MF3.MIF_by.date$X.date[start_day]
MF3.MIF_by.date$X.date[end_day]

# コンパイル
mod <- cmdstan_model("./stan/bsts-AR-tvc-rd-poisson-remod_predict.stan", cpp_options = list(stan_threads = TRUE))
# MCMCサンプリング
fit <- mod$sample(data = data_list,
                  seed = 1234,
                  chains = 6,
                  parallel_chains = getOption("mc.cores", 24),
                  threads_per_chain = 2,
                  iter_warmup = 60000,
                  iter_sampling = 20000,
                  thin = 20,
                  # adapt_delta = 0.90,
                  max_treedepth = 15,
                  refresh = 500)

fit$print(c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"))
fit %>% bayesplot::rhat() %>% hist

# 推定結果の図示 -----------------------------------------------------------------
# フォント設定
par(family="Noto Sans")
# x軸の年月日設定
MF3.MIF_by.date$X.date[start_day]
date_plot <- 
  seq(
    from = as.POSIXct(MF3.MIF_by.date$X.date[start_day]),
    by = "days",
    len = end_day-start_day+1+pred_term
  )
length(date_plot)
date_plot.MF3 <- tibble(X.date = date_plot)
date_plot.MF3.MIF_by.date.1 <- 
  date_plot.MF3 %>% 
  left_join(MIF.count.MF3, by=c("X.date" = "納品年月日")) %>% 
  left_join(EM.count.MF3, by=c("X.date" = "Maintenance_date")) %>% 
  dplyr::filter(X.date <= MF3.MIF_by.date$X.date[end_day]) %>% 
  mutate_if(where(is.numeric), ~replace(., is.na(.), 0))
date_plot.MF3.MIF_by.date.2 <- 
  date_plot.MF3 %>% 
  left_join(MIF.count.MF3, by=c("X.date" = "納品年月日")) %>% 
  left_join(EM.count.MF3, by=c("X.date" = "Maintenance_date")) %>% 
  dplyr::filter(X.date > MF3.MIF_by.date$X.date[end_day])
date_plot.MF3.MIF_by.date <- 
  bind_rows(date_plot.MF3.MIF_by.date.1,date_plot.MF3.MIF_by.date.2)

# すべての成分を含んだ状態推定値の図示
p_lambda_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date_plot,
                                  obs_vec = date_plot.MF3.MIF_by.date$EM.count,
                                  state_name = "lambda_exp", 
                                  graph_title = str_c("EM:",Phenom.MF3," λ(μ + γ + tvc + r)：すべての成分を含んだ状態推定値"), 
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
                              graph_title = "μ：水準成分/自己回帰", 
                              y_label = "件数",
                              date_labels = "%Y/%m",
                              date_breaks = "2 month") 
# 事後予測区間
p_pred_pois <- plotSSM.CmdStanr(fit = fit, 
                                time_vec = date_plot,
                                obs_vec = date_plot.MF3.MIF_by.date$EM.count,
                                state_name = "y_pred", 
                                graph_title = str_c("EM:",Phenom.MF3,"95%予測区間"), 
                                y_label = "件数",
                                date_labels = "%Y/%m",
                                date_breaks = "2 month")
# ドリフト成分
p_drift_pois <- plotSSM.CmdStanr(fit = fit, 
                                 time_vec = date_plot[32:(end_day-start_day+1+30)],
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
    str_c("Metis-MF3　　EM現象：", Phenom),
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
ggsave(str_c("./PDF/Metis-MF3-EM_AR.tvc.period_poisson.",Phenom.MF3,".pdf"), 
       plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)

# グラフ保存
ggsave(str_c("./PDF/Metis-MF3-EM-",Phenom.MF3,"_1年間+予測区間.pdf"), 
       plot = p_pred_pois, device = cairo_pdf, dpi=300, width=20, height=5)

ggsave(str_c("./PDF/Metis-MF3-EM-",Phenom.MF3,"_1年間+信頼区間.pdf"), 
       plot = p_lambda_pois, device = cairo_pdf, dpi=300, width=20, height=5)

