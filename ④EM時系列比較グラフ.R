# C1ファイル読込
C1_maintenance <- read_tsv("./tsv_data/C1_maintenance.tsv")　# 保守データ
C1_machine <- read_tsv("./tsv_data/C1_machine.tsv")          # 機器データ

# 現象項目
C1_maintenance %>% distinct(Phenomenon) %>% dput()
# 周辺機名称
C1_maintenance$Peripheral_name
# 現象項目の中で上位10項目を抽出
Phe10 <- 
  head(levels(fct_infreq(C1_maintenance$Phenomenon)), n=10)
Phe10[5]
# names(C1_maintenance)
# 保守データから日付ごとのEM件数を取得
Phenom <- Phe10[5]
Phenom <- "ソフトエラ―表示"
EM.count <- 
  C1_maintenance %>% 
  dplyr::filter(is.na(Peripheral_name)) %>%
  dplyr::filter(Phenomenon == Phenom) %>%
  group_by(Maintenance_date) %>%
  summarise(
    EM.count = n()
  ) %>% 
  arrange(-EM.count) %>%
  ungroup()

# 市場機台数
MIF.count <- 
  C1_machine %>% 
  group_by(機種略機番,納品年月日) %>% 
  summarise(
    num = n()
  ) %>% 
  group_by(納品年月日) %>% 
  summarise(
    N.date = n()
  ) %>% 
  ungroup()

# 結合
X.date <- seq(as.POSIXct("2013-03-01"), as.POSIXct("2016-05-31"), by = "day")
MIF.date <- tibble(X.date = X.date)
C1.MIF_by.date <- 
  MIF.date %>% 
  full_join(MIF.count, by=c("X.date" = "納品年月日")) %>% 
  full_join(EM.count, by=c("X.date" = "Maintenance_date")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    MIF.cumsum = cumsum(N.date)
  ) %>% 
  dplyr::filter(X.date >= "2013-06-01") %>% 
  mutate(
    num = row_number(),
    model = "C1"
  )

# MF3ファイル読込
MF3_maintenance <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")　    # 保守データ
MF3_machine <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")          # 機器データ

# 現象項目
MF3_maintenance %>% distinct(Phenomenon) %>% dput()
# 周辺機名称
MF3_maintenance$Peripheral_name
# 現象項目の中で上位10項目を抽出
Phe10.MF3 <- 
  head(levels(fct_infreq(MF3_maintenance$Phenomenon)), n=10)
Phe10.MF3[7]
# names(MF3_maintenance)
# 保守データから日付ごとのEM件数を取得
Phenom.MF3 <- Phe10.MF3[7]
# Phenom.MF3 <- "全体"
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
  ) %>% 
  dplyr::filter(X.date >= "2019-02-01") %>% 
  mutate(
    num = row_number(),
    model = "MF3"
  )

# 結合
MIF_by.date <- 
  MF3.MIF_by.date %>% 
  bind_rows(C1.MIF_by.date)

# ファイル保存
# write_tsv(MIF_by.date,"./tsv_data/MIF_by.date.tsv")

# モデル読込み
fit.C1 <- read_rds(str_c("./Cmdstan_files/Metis-C1.system.EM.fit.",Phenom,".rds"))
fit.C1$print(c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"))
# fit.C1 %>% bayesplot::rhat() %>% hist

fit.MF3 <- read_rds(str_c("./Cmdstan_files/Metis-MF3.system.EM.fit.",Phenom.MF3,".rds"))
fit.MF3$print(c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"))
# fit.MF3 %>% bayesplot::rhat() %>% hist


#### 時系列グラフ比較の関数 -------------------------

plotSSM.comparison <- function(
    fit.1 = fit.C1,
    fit.2 = fit.MF3,
    state_name = "mu_tvc_exp",
    graph_title =  "Metisにおける時系列比較 (μ + tvc：水準成分＋時変係数成分)",
    y_label = "EM件数",
    obs_vec = NULL){
  
  # すべての時点の状態の、95%区間と中央値
  result_df.1 <- data.frame(t(apply(
    X = (fit.1$draws(state_name) %>% as_draws_df),
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  
  result_df.2 <- data.frame(t(apply(
    X = (fit.2$draws(state_name) %>% as_draws_df),
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  # 列名の変更
  colnames(result_df.1) <- c("lwr", "fit", "upr")
  colnames(result_df.2) <- c("lwr", "fit", "upr")
  
  result_df.1 <- 
    result_df.1[1:(nrow(result_df.1)-3),] %>% 
    mutate(num = row_number(),
           model = "C1")
  
  result_df.2 <- 
    result_df.2[1:(nrow(result_df.2)-3),] %>% 
    mutate(num = row_number(),
           model = "MF3")
  
  # nrow(result_df.1)
  # nrow(result_df.2)
  
  result_df <- 
    result_df.2 %>% 
    bind_rows(result_df.1)
  
  result_df <- 
    result_df %>% 
    tibble() %>% 
    dplyr::select(num, everything())
  
  result_df <- 
    result_df %>% 
    left_join(MIF_by.date, by=c("num"="num", "model"="model"))
  
  
  # フォント定義
  par(family="Noto Sans")
  # 図示
  p <- ggplot(data = result_df, aes(x = num)) + 
    theme_bw() + 
    labs(title = graph_title) +
    # scale_colour_jco() + scale_fill_jco() +
    # scale_colour_npg() + scale_fill_npg() +
    # scale_colour_jama(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) + 
    # scale_fill_jama(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) +
    # scale_color_hue(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) +
    # scale_fill_hue(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) +
    scale_colour_brewer(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3"), palette="Accent") +
    scale_fill_brewer(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3"), palette="Accent") +
    ylab(y_label) + xlab("経過日数") +
    scale_x_continuous(breaks=seq(0,1400,100), labels = label_comma()) +
    scale_y_continuous(labels = label_number()) + # eで省略されないそのままの値
    theme(plot.title = element_text(size = 24, hjust = 0.01)) +
    geom_ribbon(mapping = aes(ymin = lwr, ymax = upr, fill = model), alpha = 0.3) +
    geom_line(mapping = aes(y = fit, colour = model), linewidth = 1.2) +
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.position = c(0.9, 0.85), legend.justification = "center")
    
  
  # 観測値をグラフに追加
  if(!is.null(obs_vec)){
    p <- p +
      geom_point(mapping = aes(y = EM.count, colour=model), alpha = 0.6, size = 2.0)
  }
  # グラフを返す
  return(p)
}

# 合体グラフの関数
plot.union <- function(){
  # すべての成分を含んだ状態推定値の図示
  p_lambda_pois <- 
    plotSSM.comparison(state_name = "lambda_exp", 
                       graph_title = "λ(μ + γ + tvf + r)：すべての成分を含んだ状態推定値",
                       obs_vec = 0)
  # 水準成分
  p_mu_pois <- 
    plotSSM.comparison(state_name = "mu_exp", 
                       graph_title =  "μ：水準成分/自己回帰")
  # 水準成分＋時変係数成分
  p_mu_tvc_pois <- 
    plotSSM.comparison(state_name = "mu_tvc_exp",
                       graph_title = "μ + tvf：水準成分＋時変係数成分")
  # 時変係数×1台稼働
  p_tvc_1_pois <- 
    plotSSM.comparison(state_name = "tvc_1", 
                       graph_title = "tvc：時変係数", 
                       y_label = "tvc",
                       obs_vec = NULL)
  # 時変係数ｘ稼働台数
  p_tvc_pois <- 
    plotSSM.comparison(state_name = "tvc_exp",  
                       graph_title = "tvf：時変係数ｘ稼働台数", 
                       obs_vec = NULL)
  # グラフ描画
  plot <- plot_grid(p_lambda_pois,
                    p_mu_tvc_pois,
                    p_mu_pois,
                    p_tvc_pois,
                    p_tvc_1_pois,
                    ncol = 1, 
                    align = "v")
  # now add the title
  title <- ggdraw() + 
    draw_label(
      str_c("Metis-C１＆ MF3　　EM現象：", Phenom.MF3),
      fontface = 'bold',
      color = "darkblue",
      size = 30,
      x = 0,
      hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 100))
  
  plot.title <- 
    plot_grid(
      title, plot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.03, 1)
    )
  
  # グラフ保存
  ggsave(str_c("./PDF/Metis-EM時系列比較_AR.tvc.period_poisson.",Phenom.MF3,".pdf"), 
         plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)
}

plot.union()


library(RColorBrewer)
display.brewer.all()

# # すべての成分を含んだ状態推定値の図示
# p_lambda_pois <- 
#   plotSSM.comparison(state_name = "lambda_exp", 
#                      graph_title = "λ(μ + γ + tvf + r)：すべての成分を含んだ状態推定値",
#                      obs_vec = 0)
# # ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(λすべての成分を含んだ状態推定値).pdf"), 
# #        plot = p_lambda_pois, device = cairo_pdf, dpi=300, width=20, height=5)
# 
# # 水準成分
# p_mu_pois <- 
#   plotSSM.comparison(state_name = "mu_exp", 
#                      graph_title =  "μ：水準成分/自己回帰")
# # ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(μ水準成分).pdf"), 
# #        plot = p_mu_pois, device = cairo_pdf, dpi=300, width=20, height=5)
# 
# # 水準成分＋時変係数成分
# p_mu_tvc_pois <- 
#   plotSSM.comparison(state_name = "mu_tvc_exp",
#                      graph_title = "μ + tvf：水準成分＋時変係数成分")
# # ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(μ+tvc水準成分＋時変係数成分).pdf"), 
# #        plot = p_mu_tvc_pois, device = cairo_pdf, dpi=300, width=20, height=5)
# 
# # 時変係数×1台稼働
# p_tvc_1_pois <- 
#   plotSSM.comparison(state_name = "tvc_1", 
#                      graph_title = "tvc：時変係数", 
#                      y_label = "tvc",
#                      obs_vec = NULL)
# # ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(tvc時変係数).pdf"), 
# #        plot = p_tvc_1_pois, device = cairo_pdf, dpi=300, width=20, height=5)
# 
# # 時変係数ｘ稼働台数
# p_tvc_pois <- 
#   plotSSM.comparison(state_name = "tvc_exp",  
#                      graph_title = "tvf：時変係数ｘ稼働台数", 
#                      obs_vec = NULL)
# # ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(tvc時変係数ｘ稼働台数数).pdf"), 
# #        plot = p_tvc_pois, device = cairo_pdf, dpi=300, width=20, height=5)
# 
# # グラフ描画
# plot <- plot_grid(p_lambda_pois,
#                   p_mu_tvc_pois,
#                   p_mu_pois,
#                   p_tvc_pois,
#                   p_tvc_1_pois,
#                   ncol = 1, 
#                   align = "v")
# # now add the title
# title <- ggdraw() + 
#   draw_label(
#     str_c("Metis-C１＆ MF3　　EM現象：", Phenom),
#     fontface = 'bold',
#     color = "blue",
#     size = 30,
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 100)
#   )
# 
# 
# plot.title <- 
#   plot_grid(
#     title, plot,
#     ncol = 1,
#     # rel_heights values control vertical title margins
#     rel_heights = c(0.03, 1)
#   )
# 
# # グラフ保存
# ggsave(str_c("./PDF/Metis-EM時系列比較_AR.tvc.period_poisson.",Phenom,".pdf"), 
#        plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)

# 推定結果の図示 -----------------------------------------------------------------

# state_name <- "mu_tvc_exp"
# graph_title <-  "Metisにおける時系列比較 (μ + tvc：水準成分＋時変係数成分)"
# y_label = "EM件数"
# date_labels <-  "%Y/%m"
# date_breaks <-  "2 month"
# # すべての時点の状態の、95%区間と中央値
# result_df.C1 <- data.frame(t(apply(
#   X = (fit.C1$draws(state_name) %>% as_draws_df),
#   MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
# )))
# 
# result_df.MF3 <- data.frame(t(apply(
#   X = (fit.MF3$draws(state_name) %>% as_draws_df),
#   MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
# )))
# # 列名の変更
# colnames(result_df.C1) <- c("lwr", "fit", "upr")
# colnames(result_df.MF3) <- c("lwr", "fit", "upr")
# 
# result_df.C1 <- 
#   result_df.C1[1:(nrow(result_df.C1)-3),] %>% 
#   mutate(num = row_number(),
#          model = "C1")
# 
# result_df.MF3 <- 
#   result_df.MF3[1:(nrow(result_df.MF3)-3),] %>% 
#   mutate(num = row_number(),
#          model = "MF3")
# 
# nrow(result_df.C1)
# nrow(result_df.MF3)
# 
# result_df <- 
#   result_df.MF3 %>% 
#   bind_rows(result_df.C1)
# 
# result_df <- 
#   result_df %>% 
#     tibble() %>% 
#     dplyr::select(num, everything())
# 
# result_df <- 
#   result_df %>% 
#     left_join(MIF_by.date, by=c("num"="num", "model"="model"))
# 
# 
# # フォント定義
# par(family="Noto Sans")
# # 図示
# p <- ggplot(data = result_df, aes(x = num)) + 
#   theme_bw() + 
#   labs(title = graph_title) +
#   scale_color_hue(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) +
#   scale_fill_hue(name = "機種", labels = c(C1 = "Metis-C1", MF3 ="Metis-MF3")) +
#   ylab("EM件数") + xlab("経過日数") +
#   scale_x_continuous(breaks=seq(0,1400,100), labels = label_comma()) +
#   theme(plot.title = element_text(size = 18, hjust = 0.01)) +
#   geom_line(mapping = aes(y = fit, colour = model), linewidth = 1.2) +
#   geom_ribbon(mapping = aes(ymin = lwr, ymax = upr, fill = model), alpha = 0.5) +
#   theme(axis.title.x = element_text(size = 16),
#         axis.title.y = element_text(size = 16),
#         axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
#         axis.text.y = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         legend.text = element_text(size = 16),
#         legend.position = c(0.1, 0.8), legend.justification = "center")
# 
# # 観測値をグラフに追加
# p.obs <- p +
#   geom_point(mapping = aes(y = EM.count, color=model), alpha = 0.6, size = 0.9)
# 
# # グラフ保存
# ggsave(str_c("./PDF/Metis-EM-",Phenom,"-C1-MF3比較(μ+tvc).pdf"), 
#        plot = p, device = cairo_pdf, dpi=300, width=20, height=5)