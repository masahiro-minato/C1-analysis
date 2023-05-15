####ローカルレベルモデル推定結果####
# library(scales)
plotSSM.CmdStanr <- function(fit, time_vec, obs_vec = NULL,
                    state_name, graph_title, y_label,
                    date_labels = "%Y年%m月", date_breaks = "2 month"){
  # 状態空間モデルを図示する関数
  #
  # Args:
  #   fit         : CmdStanrでのMCMCサンプル
  #   time_vec    : 時間軸(POSIXct)のベクトル
  #   obs_vec     : (必要なら)観測値のベクトル
  #   state_name  : 図示する状態の変数名
  #   graph_title : グラフタイトル
  #   y_label     : y軸のラベル
  #   date_labels : 日付の書式
  #
  # Returns:
  #   ggplot2により生成されたグラフ
    
  # すべての時点の状態の、95%区間と中央値
  result_df <- data.frame(t(apply(
    X = (fit$draws(state_name) %>% as_draws_df),
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  result_df <- 
    result_df[1:(nrow(result_df)-3),]
  
  # 列名の変更
  colnames(result_df) <- c("lwr", "fit", "upr")
  
  # 時間軸の追加
  result_df$time <- time_vec
  
  # 観測値の追加
  if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  # フォント定義
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  # 図示
  p <- ggplot(data = result_df, aes(x = time)) + 
    theme_bw() + 
    labs(title = graph_title) +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
                                    )) +
    ylab(y_label) +
    geom_line(aes(y = fit), linewidth = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5, fill="lightblue") + 
    scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 16))
    
  
  # 観測値をグラフに追加
  if(!is.null(obs_vec)){
    p <- p + geom_point(alpha = 0.6, size = 0.9, color="red",
                        data = result_df, aes(x = time, y = obs))
  }
  
  # グラフを返す
  return(p)
}
