Dglm_poisson_multi <- function(df, start, end, 
                               T_pred=3, 
                               start_date = "2021-01-01",
                               Machine_numbers = Num_of_JAM_TotalJAM_MN$Machine_numbers,
                               path_stan = "./stan/Dglm_poisson_rd_matrix_probs.stan",
                               iter_warmup = 2000, 
                               iter_sampling = 1000,
                               thin = 2,
                               refresh = 100,
                               y_label = "Total_JAM件数累計",
                               graph_title = "Total_JAM 時系列推移",
                               date_labels = "%Y年%m月",
                               date_breaks = "2 month",
                               plot = TRUE
                               ){
  # 複数行の時系列データをDglmのポアソン分布想定でMCMCサンプリングする
  # NAが含まれている場合も受け入れるが、計算は測定値のインデックスのみで行う
  # stanファイルはreduce_sumで並列処理が前提である
  # df:説明変数のtibble
  # start:開始行　end:終了行
  # T_pred:予測数
  
  ncol_df <- ncol(df)
  max_row <- nrow(df)
  if(end > max_row){
    end <- max_row
  }
  N <- end - start + 1
  
  # 観測数ベクトル
  len_obs <- c()
  # 観測総数
  total_obs <- 0
  # 観測値のインデックス
  obs_no <- c()
  for(i in start:end){
    omit_NA <- length(na.omit(as.numeric(df[i,])))
    len_obs <- len_obs %>% append(omit_NA)
    total_obs <- total_obs + omit_NA
    obs_no <- obs_no %>% append(which(!is.na(df[i,])))
  }
  print("観測値の数", quote=F)
  print(len_obs, quote=F)
  print(paste0("Step = ",N), quote=F)
  # データの準備/na含む
  data_list <- list(
    T       = ncol(df),
    len_obs = len_obs,
    total_obs = total_obs,
    obs_no  = obs_no,
    N       = N,
    T_pred  = T_pred,
    grainsize = 250,
    y       = as.matrix(df %>% mutate_if(is.numeric, ~ replace_na(., 0)))[start:end,]
  )
  # コンパイル reduce_sumのオプション有り
  mod_ber_2 <- cmdstan_model("./stan/Dglm_poisson_rd_matrix_probs.stan", 
                             cpp_options = list(stan_threads = TRUE))
  
  # サンプリング
  fit <- mod_ber_2$sample(data = data_list, 
                          seed = 123,
                          chains = 4, 
                          parallel_chains = 4,
                          threads_per_chain = 2,
                          iter_warmup = iter_warmup,
                          iter_sampling = iter_sampling,
                          thin = thin,
                          max_treedepth = 15,
                          refresh = refresh)
  # y_probsの取得/ジャム数状態推定値+予測
  y_probs <- get_quantile_cmdfit(fit = fit,
                                 item_name = "y_probs",
                                 ncol_df = ncol_df,
                                 N = N,
                                 T_pred = T_pred,
                                 start_date = start_date,
                                 Machine_numbers = Machine_numbers
                                 )
  # r_probsの取得/ランダム効果
  r_probs <- get_quantile_cmdfit(fit = fit,
                                 item_name = "r_probs",
                                 ncol_df = ncol_df,
                                 N = N,
                                 T_pred = T_pred,
                                 start_date = start_date,
                                 Machine_numbers = Machine_numbers
  )
  
  # # y_predの取得
  # y_probs <- fit$draws("y_probs") %>% 
  #   as_draws_df
  # y_probs_tib <- tibble(y_probs)
  # 
  # N_row <- nrow(y_probs_tib)
  # N_col <- ncol(y_probs_tib)
  # # 95%信頼区間
  # y_probs_quan <- apply(y_probs_tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975))})
  # # .chain .iteration .drawの列を削除
  # y_probs_quan <- y_probs_quan[,1:(N_col-3)]
  # 
  # dates <- seq(from = as.POSIXct(start_date),
  #               by = "1 month",
  #               len = ncol_df+T_pred)
  # 
  # y_probs <- tibble(ID = c(1:(N_col-3))) %>% 
  #   mutate(
  #     `2.5%` = y_probs_quan[1,],
  #     `50%` = y_probs_quan[2,],
  #     `97.5%` = y_probs_quan[3,],
  #     Machine_numbers = rep(Machine_numbers[start:end],(ncol_df+T_pred)),
  #     date = rep(dates, each=N)
  #   )
  
  # 時系列グラフ
  p_y <- plot_time_series(df = y_probs,
                        y_label = y_label,
                        graph_title = graph_title,
                        date_labels = date_labels,
                        date_breaks = date_breaks,
                        plot = plot)

    return(list(fit, y_probs, r_probs, p_y))
}

plot_time_series <- function(df,
                             y_label = "件数累計",
                             graph_title = "時系列推移",
                             date_labels = "%Y年%m月",
                             date_breaks = "2 month",
                             plot = TRUE){
  # 時系列グラフを作成する
  # フォント定義
  par(family="Noto Sans")
  # 図示
  p <- ggplot(data = df, aes(x = date)) + 
    labs(title = graph_title) +
    theme(plot.title = element_text(size = 15,  #font size
                                    hjust = 0.1,#adjust
                                    face = "bold")) +
    ylab(y_label) +
    geom_line(aes(y = `50%`), size = 1.2) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.5, fill="lightblue") + 
    geom_point(aes(y = `50%`), alpha = 0.6, size = 1.5, color="red") +
    scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels) +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) +
    theme_bw() + 
    theme(text = element_text(size = 16)) +
    facet_wrap(~Machine_numbers, ncol = 1) +
    theme(strip.background = element_rect(fill="ivory"),
          strip.text = element_text(size = 12))
  if(plot == TRUE){
    plot(p)
  }
  return(p)
}

get_quantile_cmdfit <- function(fit = fit,
                                item_name = "y_probs",
                                ncol_df = ncol_df,
                                N = N,
                                T_pred = T_pred,
                                start_date = start_date,
                                Machine_numbers = Machine_numbers
                                ){
  # CmdStanrでのサンプリング結果fitから四分位範囲(0.025, 0.5, 0.975)を取得する
  # itemのサンプリング結果
  item <- fit$draws(item_name) %>% 
    as_draws_df
  item_tib <- tibble(item)
  
  N_row <- nrow(item_tib)
  N_col <- ncol(item_tib)
  # 95%信頼区間
  item_quan <- apply(item_tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975))})
  # .chain .iteration .drawの列を削除
  item_quan <- item_quan[,1:(N_col-3)]
  
  dates <- seq(from = as.POSIXct(start_date),
               by = "1 month",
               len = ncol_df+T_pred)
  
  ret_obj <- tibble(ID = c(1:(N_col-3))) %>% 
    mutate(
      `2.5%` = item_quan[1,],
      `50%` = item_quan[2,],
      `97.5%` = item_quan[3,],
      Machine_numbers = rep(Machine_numbers[start:end],(ncol_df + T_pred)),
      date = rep(dates, each=N)
    )
  
  return(ret_obj)
}
