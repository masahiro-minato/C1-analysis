
# b_colname <- c("切片", "M302A", "M302B", "M302C", "製造経過月数", "稼働月数", 
#                "リセット", "J014", "J016", "J017", "J018", "J066", "J067", "原稿表面枚数", 
#                "原稿裏面枚数")
# 
# reg_coef <- 
#   b_conv(fit_ZIP_3_CPU_target, 
#          b_colname,
#          max_col,
#          path_b1 = str_c("./ZIP-COOK_b1_確率", "_", lubridate::today(), ".csv"), 
#          path_b2 = str_c("./ZIP-COOK_b2_回数", "_", lubridate::today(), ".csv"), 
#          save = TRUE
#          )
# 
# reg_coef1 <- 
#   reg_coef[[1]]
# 
# reg_coef1[3001:3004,]
# 
# b_tib2 <- 
#   tib_col_division(b_tib, max_col)

tib_col_division <- function(df, vec_col){
  # df（データフレーム、tibble）の各列を指定値vec_col(ベクトル)で割る
  print("dfの各列を指定値vec_colで割る", quote=F)
  # print(names(df))
  i <- 0
  for (col_name in names(df)){
    # print(col_name)
    i <- i+1
    # print(i)
    # print(vec_col[i])
    df <- 
      df %>% 
      mutate(
        !!col_name := eval(parse(text = paste0("`",col_name,"`")))/vec_col[i]
      )
  }
  return(df)
}

b_conv <- function(fit, b_colname, max_col, N_row, path_b12, save, rescale){
  # CmdStanrでのZIPモデルfitからb（回帰係数）を取り出す
  # 四分位、平均値とサンプリングデータをtibbleとしてまとめる
  # b_colnameが説明変数名のベクトルとなる
  # max_colは正規化した時の最大値のベクトルで、スケールを元に戻すのに必要となる
  # N_rowはサンプリング数をランダムに削減する際の行数
  # save = TRUE にてCSVファイル保存　path_b12がファイルのパス
  # 戻り値は予測確率と予測回数のそれぞれのtibbleがリストで返る
  # 正規化前のスケールに戻すのにtib_col_divisionを内部で使用している
  
  b <- fit$draws("b") %>% 
    as_draws_df
  b_tib <- tibble(b)
  n_row <- nrow(b_tib)
  # 行数（サンプリング数）を削減
  # print("行数", quote=F)
  # print(n_row)
  rrows <- sort(sample(1:n_row, N_row))
  b_tib <- b_tib[rrows,]
  # 平均値
  b_mean <- fit$draws("b") %>% apply(3,mean)
  print(b_mean)
  # スケーリングを戻す
  if (rescale == TRUE){
    # 最大値ベクトルを２個づつの並びに変える
    max_col_2 <- rep(max_col, each = 2)
    # 正規化した最大値で割ることで正規化前の値に戻す
    b_tib <- tib_col_division(b_tib, max_col_2)
    b_mean <- b_mean/max_col_2[1:length(b_mean)]
  }
  
  # 95%信頼区間
  # b_quan <- apply(b_tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975))})
  # b_tib <- b_tib %>%
  #   rbind(b_quan)
  
  # 不要列削除と平均値の結合
  b_tib <- b_tib %>% 
    select(-.chain, -.iteration, -.draw ) %>% 
    rbind(b_mean) 
  
  # 確率に対する回帰係数
  b1_tib <- 
    b_tib %>% 
    select(seq(1,length(b_tib), by = 2))%>% 
    mutate(
      iter = as.character(c(rrows, "mean")),
      mean = if_else(iter == "mean", "mean", "data")
    ) %>% 
    select(iter, mean, everything())
  # 回数に対する回帰係数
  b2_tib <- 
    b_tib %>% 
    select(seq(2,length(b_tib), by = 2))%>% 
    mutate(
      iter = as.character(c(rrows, "mean")),
      mean = if_else(iter == "mean", "mean", "data")
    ) %>% 
    select(iter, mean, everything())
  # 列名変更
  colnames(b1_tib) <- c("iter", "mean", b_colname) # b1が確率
  colnames(b2_tib) <- c("iter", "mean", b_colname) # b2が回数
  
  # pivot
  # print("pivot", quote=F)
  b1_tib_pivot <-
    b1_tib %>%
    pivot_longer(
      cols = all_of(b_colname),
      names_to = "説明変数",
      values_to = "確率"
    )
  b2_tib_pivot <-
    b2_tib %>%
    pivot_longer(
      cols = all_of(b_colname),
      names_to = "説明変数",
      values_to = "回数"
    )
  b12_tib_pivot <-
    b1_tib_pivot %>%
    left_join(b2_tib_pivot)
  
  if (save == TRUE){
    # 回帰係数_csv保存
    print("回帰係数_csv保存", quote=F)
    write.csv(b12_tib_pivot, path_b12)
  }
  print("b_conv end")
  return(b12_tib_pivot)
}
