# ql_conv

# ql_restaurant <- ql_conv(fit = fit_ZIP_CPU,
#                          col_list = c(1:200),
#                          names = "ID",
#                          N_row = 1000,
#                          N_col = 200,
#                          path = str_c("./ZIP-飲食店来店回数_ql", "_", lubridate::today(), ".csv"),
#                          save = TRUE
# )


ql_conv <- function(fit, col_list, names, N_row, N_col, path, save=FALSE){
  # CmdStanrでのZIPモデルfitからq,lambdaを取り出す
  # col_listが列名となる
  # N_row:サンプリングデータから摘出する乱数の数/全てを出力すると最終の行数が膨大になるため 
  # N_col:col_listから摘出する数/同上
  # 最終行に列の平均値が付加される
  # save = TRUE にてCSVファイル保存
  # 戻り値は縦型データへ変換されている
  
  # q
  # print("q", quote=F)
  q <- fit$draws("q") %>% as_draws_df
  N_iter <- nrow(q)
  # 乱数
  if (N_row > N_iter){
    N_row  <-  N_iter
  }
  if (N_col > length(col_list)){
    N_col <- length(col_list)
  }
  rrows <- sort(sample(1:N_iter, N_row))
  col_id <- sort(sample(1:length(col_list), N_col))
  print(length(unique(col_id)))
  # q
  q_tib <- 
    tibble(q) %>% 
    select(-.chain, -.iteration, -.draw )
  q_tib <- q_tib[rrows,]
  colnames(q_tib) <- col_list
  # 平均値
  # print("mean", quote=F)
  q_mean <- fit$draws("q") %>% apply(3,mean)
  l_mean <- fit$draws("lambda") %>% apply(3,mean)
  l_mean_exp <- exp(l_mean)
  
  # col_id <- which(0.8 > q_mean & q_mean > 0.4 & l_mean_exp > 0.4)
  
  # print("q_tib", quote=F)
  q_tib <- 
    q_tib %>% 
    rbind(q_mean) %>% 
    mutate(
      iter = as.character(c(rrows, "mean")),
      mean = if_else(iter == "mean", "mean", "data")
    ) %>% 
    select(iter, mean, everything())
  
  q_tib <- 
    q_tib %>% 
    select(c(1,2,as.vector(col_id)+2))
  
  # lambda
  # print("lambda", quote=F)
  l <- fit$draws("lambda") %>% as_draws_df
  l_tib <- 
    tibble(l) %>% 
    select(-.chain, -.iteration, -.draw )
  l_tib <- l_tib[rrows,]
  colnames(l_tib) <- col_list
  # print("lambda_tib", quote=F)
  l_tib <- 
    exp(l_tib) %>% 
    rbind(l_mean_exp) %>% 
    mutate(
      iter = as.character(c(rrows,"mean")),
      mean = if_else(iter == "mean", "mean", "data")
    ) %>% 
    select(iter, mean, everything())
  
  l_tib <- 
    l_tib %>% 
    select(c(1,2,as.vector(col_id)+2))
  # pivot
  # print("q-pivot", quote=F)
  q_tib_pivot <-
    q_tib %>%
    pivot_longer(
      cols = str_c(col_list[col_id]),
      names_to = names,
      values_to = "q"
    )
  # print("l-pivot", quote=F)
  l_tib_pivot <-
    l_tib %>%
    pivot_longer(
      cols = str_c(col_list[col_id]),
      names_to = names,
      values_to = "lambda"
    )
  # print("ql-pivot", quote=F)
  pl_tib_pivot <-
    q_tib_pivot %>%
    dplyr::left_join(l_tib_pivot)
  
  if (save == TRUE){
    # csv保存
    print("csv保存", quote=F)
    write.csv(pl_tib_pivot, path)
  }
  
  return(pl_tib_pivot)
}
