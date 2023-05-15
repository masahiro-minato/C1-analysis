# d <- read.csv('../RStanBook-master/chap11/input/data-ZIP.txt')
# # d$Age <- d$Age/10
# 
# # 列名変更
# colnames(d) <- c("性別_女性","酒_飲む","年齢","来店回数")
# # 要素を数値⇒factor
# d$性別 <- ifelse(d$性別==0, "男性", "女性")
# d$酒 <- ifelse(d$酒==0, "飲まない", "飲む")
# d$性別 <- as.factor(d$性別)
# d$性別 <- factor(d$性別, levels=c("男性", "女性"))
# d$酒 <- as.factor(d$酒)
# d$酒 <-factor(d$酒, levels=c("飲まない", "飲む"))

ZIP_calc <- function(data = d, # 入力データ　tibble形式で応答変数を含める
                     value = "来店回数", # 応答変数
                     path_data_save = str_c("./ZIP-data", "_", lubridate::today(), ".csv"),
                     save_data = FALSE,
                     col_list = c(1:200), # pivot_longerでの列名
                     names = "ID", # pivot_longerにおけるnames
                     device = "CPU", # "CPU" or "GPU" or "CPU.rd"
                     opencl_ids = c(0, 0), # GPUのPlatform番号とDevice番号
                     path_ZIP = "./stan/ZIP_3_CPU.stan", # Stanファイルのアドレス
                     grainsize = 1, # 並列処理（reduce_sum）における粒度 1で最適化だが怪しい
                     chains = 4, 
                     parallel_chains = 4,
                     iter_warmup = 1000,
                     iter_sampling = 500,
                     thin = 1,
                     max_treedepth = 15,
                     refresh = 100,
                     path_fit_remove = "D:/R_working/CmdStan_files/", # サンプリング結果ファイルの移動先
                     path_fit_save = "D:/R_working/Metis-MF3/ZIP_CmdStan_＊.rds",
                     save_fit = FALSE,
                     rhat = FALSE,
                     N_col = 200, # qlの取得列数
                     N_row = 300, # ql,b12の取得行数
                     path_ql = str_c("./ZIP-ql", "_", lubridate::today(), ".csv"),
                     save_ql = FALSE,
                     path_b12 = str_c("./ZIP-b12_回数", "_", lubridate::today(), ".csv"),
                     path_b = str_c("./ZIP-b", "_", lubridate::today(), ".csv"),
                     save_b = FALSE,
                     rescale = FALSE, # 回帰係数の正規化を元に戻す/説明変数の元のスケールに戻す
                     sel_colname = c("酒_飲む", "性別_女性"),
                     digits = 2, # グラフ表示の中央値の小数点以下桁数
                     prob = TRUE, # TRUEにて確率の回帰係数の降順でソートする/FALSEは回数　
                     path_graph = "./PDF/回帰係数_*.pdf",
                     save_graph = FALSE,
                     dpi = 100,
                     width = 6, 
                     height = 4){
  # 戻り値：list(fit_ZIP, ql, reg_coefficient, p)
  print(Sys.time(), quote=F)
  set.seed(123)
  # 入力データを保存
  data_origin <- data
  if (save_data == TRUE){
    # csv保存
    data_save <- 
      data_origin %>% 
      mutate(
        !!names := col_list
      ) %>% 
      select(all_of(names), everything())
    print("data_csv保存", quote=F)
    write.csv(data_save, path_data_save)
  }
  # 正規化のために最大値を算出
  max_data <-
    data %>%
    summarize(across(where(is.numeric), \(x) max(x, na.rm = TRUE))) %>% # R4.2.3以降
    # summarize(across(where(is.numeric), max, na.rm = TRUE)) %>%
    select(-all_of(value))
  print(max_data)
  # 正規化
  print("正規化", quote=F)
  for(col in names(max_data)){
    data <- data %>%
      mutate(
        !!col :=eval(parse(text = col))/eval(parse(text = paste0("max_data$",col)))
      )
  }
  n_col <- ncol(data)
  print(data)
  # 応答変数（value）のインデックスを取得
  print("応答変数のインデックスを取得", quote=F)
  N_value <- which(colnames(data) == value)
  print(N_value)
  colnames(data) <- str_c(1:n_col)
  # 入力データの列名をベクトルとして取得
  b_colname <- data_origin %>% 
    names() %>% 
    dput()
  # 回帰係数の列名に対し応答変数を削除し、"切片"を追加する
  b_colname <- b_colname[-N_value] %>% 
    append(c("切片"), after = 0)
  
  print(b_colname)
  
  # マトリクスへ変換
  print("マトリクスへ変換", quote=F)
  X <- cbind(1, data[,-N_value])
  # print(X)
  # print(data[[N_value]])
  data_list <- list(N=nrow(data), 
                    D=ncol(X), 
                    Y=data[[N_value]],
                    X=X)
  if (device == "CPU"){
    # コンパイル
    mod_ZIP <- cmdstan_model(path_ZIP)
    # サンプリング
    fit_ZIP <- mod_ZIP$sample(data = data_list, 
                              seed = 123,
                              chains = chains, 
                              parallel_chains = parallel_chains,
                              iter_warmup = iter_warmup,
                              iter_sampling = iter_sampling,
                              thin = thin,
                              max_treedepth = max_treedepth,
                              refresh = refresh)
  }else if (device == "GPU"){
    # コンパイル
    mod_ZIP <- cmdstan_model(path_ZIP,
                             cpp_options = list(stan_opencl = TRUE))
    # サンプリング
    fit_ZIP <- mod_ZIP$sample(data = data_list, 
                              seed = 123,
                              chains = chains, 
                              parallel_chains = parallel_chains,
                              opencl_ids = opencl_ids,
                              iter_warmup = iter_warmup,
                              iter_sampling = iter_sampling,
                              thin = thin,
                              max_treedepth = max_treedepth,
                              refresh = refresh)
  }else if (device == "CPU.rd"){
    data_list <- list(N=nrow(data), 
                      D=ncol(X), 
                      Y=data[[N_value]],
                      grainsize=grainsize,
                      X=X)
    # コンパイル
    mod_ZIP <- cmdstan_model(path_ZIP,
                             cpp_options = list(stan_threads = TRUE))
    # サンプリング
    fit_ZIP <- mod_ZIP$sample(data = data_list, 
                              seed = 123,
                              chains = chains, 
                              parallel_chains = parallel_chains,
                              threads_per_chain = 2,
                              iter_warmup = iter_warmup,
                              iter_sampling = iter_sampling,
                              thin = thin,
                              max_treedepth = max_treedepth,
                              refresh = refresh)
  }
  
  if(save_fit == TRUE){
    # 移動先フォルダが存在しない場合は作成する
    if(!dir.exists(path_fit_remove)){
      dir.create(path_fit_remove)
    }
    # ファイル移動
    print("ファイル移動", quote=F)
    fit_ZIP$save_output_files(path_fit_remove)
    # モデル保存
    print("モデル保存", quote=F)
    saveRDS(fit_ZIP, path_fit_save)
  }
  # rhatのヒストグラム
  if(rhat == TRUE){
    print("rhatのヒストグラム", quote=F)
    print(Sys.time(), quote=F)
    print(
      system.time(
        fit_ZIP %>% 
          bayesplot::rhat() %>% 
          hist
      )
    )
  }
  
  # 予測確率と回数
  print("予測確率と回数/ql_conv", quote=F)
  # print(N_col)
  ql <- ql_conv(fit = fit_ZIP,
                col_list = col_list,
                names = names,
                N_col = N_col,
                N_row = N_row,
                path = path_ql,
                save = save_ql
                )
  
  # 応答変数を削除
  data_novalue <- data_origin[,-N_value]
  
  # 最大値のベクトル作成
  print("MAXベクトル", quote=F)
  # 数値データ以外は、MAX値＝1を設定する
  # max_col <- rep(1, length(data_novalue[1,]))
  max_col <- rep(1, (length(data_novalue[1,])+3))
  for(col in names(max_data)){
    max_col[which(names(max_data) == col)] <- max_data[1,col]
  }
  max_col <- 
    max_col %>% 
    append(1, after = 0)
  
  # 下記方法だと、文字列データに対してエラーとなる
  # max_col <-
  #   sapply(1:length(data_novalue[1,]), function(i) max(data_novalue[,i])) %>%
  #   append(1, after = 0) %>%   # 切片項の列に対して1を追加
  #   append(c(1,1,1)) # ".chain" ".iteration" ".draw"に対して1を追加 ←不要かも
  #   # append(c(1,1,1), after = 15)　# 謎の15
  print(max_col)
  print(sel_colname)
  # 回帰係数 b1:確率 b2:回数
  print("回帰係数 b1:確率 b2:回数/b_conv", quote=F)
  reg_coefficient <- b_conv(fit = fit_ZIP,
                     b_colname = b_colname,
                     max_col = max_col,
                     N_row = N_row,
                     path_b12 = path_b12,
                     save = save_b,
                     rescale = rescale
                     )
  # 回帰係数グラフ
  p <- reg_coe(fit_ZIP, 
               b_colname, 
               sel_colname, 
               max_col, 
               digits=digits, 
               prob = prob,
               path_b,
               save_b = save_b,
               rescale = rescale
               )
  
  if(save_graph == TRUE){
    # 回帰係数グラフ保存
    print("回帰係数グラフ保存", quote=F)
    ggsave(path_graph, plot=p, device=cairo_pdf, dpi=dpi, width=width, height=height)
  }
  print(Sys.time(), quote=F)
  return(list(fit_ZIP, ql, reg_coefficient, p))
}
