
reg_coe <- function(fit, 
                    b_colname, 
                    sel_colname, 
                    max_col, 
                    digits=2, 
                    prob = FALSE, 
                    path_b,
                    save_b = FALSE,
                    rescale){
  # ZIPでのMCMCサンプリング結果から回帰係数95％信頼区間グラフを描画する
  # b_colnameは説明変数名のベクトル
  # max_colはb_colnameの各列の最大値のベクトル（の末尾に、".chain" ".iteration" ".draw"に対してc(1,1,1)を追加したベクトル）←不要かも
  # sel_colnameは描画する説明変数のベクトル
  
  b <- fit$draws("b") %>% 
    as_draws_df
  b_tib <- tibble(b)
  print(b_tib)
  # スケーリングを戻す
  if (rescale == TRUE){
    # 最大値ベクトルを２個づつの並びに変える
    max_col_2 <- rep(max_col, each = 2)
    # 正規化した最大値で割ることで正規化前の値に戻す
    b_tib <- tib_col_division(b_tib, max_col_2)
  }
  N_row <- nrow(b_tib)
  N_col <- ncol(b_tib)
  
  # print("チェックーーーーーーーーーーー")
  print(b_tib)
  # 95%信頼区間
  b_quan <- apply(b_tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975))})
  # 回帰係数グラフ
  b_quan <- b_quan[,1:(N_col-3)]
  
  b <- tibble(ID = c(1:(N_col-3))) %>% 
    mutate(
      `2.5%` = b_quan[1,],
      `50%` = b_quan[2,],
      `97.5%` = b_quan[3,],
      type = rep(c("確率","回数"),(N_col-3)/2),
      Parameter = rep(b_colname, each=2),
      label =  round(`50%`, digits=digits)
    )
  print(b_colname)
  print(b$Parameter)
  b$Parameter <- 
    factor(b$Parameter, levels = b_colname)
  print(b, n=50)
  print(b$Parameter %in% sel_colname)
  b <-
    b %>%
    arrange(Parameter) %>%
    # dplyr::filter(Parameter != "切片")　# 切片項を削除
    dplyr::filter(Parameter %in% sel_colname)
  print(b)
  p <- reg_coe_graph(b, prob=prob)
  if (save_b == TRUE){
    # csv保存
    print("csv保存", quote=F)
    write.csv(b, path_b)
  }
  
  return(p)
}

reg_coe_graph <- function(b, prob=TRUE){
  # 回帰係数95％信頼区間を示すグラフ描画
  # b:入力データ（tibble）
  # prob=TRUEで確率の回帰係数の降順でソートし、FALSEの場合は回数の回帰係数の降順でソートする
  par(family="Noto Sans")
  if(prob == TRUE){
    sel <- "type"
  }else{
    sel <- "fct_rev(type)"
  }
  p <- ggplot() +
    geom_hline(yintercept=0, linetype="dashed", linewidth=0.5) +
    geom_pointrange(data=b, mapping=aes(x=forcats::fct_reorder2(Parameter, eval(parse(text = sel)), `50%`, .desc = F), 
                                        y=`50%`, ymin=`2.5%`, ymax=`97.5%`, col = type), linewidth=1.0, shape=1) +
    geom_text(data=b,aes(x=Parameter, y=`50%`, label=label), size = 4, vjust = -1) +
    # scale_y_continuous(limits = c(-100, 100)) +
    coord_flip() +
    labs(x='説明変数', y='回帰係数', 
         title="回帰係数95％信頼区間") +
    theme_bw() +
    theme(legend.position = c(0.98,0.98), legend.justification = c(1,1)) +
    theme(title = element_text(size = 12), text = element_text(size = 12)) +
    facet_wrap(~type, ncol = 2) +
    theme(strip.background = element_rect(fill="ivory"),
          strip.text = element_text(size = 12), legend.position = "none") 
  plot(p)
  return(p)
}
