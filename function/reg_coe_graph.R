
reg_coe_graph <- function(b, prob=TRUE){
  # 回帰係数95％信頼区間を示すグラフ描画
  # b:入力データ（tibble）
  # prob=TRUEで確率の回帰係数の降順でソートし、FALSEの場合は回数の回帰係数の降順でソートする
  # windowsFonts(Japan1GothicBBB = windowsFont("Japan1GothicBBB"))
  par(family="Noto Sans")
  if(prob == TRUE){
    sel <- "type"
  }else{
    sel <- "fct_rev(type)"
  }
  p <- ggplot() +
    geom_pointrange(data=b, mapping=aes(x=forcats::fct_reorder2(Parameter, eval(parse(text = sel)), `50%`, .desc = F), 
                                        y=`50%`, ymin=`2.5%`, ymax=`97.5%`, col = type), size=1.0, shape=1) +
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

# reg_coe_graph(b, prob=FALSE)
