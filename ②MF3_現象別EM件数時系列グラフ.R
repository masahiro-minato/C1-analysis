#### 現象別でのEM件数時系列グラフ

# ファイル読込
MF3_maintenance <- read_tsv("./tsv_data/Metis_MF3_2211.tsv")　    # 保守データ
MF3_machine <- read_tsv("./tsv_data/Metis_MIF_2211.tsv")          # 機器データ
# 現象項目
MF3_maintenance %>% distinct(Phenomenon) %>% dput()
# 周辺機名称
MF3_maintenance$Peripheral_name
# 列名
names(MF3_maintenance)

# 現象項目の中で上位10項目を抽出
Phe10.MF3 <- 
  head(levels(fct_infreq(MF3_maintenance$Phenomenon)), n=10)
# 現象別EM件数
EM.num_by.phenom.MF3 <- 
MF3_maintenance %>%
  dplyr::filter(is.na(Peripheral_name)) %>%
  mutate(
    Phenomenon10 = if_else(Phenomenon %in% Phe10.MF3, Phenomenon, "その他"),
    Phenomenon10 = factor(Phenomenon10,levels=c(Phe10.MF3,"その他"))
  ) %>% 
  group_by(Phenomenon10) %>% 
  summarise(
    num = n()
  ) 
# 現象別EM件数ベクトルを名前付きにする
EM.num.MF3 <- EM.num_by.phenom.MF3$num
names(EM.num.MF3) <- c(Phe10.MF3, "その他")
# 確認
EM.num.MF3["その他"]
# factorのレベル設定用ベクトル
levels.display.MF3 <- 
  str_c(c(Phe10.MF3, "その他")," (",EM.num.MF3,"件）")

# 保守データから日付ごとのEM件数を取得/現象項目の11位以下はその他へまとめる
EM.count.MF3 <- 
  MF3_maintenance %>% 
  dplyr::filter(is.na(Peripheral_name)) %>%
  mutate(
    Phenomenon10 = if_else(Phenomenon %in% Phe10.MF3, Phenomenon, "その他"),
    Phenomenon10 = factor(Phenomenon10,levels=c(Phe10.MF3,"その他"))
    ) %>% 
  group_by(Maintenance_date, Phenomenon10) %>%
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
  drop_na()
# EM現象上位10項目の確認
MF3.MIF_by.date %>% distinct(Phenomenon10) %>% dput()

# EM現象項目に発生件数を結合し、表示用列としてdisplayを加える
MF3.MIF_by.date <- 
  MF3.MIF_by.date %>% 
    mutate(
      display = factor(str_c(Phenomenon10," (",EM.num.MF3[Phenomenon10],"件）"), levels=levels.display.MF3)
    )

MF3.MIF_by.date$display

# 図示
par(family="Noto Sans")
p.MF3 <- ggplot(data = MF3.MIF_by.date, aes(x = X.date, fill = display)) + 
            theme_bw() +
            labs(title = "Metis-MF3 現象別EM時系列件数 Top10") +
            ylab("EM件数") +
            xlab("年月日") +
            geom_bar(mapping = aes(y = EM.count), stat = "identity") +
            facet_wrap(~ display, ncol = 1) +
            scale_x_datetime(date_breaks = "2 month", date_labels = "%Y/%m") +
            theme(plot.title = element_text(size = 30, hjust = 0.01),
                　axis.title.x = element_text(size = 20),
                  axis.title.y = element_text(size = 20),
                  axis.text.x = element_text(angle = 30, hjust = 1, size = 18),
                  axis.text.y = element_text(size = 20)) +
            guides(fill = "none") +
            theme(strip.background = element_blank(), strip.text = element_text(size = 26))
plot(p.MF3)          

# グラフ保存
ggsave(str_c("./PDF/Metis-MF3-EM現象別時系列.pdf"), 
       plot = p.MF3, device = cairo_pdf, dpi=300, width=40, height=30)
