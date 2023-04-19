#### 現象別でのEM件数時系列グラフ

# ファイル読込
C1_maintenance <- read_tsv("./tsv_data/C1_maintenance.tsv")　# 保守データ
C1_machine <- read_tsv("./tsv_data/C1_machine.tsv")          # 機器データ
# 現象項目
C1_maintenance %>% distinct(Phenomenon) %>% dput()
# 周辺機名称
C1_maintenance$Peripheral_name
# 列名
names(C1_maintenance)

# 現象項目の中で上位10項目を抽出
Phe10.C1 <- 
  head(levels(fct_infreq(C1_maintenance$Phenomenon)), n=10)
# 現象別EM件数
EM.num_by.phenom.C1 <- 
  C1_maintenance %>%
  dplyr::filter(is.na(Peripheral_name)) %>%
  mutate(
    Phenomenon10 = if_else(Phenomenon %in% Phe10.C1, Phenomenon, "その他"),
    Phenomenon10 = factor(Phenomenon10,levels=c(Phe10.C1,"その他"))
  ) %>% 
  group_by(Phenomenon10) %>% 
  summarise(
    num = n()
  ) 
# 現象別EM件数ベクトルを名前付きにする
EM.num.C1 <- EM.num_by.phenom.C1$num
names(EM.num.C1) <- c(Phe10.C1, "その他")
# 確認
EM.num.C1["その他"]
# factorのレベル設定用ベクトル
levels.display.C1 <- 
  str_c(c(Phe10.C1, "その他")," (",EM.num.C1,"件）")

# 保守データから日付ごとのEM件数を取得/現象項目の11位以下はその他へまとめる
EM.count.C1 <- 
  C1_maintenance %>% 
  dplyr::filter(is.na(Peripheral_name)) %>%
  mutate(
    Phenomenon10 = if_else(Phenomenon %in% Phe10.C1, Phenomenon, "その他"),
    Phenomenon10 = factor(Phenomenon10,levels=c(Phe10.C1,"その他"))
    ) %>% 
  group_by(Maintenance_date, Phenomenon10) %>%
  summarise(
    EM.count = n()
  ) %>% 
  arrange(-EM.count) %>%
  ungroup()

# 市場機台数
MIF.count.C1 <- 
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
X.date.C1 <- seq(as.POSIXct("2013-03-01"), as.POSIXct("2016-05-31"), by = "day")
MIF.date.C1 <- tibble(X.date = X.date.C1)
C1.MIF_by.date <- 
  MIF.date.C1 %>% 
  full_join(MIF.count.C1, by=c("X.date" = "納品年月日")) %>% 
  full_join(EM.count.C1, by=c("X.date" = "Maintenance_date")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    MIF.cumsum = cumsum(N.date)
  ) %>% 
  drop_na()
# EM現象上位10項目の確認
C1.MIF_by.date %>% distinct(Phenomenon10) %>% dput()

# EM現象項目に発生件数を結合し、表示用列としてdisplayを加える
C1.MIF_by.date <- 
  C1.MIF_by.date %>% 
  mutate(
    display = factor(str_c(Phenomenon10," (",EM.num.C1[Phenomenon10],"件）"), levels=levels.display.C1)
  )

C1.MIF_by.date$display

# 図示
par(family="Noto Sans")
p <- ggplot(data = C1.MIF_by.date, aes(x = X.date, fill = display)) + 
            theme_bw() +
            labs(title = "Metis-C1 現象別EM時系列件数 Top10") +
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
plot(p)          

# グラフ保存
ggsave(str_c("./PDF/Metis-C1-EM現象別時系列.pdf"), 
       plot = p, device = cairo_pdf, dpi=300, width=40, height=30)
