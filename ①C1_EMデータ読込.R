###エクセルファイルの読み込み####

C1_保守履歴 <- 
  read_excel("./excel/Metis-C1_保守履歴.機器(201306-201605).xlsx",sheet = 2)
C1_機器 <- 
  read_excel("./excel/Metis-C1_保守履歴.機器(201306-201605).xlsx",sheet = 3)
C1_周辺機データ <- 
  read_excel("./excel/Metis-C1周辺機データ.xlsx",sheet = 2)

colnames(C1_保守履歴)

C1_maintenance <- 
  C1_保守履歴 %>% 
  mutate(
    機種略機番 = paste(機種略号, 機番, sep="")
  ) %>% 
  select(機種略機番, everything()) %>% 
  select(機種略号,機種略機番,製造年月,納入日,経過月,実施日,
         コール,EM,現象,現象SC名称,詳細場所,再現性,原因,処置,処置場所系,作業時間) %>% 
  dplyr::filter(EM == 1)
C1_Peripheral <- 
  C1_周辺機データ %>% 
  mutate_at(.vars = "機番", .funs = ~ str_sub(., start = -6, end = -1)) %>% 
  mutate(
    機種略機番 = paste(機種略号, 機番, sep="")
  ) %>% 
  select(機種略機番, 周辺機名)

C1_maintenance.P <- 
  C1_Peripheral %>% 
  left_join(C1_maintenance) %>% 
  subset(!(is.na(機種略号))) %>%
  distinct_all()

colnames(C1_機器)
C1_machine <- 
  C1_機器 %>% 
  select(機種略号,機種機番,納入月,納入年月日,製造年月)

C1_machine.P <- 
  C1_Peripheral %>% 
    left_join(C1_machine, by=c("機種略機番"="機種機番")) %>% 
    subset(!(is.na(機種略号))) %>%
    distinct_all()

names(C1_maintenance.P)
names(C1_machine.P)
# 日付データをdate型へ変換
C1_maintenance.P.D <- 
  Date_conversion("C1_maintenance.P", c("製造年月", "納入日", "実施日"))
C1_machine.P.D <- 
  Date_conversion("C1_machine.P", c("納入月", "納入年月日", "製造年月"))

# 列名
colnames(C1_maintenance.P.D)
C1_maintenance.P.D.re <- 
  rename(C1_maintenance.P.D, 
         Model_abbreviation = 機種略号,
         Machine_numbers = 機種略機番,
         # Model_code = 機種ｺｰﾄﾞ,
         # Visit_classification = 訪問区分,
         # Year_and_month = 年月度,
         Manufacturing_date = 製造年月,
         Due_date = 納入日,
         Working_month = 経過月,
         Maintenance_date = 実施日,
         CE_Working_hours = 作業時間,
         Call = コール,
         Phenomenon = 現象,
         SC_name = 現象SC名称,
         Treatment_location = 処置場所系,
         Peripheral_name = 周辺機名,
         cause = 原因,
         Treatment = 処置,
         Reproducibility = 再現性,
         Detailed.location = 詳細場所
  )

C1_maintenance.P.D.re %>% distinct_all() #744175

# 現象
C1_maintenance.P.D.re %>% 
  distinct(Phenomenon) %>% 
  print(n=30)
# 現象SC名称
C1_maintenance.P.D.re %>% 
  distinct(SC_name) %>% 
  print(n=30)
# 処置場所
C1_maintenance.P.D.re %>% 
  distinct(Treatment_location) %>% 
  print(n=30)

# 製造経過月数・稼働月数
C1_maintenance.P.D.re.EW <- 
  C1_maintenance.P.D.re %>% 
  rowwise() %>%
  mutate(
    Elapsed_mf_months = 
      (length(seq(as.Date("2013/03/01"), as.Date(Manufacturing_date), "month"))-1),
    Working_months = 
      (length(seq(as.Date(Due_date), as.Date(Maintenance_date), "month"))-1)
  ) %>% 
  ungroup()
# 稼働月数/2016/06/01時点
C1_machine.P.D.W <- 
  C1_machine.P.D %>% 
  rowwise() %>%
  mutate(
    Working_months = 
      (length(seq(as.Date(納入年月日), as.Date("2016/06/01"), "month"))-1)
  ) %>% 
  ungroup()

# 日次EM数の抽出
C1.EM <- 
  C1_maintenance.P.D.re.EW %>% 
  dplyr::filter(is.na(Peripheral_name)) %>%
  group_by(Maintenance_date) %>% 
  summarise(
    C1.EM = n()
  )

C1_machine.P.D.W %>% 
  distinct_all()

C1_maintenance.P.D.re.EW %>% 
  distinct_all()

C1_machine.P.D.W <- 
  C1_machine %>% 
  rename(納品年月日 = 納入年月日)

# ファイル保存
write_tsv(C1_maintenance.P.D.re.EW, "./tsv_data/C1_maintenance.tsv")
write_tsv(C1_machine.P.D.W, "./tsv_data/C1_machine.tsv")
