Create_Metis_MF3_SCname <- function(
  df = Metis_MF3_2211,
  T_location = "ADF部", 
  P_name = "COOK-C",
  Choices = "Working_month",
  num = 10
){
  # ファイル読込
  # Metis_MF3 <- read_tsv("./VOLGA/tsv_data/Metis_MF3_2211.tsv")
  Metis_MF3 <- df
  # EM数,SCの抽出
  Metis_MF3_EMnum <- 
    Metis_MF3 %>% 
    dplyr::filter(Treatment_location %in% T_location) %>% 
    dplyr::filter(Peripheral_name == P_name) %>%
    group_by(eval(parse(text = Choices))) %>%
    summarise(
      Peripheral_name_EM = n()
    ) %>% 
    ungroup()
  
  Metis_MF3_sc <- 
    Metis_MF3 %>% 
    dplyr::filter(Treatment_location %in% T_location) %>% 
    dplyr::filter(Peripheral_name == P_name) %>%
    group_by(eval(parse(text = Choices)), SC_name) %>%
    summarise(
      Peripheral_name_SC = n()
    ) %>% 
    ungroup()
  
  # SCの発生割合の算出/列追加
  Metis_MF3_sc <-
    Metis_MF3_sc %>%
    left_join(Metis_MF3_EMnum, by='eval(parse(text = Choices))') %>%
    mutate(
      SC_rate = Peripheral_name_SC / Peripheral_name_EM
    )

  # 稼働月当たりnum件以上に絞る/未満はその他へまとめる
  Metis_MF3_sc_num <-
    Metis_MF3_sc %>%
    mutate(
      SC_name_num = if_else(Peripheral_name_SC < num, "その他", SC_name)
    )

  Metis_MF3_sc_num <-
    Metis_MF3_sc_num %>%
    group_by(`eval(parse(text = Choices))`,SC_name_num) %>%
    summarise(
      Peripheral_name_SC_sum = sum(Peripheral_name_SC),
      Peripheral_name_EM = Peripheral_name_EM
    ) %>%
    ungroup()

  # 重複行削除
  Metis_MF3_sc_num <-
    Metis_MF3_sc_num %>%
    distinct(`eval(parse(text = Choices))`,SC_name_num, .keep_all=T)

  Metis_MF3_sc_num <-
    Metis_MF3_sc_num %>%
    rename(tmp = `eval(parse(text = Choices))`)
  
  colnames(Metis_MF3_sc_num) <- 
  colnames(Metis_MF3_sc_num) %>%
    str_replace("tmp", Choices)
  
  # factor型へ変換
  Metis_MF3_sc_num <-
    Metis_MF3_sc_num %>%
    mutate(
      SC_name_num = factor(SC_name_num))
  
  return(Metis_MF3_sc_num)
}
