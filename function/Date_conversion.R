Date_conversion <- function(
  df, 
  col_list
){
  # データフレームの年月日列がdblで表記されている場合にdata型へ変換する
  # df:データフレーム名　文字列で表す（例："Metis_sheet3"）
  # col_list:対象となる列名の文字列ベクトル（例：c("納品年月日","製造年月","納入月")）
  # 戻り値 After_conversion:変換後のデータフレーム
  
  # get関数にて文字列で示されたデータフレームの内容を代入
  After_conversion <- get(df)
  
  for (col_name in col_list) {
    if(nchar(as.character(eval(parse(text = paste(df,"$",col_name,"[1]",sep=""))))) == 8){
      print(col_name)
      print("length=8")
      
      # Rでは!!を用いることで先に変数の中身を展開したうえで関数に渡すことができる。
      # 列名に!!を用いて展開した値を使用する場合は、=ではなく:=をセットで用いる必要がある。
      After_conversion <- 
        After_conversion %>% 
        mutate(
          !!col_name := 
            paste(str_sub(eval(parse(text = paste(df, "$", col_name, sep=""))), start=1, end=4),
                  str_sub(eval(parse(text = paste(df, "$", col_name, sep=""))), start=5, end=6),
                  str_sub(eval(parse(text = paste(df, "$", col_name, sep=""))), start=7, end=-1),
                  sep="-") %>% 
            as.POSIXct() %>% 
            as.Date(tz = "Asia/Tokyo")
        )
      
    }else if(nchar(as.character(eval(parse(text = paste(df,"$",col_name,"[1]",sep=""))))) == 6){
      print(col_name)
      print("length=6")
      
      After_conversion <- 
        After_conversion %>% 
        mutate(
          !!col_name := 
            paste(str_sub(eval(parse(text = paste(df, "$", col_name, sep=""))), start=1, end=4),
                  str_sub(eval(parse(text = paste(df, "$", col_name, sep=""))), start=5, end=-1),("01"),
                  sep="-") %>%
            as.POSIXct() %>%
            as.Date(tz = "Asia/Tokyo")
        )
    }else{
      print(col_name)
      print("length:NG")
    }
  }
  return(After_conversion)
}
