################################################################################

# 作成日:20231128

# 作成者:nujabec

################################################################################
# パッケージの読み込み
pacman::p_load(
  lubridate, # 日付操作 
  janitor, # 集計表 tabyl(),adorn_totals()
  openxlsx,
  readxl,
  writexl, # Excel出力 write_xlsx()
  flextable, # 見やすい表作成
  gtsummary, # 見やすい統計表作成
  esquisse, # マウスでグラフ作成 esquisser()
  ggrepel, # ggplotのテキスト位置自動調整 geom_text_repel() 
  ggridges, # リッジプロット 変数の分布をグループ別に出力 geom_density()
  gghighlight, # ggplotでにhighlight機能を追加 gghighlight()
  patchwork, # ggplotのグラフを結合 
  scales, # ggplotのメモリをパーセントにするのに使用 percent
  export, # パワポ出力 graph2ppt() table2ppt()
  officer, # 汎用オフィス系出力
  summarytools, # 探索的データ分析(EDA) dfSummary()
  DataExplorer, # 探索的データ分析(EDA)　create_report()
  RSQLite,
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
)

# 自作関数の読み込み
source('/home/rstudio/srv/function/index.R')

# function以外の変数を削除
clean_vars()

# データベースのpath
db_dpc <- 'output/dpcmst.sqlite'

# connect
con_dpc = dbConnect(SQLite(), db_dpc, synchronous="off")

################################################################################

# 各年度のdpcmstを読み込んで、縦結合する

# dpcmst内のすべてのrdsファイル名を取得
files <- list.files(path = 'dpcmst',
                         pattern = '.rds',
                         full.names = TRUE,
                         recursive = TRUE) %>% 
  print()

outputdir <- 'dpcmst_integrated/'

# dpcmst内のすべてのrdsファイル名を取得
targets <- list.files(path = 'dpcmst/2022',
                    pattern = '.rds',
                    recursive = TRUE) %>% 
  print()

for(target in targets){
  
  targetfile <- files %>% 
    str_subset(target) %>% 
    print()
  
  # データを読み込んで縦に結合
  df <- targetfile %>% 
    map_dfr(readRDS) %>% 
    print()
  
  # outputdirにrds保存
  # df %>% saveRDS(str_c(outputdir,target))
  
  dbname <- str_remove(target,'.rds')
  
  # dbに書き出し
  dbWriteTable(con_dpc,dbname,df,overwrite = TRUE,row.names = FALSE)
}

# dbのtable一覧を確認
dbListTables(con_dpc) %>% print()
