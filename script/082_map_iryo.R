################################################################################

# date: 2023-11-28

# author: nuajbec

################################################################################
# read packages
pacman::p_load(
  lubridate, # 日付操作 
  janitor, # 集計表 tabyl(),adorn_totals()
  openxlsx,
  readxl,
  writexl, # Excel出力 write_xlsx()
  ggrepel, # ggplotのテキスト位置自動調整 geom_text_repel() 
  ggridges, # リッジプロット 変数の分布をグループ別に出力 geom_density()
  gghighlight, # ggplotでにhighlight機能を追加 gghighlight()
  patchwork, # ggplotのグラフを結合 
  scales, # ggplotのメモリをパーセントにするのに使用 percent
  export, # パワポ出力 graph2ppt() table2ppt()
  RSQLite, # SQLite操作
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
)


pacman::p_load(sf,rmapshaper,tidyverse,tidylog)


#################################################################################

# データベースのpath
db <- 'output/db.sqlite'

# connect
con = dbConnect(SQLite(), db, synchronous="off")

# dbのtable一覧を確認
dbListTables(con) %>% print()

# dbのtableをすべて読み込みして、同名の変数に格納
dbListTables(con) %>% 
  map(~dbReadTable(con, .x)) %>% 
  set_names(dbListTables(con)) %>% 
  list2env(envir = .GlobalEnv)

###################################################################################

# 2次医療圏用のマスタを読み込み
mst_district <- tibble(mst_district) %>% 
  glimpse()

mst_district <- mst_district %>%
  select(citycd,iryocd,iryo)

################################################################################

# shiny用のdbを作成
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_shiny) %>% 
  map(~dbReadTable(con_shiny, .x)) %>% 
  set_names(dbListTables(con_shiny)) %>% 
  list2env(envir = .GlobalEnv)

################################################################################

# sample用データ

mst_latest_latlon <- tibble(mst_latest_latlon) %>% glimpse()
mst_latest <- tibble(mst_latest) %>% glimpse()

mst_latest_saitama_latlon <- mst_latest %>% 
  filter(pref=='埼玉県') %>% 
  left_join(mst_latest_latlon,by=c('mstno'))

mst_latest_saitama_latlon %>% 
  filter(is.na(lat)) %>% 
  glimpse()

mst_latest_saitama_latlon <- mst_latest_saitama_latlon %>% 
  filter(!is.na(lat))

df <- mst_latest_saitama_latlon %>% 
  select(hpname,lat,long,dpcbed) %>% 
  print()

df <- df %>% 
  filter(dpcbed>500) %>% 
  glimpse()

################################################################################

# mapの読み込み
japan = read_sf("https://okumuralab.org/~okumura/stat/data/japan2.geojson", stringsAsFactors=FALSE) %>%
  glimpse()

japan <- japan %>% 
  select(code=N03_007,pref=N03_001,gun_seirei=N03_003,city=N03_004)

map <- japan %>% 
  filter(pref=='埼玉県')

# 2次医療圏を結合
map <- map %>% 
  left_join(mst_district,by=c('code'='citycd')) %>%
  print()

map_iryo <- aggregate(map, list(map$iryocd), head, n=1)

map_iryo$Group.1 <- NULL

map_iryo <- map_iryo %>% 
  select(iryocd,iryo)


# 重心点を求める
map_iryo <- map_iryo %>%
  group_by(iryocd,iryo) %>%
  mutate(centroid = st_centroid(geometry),
         x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2]) %>%
  ungroup()

ggplot(map_iryo)+
  geom_sf()


# 書き出し
saveRDS(map_iryo,'output/map_iryo.rds')


###############################################################################

# graphのsample

base<- ggplot(map_iryo)+
  geom_sf(fill = NA)+ # 塗りなし
  coord_sf(datum=NA)+ # 軸なし
  geom_text(aes(x=x,y=y,label=iryo),size=3,alpha=0.3)+
  theme_void()

base

base + 
  geom_point(data=df,aes(x=long,y=lat,size=dpcbed),alpha=.3)+
  geom_text_repel(data=df,aes(x=long,y=lat,label=hpname),size=3)+
  scale_size_continuous(range = c(1, 20))

