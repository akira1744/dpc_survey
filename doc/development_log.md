# 2023/11/28

## DataSource

### DPC導入の影響評価に関する調査：集計結果

- top

https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000049343.html

- 令和3年度(2021年度)

https://www.mhlw.go.jp/stf/shingi2/0000196043_00006.html

- 令和2年度(2020年度)

https://www.mhlw.go.jp/stf/shingi2/0000196043_00005.html

- 令和元年度(2019年度)

https://www.mhlw.go.jp/stf/shingi2/0000196043_00004.html

- 平成30年度(2018年度)

https://www.mhlw.go.jp/stf/shingi2/0000196043_00003.html

- 平成29年度(2017年度)

https://www.mhlw.go.jp/stf/shingi2/0000196043_00002.html


### usage data

- 施設概要表 = facility_summary_table.xlsx
- 参考資料1（３）在院日数の状況（Excel：5,045KB） - agg_hp_day 
- 参考資料1 (12)施設別MDC比率 = mdc_ratio_by_facility.xlsx
- 参考資料1 （５）救急車による搬送の有無（Excel：702KB) agg_qq
- 参考資料1 （６）救急医療入院（Excel：1,165KB）agg_kinkyu
- 参考資料2 (8)疾患別手術別集計 = aggregation_by_surgery_by_disease
- 参考資料2 （６）診断群分類毎の集計（Excel：16,999KB）= agg_by_dpc
- 参考資料2（４）救急車による搬送の有無の医療機関別MDC別集計 - agg_mdc2_qq
- 参考資料2 （３）予定・救急医療入院医療機関別MDC別集計 - agg_mdc2_yotei_kinkyu



### 医療施設調査 / ○○年医療施設（動態）調査 二次医療圏・市区町村編

政府統計コード	00450021

2次医療圏のデータとして使用

データは最新年度のものを使用

https://www.mhlw.go.jp/toukei/list/79-1a.html

### 2023/11/29

- 2018年度から2021年度の4年分のdetaをinputに入れた。
- 施設概要について、年度をまたいだmst番号を生成
- mdc10について10年分のデータを集計完了

## 2023/11/30

### アイデアをいただく点

- 月平均患者数に変更?
- 平均在院日数追加
- 二次医療圏シェアを追加
- sidebarに年度を追加すれば、その年度のマスタ検索をすることができる


### 不便だなと思う点

- mdc2別のグラフを見ながら疾患を選べた方がいい。
- mdc6別のグラフをみながら疾患が選べた方がいい。
- Dr.JOYに入っていないと見れないから不便。
- 県をまたいだ分析はできない点

## TODO

- 在院日数の実績をきちんと取得してくるようにする
- 追加データ取得 参考資料1（３）在院日数の状況（Excel：5,045KB） - agg_hp_day 
- 施設別集計に救急車による搬送をつける
- 予定緊急の指標を付ける


## 20231211

- 012_1-03_agg_stay_by_hp.Rを作成 done

1-03在院日数の状況 ~ 医療機関別の平均在院日数
出力はmstnoとfyとstay
table名はagg_stay_by_hp


- 014_1-05_agg_qq_by_hp.Rを作成 done

- 016_1-06_agg_qqiryo_by_hp.Rを作成 done

- 060_make_agg_for_shiny.Rのagg_long_hpにstayとqqとkinkyu列を追加 done

- 022_agg_qq_by_mdc2.Rを作成 done

- 024_agg_qqiryo_by_mdc2.Rを作成 done

- 060_make_agg_for_shiny.Rのagg_long_mdc2にqqとqqiryo列を追加 done

