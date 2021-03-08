#####1.準備#####
#アソシエーション分析を行うためのパッケージをインストール
#install.packages("arules")　#初回のみ実行
#install.packages("arulesViz") #初回のみ実行
library(arules)
library(arulesViz)

data(Groceries) #サンプルデータの呼び出し
Groceries@itemInfo　#アイテムリストを確認
Groceries@data #トランザクションの確認(※記号化されてて不可読)

attributes(Groceries)

Groceries@transactions
#Groceriesには、小規模食料品店の典型的なPOSデータが含まれている
#9,835件、169アイテム

#####2.ルールの抽出#####
rules <- apriori(Groceries, parameter=list(sup=0.001,conf=0.5))

subrules <- rules[quality(rules)$confidence>0.8]
inspect(subrules)

#多数結果が表示されるので、confidenceトップ20をピックアップ
rules_high_conf <- head(sort(rules, by="confidence"),20)
inspect(rules_high_conf)

#今度はsupportトップ20をピックアップ
rules_high_supp <- head(sort(rules, by="support"),20)
inspect(rules_high_supp)

#####3.結果のビジュアル化#####
#沢山のルールが抽出されたときは、plotで一覧する
plot(subrules, control=list(jitter=2))

#ルール数を絞った後はmatrixにすることでも傾向が把握しやすい
#plot関数にて、method="grouped"というオプションを指定する
plot(rules_high_conf, method="grouped")
plot(rules_high_supp, method="grouped")

#ルール数を絞った後はネットワーク図でも良い
#plot関数にて、method="graph"というオプションを指定する
#interactive=TRUEとすることで、ネットワーク図を編集可能
#現れる図の矢印の色の濃さはconfを表す
plot(rules_high_conf, method="graph", interactive=TRUE)
plot(rules_high_supp, method="graph", interactive=TRUE)