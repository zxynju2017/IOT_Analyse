#加载包、读取论文数据
library(dplyr)
library(tidyr)
#c<-dplyr::bind_rows(one,two)，用于列名不同的数据框按行合并，保留所有值  

#读取数据
iot_data<-read.csv("iot_data_new.csv",stringsAsFactors = FALSE,na.strings = "")
iot_title_keywords<-iot_data[,c(2,7,3)]
iot_title_abstract<-iot_data[,c(2,5,3)]

#数据清洗：去除标题、摘要缺失的条目
x<-is.na(iot_title_abstract$title_ch)
iot_title_abstract<-iot_title_abstract[!x,]
x<-is.na(iot_title_abstract$abstract_ch)
iot_title_abstract<-iot_title_abstract[!x,]

#去除摘要中的指示词、英文字符、阿拉伯数字部分，以解决:
#1.英文由于在中文摘要中出现次数较少而占有高区分度的问题
#2.中英文混合出现导致分词混乱的问题
iot_title_abstract$abstract_ch<-gsub("摘要:","",iot_title_abstract$abstract_ch)
iot_title_abstract$abstract_ch<-gsub("[a-zA-Z]","",iot_title_abstract$abstract_ch)
iot_title_abstract$abstract_ch<-gsub("[0-9]","",iot_title_abstract$abstract_ch)

#分词-词频统计-整洁文本格式
library(jiebaR)
wk<-worker(user = "IT_dict.txt" ,stop_word = "stop_word.txt")
title_words<-data.frame('title'=NULL,'char'=NULL,'freq'=NULL,'year'=NULL)
for(i in 1:length(iot_title_abstract$title_ch)){
  title<-data.frame('title'=iot_title_abstract$title[i])
  year<-data.frame('year'=iot_title_abstract$year[i])
  text<-iot_title_abstract$abstract[i]
  temp<-cbind(title,freq(wk[text]),year)
  title_words<-rbind(title_words,temp)
}

#tf-idf值计算
library(tidytext)
title_words_tf_idf<-bind_tf_idf(tbl = title_words,term = char,document = title,n=freq)

#筛选每篇文章分词结果中tf-idf值最高的五个词作为文献摘要的关键词
library(sqldf)
titles<-sqldf("select distinct title from title_words_tf_idf")
abstract_keywords<-data.frame(title=NULL,'char'=NULL,'freq'=NULL,'year'=NULL,'tf'=NULL,'idf'=NULL,'tf_idf'=NULL)
for(i in titles$title){
  temp<-subset(title_words_tf_idf,title_words_tf_idf$title==i)
  temp<-temp[order(temp$tf_idf,decreasing = TRUE),]
  abstract_keywords<-rbind(abstract_keywords,temp[1:5,])
}

#绘制2015-2020词云图,每年选取tf-idf值top100的词绘制
library(wordcloud2)
for(i in 2015:2020){
  abstract_keywords_by_year<-subset(abstract_keywords,abstract_keywords$year==i)
  wordcloud_data<-sqldf("select char,sum(tf_idf) as tf_idf from abstract_keywords_by_year group by char")
  wordcloud_data_sorted<-wordcloud_data[order(wordcloud_data$tf_idf,decreasing = TRUE),]
  print(wordcloud2(wordcloud_data_sorted[1:100,],size = 0.5))
}
print(wordcloud2(wordcloud_data_sorted,size = 0.5))#2020年词项不足100条，需单独绘制

#汇总分词结果及其tf-idf值，选取top15作为领域代表词
all_tf_idf<-sqldf("select char,sum(tf_idf) as tf_idf from abstract_keywords group by char")
field_keywords<-all_tf_idf[order(all_tf_idf$tf_idf,decreasing = TRUE),]
field_keywords<-field_keywords[1:15,]
barplot(field_keywords$tf_idf,names.arg = field_keywords$char,xlab = "关键词",ylab = "TF-IDF",main = "物联网热点领域TOP15")

#使用领域代表词去匹配文章，人工定义所属领域名称
field_define<-sqldf("select abstract_keywords.char,abstract_keywords.title,year from field_keywords,abstract_keywords where field_keywords.char=abstract_keywords.char")

#为五年文献标注领域：
five_year_data<-subset(field_define,field_define$year!=2020)
keyword_field<-data.frame("char"=field_keywords$char,"field"=c("智能制造","农业","算法设计","物联网通信","物联网通信","电子标签","智慧型社会建设","算法设计","物联网定位技术","信息安全","泛在电力物联网","智能服务系统","智慧城市","算法设计","物流"))
keyword_field<-merge(five_year_data,keyword_field,by="char")
article_count<-sqldf("select field,count(*) as article_num,year from keyword_field group by field,year")

#绘制折线图
library(ggplot2)
p<-ggplot(data=article_count, aes(x=year, y=article_num, group=field,color=field)) + geom_line() + geom_point() + xlab("年份") + ylab("文献数量") + ggtitle("物联网五年热点变化趋势图")

#数据清洗：去除关键词中的指示词，去除标题、关键词缺失的条目
x<-is.na(iot_title_keywords$keywords_ch)
iot_title_keywords<-iot_title_keywords[!x,]
x<-is.na(iot_title_keywords$title_ch)
iot_title_keywords<-iot_title_keywords[!x,]

x<-separate(iot_title_keywords,keywords_ch,c("w","keywords_ch"),sep=":")
iot_title_keywords<-x[,-2]
rownames(iot_title_keywords)<-1:length(iot_title_keywords$title_ch)

#整理关键词
iot_title_keywords$keywords_ch<-gsub(" ","",iot_title_keywords$keywords_ch)
tidy_title_keywords<-data.frame('title'=NULL,'keywords'=NULL,'year'=NULL)
for(i in 1:length(iot_title_keywords$title_ch)){
  title<-data.frame('title'=iot_title_keywords$title[i])
  year<-data.frame('year'=iot_title_keywords$year[i])
  text<-data.frame("keywords"=unlist(strsplit(iot_title_keywords$keywords_ch[i],split = ";")))
  temp<-cbind(title,text,year)
  tidy_title_keywords<-rbind(tidy_title_keywords,temp)
}
tidy_title_keywords$freq=1
keywords_tf_idf<-bind_tf_idf(tbl = tidy_title_keywords,term = keywords,document = title,n=freq)

#整理为聚类数据格式
connect<-data.frame("number"=1:length(iot_title_keywords$title_ch))
title<-iot_title_keywords$title_ch
connect<-cbind.data.frame(title,connect)
cluster_keywords<-keywords_tf_idf
cluster_keywords<-merge(cluster_keywords,connect,by='title')
cluster_keywords<-cluster_keywords[order(cluster_keywords$number),]
rownames(cluster_keywords)<-1:length(cluster_keywords$keywords)
cluster_keywords<-cluster_keywords[,c(8,2,7,3)]

# data_wide<-spread(subset(cluster_keywords,cluster_keywords$number==1),number,freq)
# for(i in 2:length(title)){
#   temp1<-spread(subset(cluster_keywords,cluster_keywords$number==i),number,freq)
#   data_wide<-merge(data_wide,temp1,by.x=c("keywords","year"),by.y = c("keywords","year"),all = TRUE)
# }

data_wide2<-spread(subset(cluster_keywords,cluster_keywords$number==1),keywords,tf_idf)
for(i in 2:length(title)){
  temp1<-spread(subset(cluster_keywords,cluster_keywords$number==i),keywords,tf_idf)
  data_wide2<-bind_rows(data_wide2,temp1)
}
col_na<-c()
for(i in 1:length(data_wide2)){
  if(all(is.na(data_wide2[,i])))col_na<-c(col_na,i)
}
data_wide2<-data_wide2[,-col_na]
data_wide2<-data_wide2[,-2]
data_wide2[is.na(data_wide2)] <- 0
write.csv(data_wide2,"wide2.csv",row.names = FALSE)

#聚类实现
# library(NbClust)
# devAskNewPage(ask = TRUE)
# rownames(data_wide2)<-data_wide2$number
# data_wide3<-data_wide2[,-1]
# data_wide3.scaled<-scale(data_wide3)
# nc<-NbClust(data_wide3.scaled[1:10,1:5],distance = "euclidean",min.nc = 2,max.nc = 15,method = "average")
# table(nc$Best.n[1,])
# barplot(table(nc$Best.n[1,]),xlab = "Number of Clusters",ylab = "Number of Criteria",main = "Nuber of Clusters Chosen by 26 Criteria")

d<-dist(data_wide2,method="euclidean") #计算矩阵距离
fit <- hclust(d, method="average")  #层次聚类算法
plot(fit)

# cutree(fit,k=3)
# plot_data<-data.frame("keywords"=one_year_data$keywords,"c_group"=cutree(fit,k=3))
# plot_weight<-subset(iot_keywords[,c(2,7)],iot_keywords$year==2015)
# plot_data<-sqldf("select plot_data.char,c_group,sum(tf_idf) from plot_data,plot_weight where plot_data.char=plot_weight.char group by plot_weight.char")
# write.csv(d,"dist.csv",row.names = FALSE)

#聚类改进
#1.追加词典(导出到txt后手动复制粘贴，获得新词典IT_dict_new.txt)
distinct_keywords<-sqldf("select distinct keywords from tidy_title_keywords")
write.table(distinct_keywords,"add_dic.txt")
  
#2.对摘要重新分词、构建整洁文本格式
new_abstract<-iot_data[,c(2,5,3)]

#数据清洗：去除标题、摘要缺失的条目
x<-is.na(new_abstract$title_ch)
new_abstract<-new_abstract[!x,]
x<-is.na(new_abstract$abstract_ch)
new_abstract<-new_abstract[!x,]

#去除摘要中的指示词
new_abstract$abstract_ch<-gsub("摘要:","",new_abstract$abstract_ch)

#分词-词频统计-整洁文本格式
wk<-worker(user = "IT_dict_new.txt" ,stop_word = "stop_word.txt")
new_title_words<-data.frame('title'=NULL,'char'=NULL,'freq'=NULL,'year'=NULL)
for(i in 1:length(new_abstract$title_ch)){
  title<-data.frame('title'=new_abstract$title[i])
  year<-data.frame('year'=new_abstract$year[i])
  text<-new_abstract$abstract[i]
  temp<-cbind(title,freq(wk[text]),year)
  new_title_words<-rbind(new_title_words,temp)
}

#提取出分词结果中的作者关键词
new_title_words<-sqldf("select title,keywords,freq,year from new_title_words,distinct_keywords where char=keywords")

#tf-idf值计算
new_title_words_tf_idf<-bind_tf_idf(tbl = new_title_words,term = keywords,document = title,n=freq)

#将新数据整理为聚类格式
connect<-data.frame("number"=1:length(new_abstract$title_ch))
title<-new_abstract$title_ch
connect<-cbind.data.frame(title,connect)
new_cluster_keywords<-new_title_words_tf_idf
new_cluster_keywords<-merge(new_cluster_keywords,connect,by='title')
new_cluster_keywords<-new_cluster_keywords[order(new_cluster_keywords$number),]
rownames(new_cluster_keywords)<-1:length(new_cluster_keywords$keywords)
new_cluster_keywords<-new_cluster_keywords[,c(8,2,7,4)]

data_wide3<-spread(subset(new_cluster_keywords,new_cluster_keywords$number==1),keywords,tf_idf)
for(i in 2:length(title)){
  temp1<-spread(subset(new_cluster_keywords,new_cluster_keywords$number==i),keywords,tf_idf)
  data_wide3<-bind_rows(data_wide3,temp1)
}
col_na<-c()
for(i in 1:length(data_wide3)){
  if(all(is.na(data_wide3[,i])))col_na<-c(col_na,i)
}
data_wide3<-data_wide3[,-col_na]
data_wide3<-data_wide3[,-2]
data_wide3[is.na(data_wide3)] <- 0
write.csv(data_wide3,"wide3.csv",row.names = FALSE)

#筛选关键词TOP20
#先从每篇文章中筛选TF-IDF值最高的3个词作为关键词
titles<-sqldf("select distinct title from new_title_words_tf_idf")
new_3_keywords<-data.frame(title=NULL,'char'=NULL,'freq'=NULL,'year'=NULL,'tf'=NULL,'idf'=NULL,'tf_idf'=NULL)
for(i in titles$title){
  temp<-subset(new_title_words_tf_idf,new_title_words_tf_idf$title==i)
  temp<-temp[order(temp$tf_idf,decreasing = TRUE),]
  new_3_keywords<-rbind(new_3_keywords,temp[1:3,])
}

new_sum_tf_idf<-sqldf("select keywords,sum(tf_idf) as tf_idf from new_3_keywords group by keywords")
top_keywords<-new_sum_tf_idf[order(new_sum_tf_idf$tf_idf,decreasing = T),][1:20,]
barplot(top_keywords$tf_idf,names.arg = top_keywords$keywords,ylab = "TF-IDF",main = "物联网热点领域TOP20",las=2)

#使用筛选出的关键词去匹配文档
cluster_article<-sqldf("select title,top_keywords.keywords,new_cluster_keywords.tf_idf,year from top_keywords,new_cluster_keywords,connect where top_keywords.keywords=new_cluster_keywords.keywords and new_cluster_keywords.number=connect.number")
#cluster_article<-sqldf("select number,top_keywords.keywords,new_cluster_keywords.tf_idf from top_keywords,new_cluster_keywords where top_keywords.keywords=new_cluster_keywords.keywords")

#为五年文献标注领域：
five_year_data<-subset(cluster_article,cluster_article$year!=2020)
keyword_field<-data.frame("char"=top_keywords$keywords,"field"=c("算法设计","认证协议","电子标签","算法设计","泛在电力物联网","农业物联网","物联网定位技术","智能家居","农业","信息安全","算法设计","区块链","大数据","生产","物联网电源监控","监管","认证协议","物联网设备","智慧城市","智能制造"))
keyword_field<-merge(five_year_data,keyword_field,by.y = "char",by.x = "keywords")
article_count<-sqldf("select field,count(*) as article_num,year from keyword_field group by field,year")

#绘制折线图
library(ggplot2)
p<-ggplot(data=article_count, aes(x=year, y=article_num, group=field,color=field)) + geom_line() + geom_point() + xlab("年份") + ylab("文献数量") + ggtitle("物联网五年热点变化趋势图")

#最新热点分析
latest_data<-subset(new_title_words_tf_idf,new_title_words_tf_idf$year==2020)
titles<-sqldf("select distinct title from latest_data")
latest_5_keywords<-data.frame(title=NULL,'char'=NULL,'freq'=NULL,'year'=NULL,'tf'=NULL,'idf'=NULL,'tf_idf'=NULL)
for(i in titles$title){
  temp<-subset(latest_data,latest_data$title==i)
  temp<-temp[order(temp$tf_idf,decreasing = TRUE),]
  latest_5_keywords<-rbind(latest_5_keywords,temp[1:5,])
}
latest_new_tf_idf<-sqldf("select keywords,sum(tf_idf) as tf_idf from latest_5_keywords group by keywords")
latest_top10<-latest_new_tf_idf[order(latest_new_tf_idf$tf_idf,decreasing = T),][1:10,]
barplot(latest_top10$tf_idf,names.arg = latest_top10$keywords,ylab = "TF-IDF",main = "2020物联网热点领域",las=2)
