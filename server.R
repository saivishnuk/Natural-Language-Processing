library(shiny)
library(lexicon)
library(sentimentr)
library(tidytext)
library(tidyr)
library(data.table)
library(shinythemes)
library(rvest)
library(tm)
library(rpart)
library(dplyr)
library(ggplot2)


function(input, output) {
  values <- reactiveValues(df_data1 = NULL, df_data2=NULL, df_data3=NULL, df_data4=NULL, df_data5=NULL, df_data6=NULL, df_data7=NULL, df_data8=NULL)
  
  #read department data
  observeEvent(input$file2, {
    
    web<-input$file2
    if(input$file2!="")
    {
      
      
      year<-read_html(web) %>%html_nodes("[class=cui-accordion-section__title]") %>%html_text()
      link<-read_html(web) %>%html_nodes("[class=star-rating-wrapper]") %>%html_attr("href")
      link<-paste0("https://www.cars.com",link)
      
      val <- data.frame(year,link,stringsAsFactors = F)
      val<-val[which(val$year<=2017 & val$year>=2012),]
      
      
      data <- data.frame()
      for (i in 1:nrow(val)){
        count <- 100
        rating <- data.frame(rating =((read_html(paste0(val$link[i],"?nr=",count)) %>%html_nodes("[class=review_card-text]") %>%html_attr("rating") )),stringsAsFactors = F)
        
        info <- read_html(paste0(val$link[i],"?nr=",count)) %>% html_nodes("cars-star-rating") %>% html_attr("rating")
        rating <- info[seq(8,length(info), by=7)]
        
        
        review <- data.frame(review =(read_html(paste0(val$link[i],"?nr=",count)) %>% html_nodes("[class=review-card-text]") %>%html_text()),stringsAsFactors = F)
        
        data2<-cbind(year= rep(val$year[i],count),rating,review)
        data<-rbind(data,data2)
        #count2 <- rbind(count,count2)
      }
      
      #data
      data$normalized<-tolower(removeNumbers(removePunctuation(data$review)))
      data$normalized2<-stripWhitespace(removeWords(tolower(removeNumbers(removePunctuation(data$review))), stopwords("english")))
      data$service <- ifelse(grepl("service", data$normalized), "service", "")
      data$price <- ifelse(grepl("price", data$normalized), "price", "")
      data$handling <- ifelse(grepl("handling", data$normalized), "handling", "")
      data$interior <- ifelse(grepl("interior", data$normalized), "interior", "")
      data$tags <- paste(data$service," ",data$handling," ",data$price," ",data$interior)
      
      
      data <- cbind(id = seq_len(nrow(data)),data)
      values$df_data1 <- data
      data <- data[,-c(6:10)]
      data <- data[,-c(1)]
      values$df_data2<-data.frame(data) #small
      
      values$df_data3=data[!data$year==2017,] #train
      values$df_data4=data[data$year==2017,]  #test
      
      
      #senti[is.na(senti)] <- "Missing"
      senti<-values$df_data1
      #senti$score<-sentiment(senti$normalized, polarity_dt = lexicon::hash_sentiment_sentiword)$sentiment
      senti$score<-sentiment(senti$normalized)$sentiment
      values$df_data6<-senti#[,c("year","rating","normalized","score")]
      
      
      #tfidf
      train=values$df_data1[!values$df_data1$year==2017,]
      
      train2 <- train %>%
        unnest_tokens(tags2, tags)  %>%
        unnest_tokens(word, normalized2) %>%
        count(tags2, word, sort = TRUE) %>%
        ungroup()
      
      train3 <- train2 %>%
        bind_tf_idf(word, tags2, n)   %>%
        arrange(desc(tf_idf))
      
      values$df_data8<-train3
    }
    else{
      values$df_data1<-""
      values$df_data2<-""
      values$df_data3<-""
      values$df_data4<-""
      values$df_data5<-""
      values$df_data6<-""
      
    }
  })
  
  #all data
  output$contents_all <- renderDataTable({
    values$df_data2
    #values$df_data8
  })
  
  #train
  output$contents1 <- renderDataTable({
    values$df_data3
    #values$df_data8
  })
  
  #test
  output$contents2 <- renderDataTable({
    values$df_data4
    #values$df_data8
  })
  
  
  #setiment score
  output$sentiment_all <- renderDataTable({
    req(input$file2)
    senti_all<-values$df_data6
    senti_all[is.na(senti_all)] <- "Missing"
    
    outp<-senti_all[,c("year","rating","normalized","score")]
    colnames(outp)<-c("year","rating","normalized","sentiment score")
    outp
    
  })
  
  #comparison
  output$sentiment_avg <- renderDataTable({
    req(input$file2)
    
    senti2 <- values$df_data6[!is.na(values$df_data6$score),]
    senti2_train=senti2[!senti2$year==2017,] #train
    senti2_test=senti2[senti2$year==2017,] #test
    senti2_all=senti2 #test
    
    #senti2_train=values$df_data6[!values$df_data6$year==2017,] #train
    #senti2_test=values$df_data6[values$df_data6$year==2017,] #test
    #senti2_all=values$df_data6 #test
    
    se_re <- data.frame()
    
    se_re = rbind(se_re,data.frame(a="All data:",b=mean(as.numeric(senti2_all$rating)),c=mean(as.numeric(senti2_all$score))))
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    se_re = rbind(se_re,data.frame(a="Train data:",b=mean(as.numeric(senti2_train$rating)),c=mean(as.numeric(senti2_train$score))))
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    se_re = rbind(se_re,data.frame(a="Test data:",b=mean(as.numeric(senti2_test$rating)),c=mean(as.numeric(senti2_test$score))))
    
    #se_re = rbind(se_re,data.frame(a="",b="",c=""))
    #se_re = rbind(se_re,data.frame(a="",b="",c=""))
    #se_re = rbind(se_re,data.frame(a="",b="",c=""))
    
    colnames(se_re)<-c("(All/Train/Test) data","Average user rating","Average sentiment rating")
    
    se_re
    
  })
  
  output$sentiment_avg_tags <- renderDataTable({
    req(input$file2)
    
    senti2 <- values$df_data6[!is.na(values$df_data6$score),]
    senti2_train=senti2[!senti2$year==2017,] #train
    senti2_test=senti2[senti2$year==2017,] #test
    senti2_all=senti2 #test
    
    #senti2_train=values$df_data6[!values$df_data6$year==2017,] #train
    #senti2_test=values$df_data6[values$df_data6$year==2017,] #test
    #senti2_all=values$df_data6 #test
    
    se_re <- data.frame()
    
    
    se_re = rbind(se_re,data.frame(a="All data, Tag: Service",b=mean(as.numeric(senti2_all[!senti2_all$service=="",]$rating)),c=mean(as.numeric(senti2_all[!senti2_all$service=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="All data, Tag: Price",b=mean(as.numeric(senti2_all[!senti2_all$price=="",]$rating)),c=mean(as.numeric(senti2_all[!senti2_all$price=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="All data, Tag: Handling",b=mean(as.numeric(senti2_all[!senti2_all$handling=="",]$rating)),c=mean(as.numeric(senti2_all[!senti2_all$handling=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="All data, Tag: Interior",b=mean(as.numeric(senti2_all[!senti2_all$interior=="",]$rating)),c=mean(as.numeric(senti2_all[!senti2_all$interior=="",]$score)) ))
    
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    
    se_re = rbind(se_re,data.frame(a="Train data, Tag: Service",b=mean(as.numeric(senti2_train[!senti2_train$service=="",]$rating)),c=mean(as.numeric(senti2_train[!senti2_train$service=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Train data, Tag: Price",b=mean(as.numeric(senti2_train[!senti2_train$price=="",]$rating)),c=mean(as.numeric(senti2_train[!senti2_train$price=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Train data, Tag: Handling",b=mean(as.numeric(senti2_train[!senti2_train$handling=="",]$rating)),c=mean(as.numeric(senti2_train[!senti2_train$handling=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Train data, Tag: Interior",b=mean(as.numeric(senti2_train[!senti2_train$interior=="",]$rating)),c=mean(as.numeric(senti2_train[!senti2_train$interior=="",]$score)) ))
    
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    se_re = rbind(se_re,data.frame(a="",b="",c=""))
    
    se_re = rbind(se_re,data.frame(a="Test data, Tag: Service",b=mean(as.numeric(senti2_test[!senti2_test$service=="",]$rating)),c=mean(as.numeric(senti2_test[!senti2_test$service=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Test data, Tag: Price",b=mean(as.numeric(senti2_test[!senti2_test$price=="",]$rating)),c=mean(as.numeric(senti2_test[!senti2_test$price=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Test data, Tag: Handling",b=mean(as.numeric(senti2_test[!senti2_test$handling=="",]$rating)),c=mean(as.numeric(senti2_test[!senti2_test$handling=="",]$score)) ))
    se_re = rbind(se_re,data.frame(a="Test data, Tag: Interior",b=mean(as.numeric(senti2_test[!senti2_test$interior=="",]$rating)),c=mean(as.numeric(senti2_test[!senti2_test$interior=="",]$score)) ))
    colnames(se_re)<-c("data: (All/Train/Test),  Tag: (Service,Price,Handling,Interior)","Average user rating","Average sentiment rating")
    
    se_re
    
  })
  
  #model
  output$sentiment_pre <- renderDataTable({
    req(input$file2)
    
    #values$df_data6<- values$df_data6[!is.na(values$df_data6),]
    
    senti2 <- values$df_data6[!is.na(values$df_data6$score),]
    senti2_train=senti2[!senti2$year==2017,] #train
    senti2_test=senti2[senti2$year==2017,] #test
    
    
    #senti2_train=values$df_data6[!values$df_data6$year==2017,] #train
    #senti2_test=values$df_data6[values$df_data6$year==2017,] #test
    
    dt_1<-rpart(rating~score,data=senti2_train,method="class",control = rpart.control(cp = 0.001, maxsurrogate=0))
    #predict(dt_1,train[, "mean_score"],type="class")
    
    new <- data.frame(score = senti2_train$score)
    senti2_train$predicted<-predict(dt_1, newdata = new, type="class")
    sum(senti2_train$predicted==senti2_train$rating)/nrow(senti2_train)*100
    #confusionMatrix(train$rating,train$predicted)
    
    new2 <- data.frame(score = senti2_test$score)
    senti2_test$predicted<-predict(dt_1, newdata = new2, type="class")
    sum(senti2_test$predicted==senti2_test$rating)/nrow(senti2_test)*100
    #confusionMatrix(test$rating,test$predicted)
    
    #new2
    
    se_pre<-data.frame()
    
    se_pre = rbind(se_pre,data.frame(a="Training Data Accuracy:",   b=(sum(senti2_train$predicted==senti2_train$rating)/nrow(senti2_train)*100)))
    
    se_pre = rbind(se_pre,data.frame(a="",b=""))
    
    
    se_pre = rbind(se_pre,data.frame(a="Test Data Accuracy:",   b=(sum(senti2_test$predicted==senti2_test$rating)/nrow(senti2_test)*100)))
    
    colnames(se_pre)<-c("Train/Test Data","Accuracy")
    se_pre
    
  })
  
  #no stop words
  output$tfidf_normal <- renderDataTable({
    req(input$file2)
    data2<-as.data.frame(cbind(values$df_data1$id,values$df_data1$review,values$df_data1$normalized,values$df_data1$normalized2))
    colnames(data2)<-c("id","review","normalized","normalized_with_no_stopwords")
    data2
    
  })
  
  #tfidf
  output$tfidf_service <- renderDataTable({
    req(input$file2)
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'service'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
  })
  
  output$tfidf_service_plot <- renderPlot({
    
    req(input$file2)
    
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'service'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
    
  })
  
  output$tfidf_price <- renderDataTable({
    req(input$file2)
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'price'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    #ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
  
  output$tfidf_price_plot <- renderPlot({
    req(input$file2)
    
    
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'price'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
  
  output$tfidf_handling <- renderDataTable({
    req(input$file2)
    
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'handling'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    #ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
  
  output$tfidf_handling_plot <- renderPlot({
    req(input$file2)
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'handling'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
  
  output$tfidf_interior <- renderDataTable({
    req(input$file2)
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'interior'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    #ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
  
  
  output$tfidf_interior_plot <- renderPlot({
    req(input$file2)
    train3<-values$df_data8
    tag<-train3[which(train3$tags2 == 'interior'),] %>%  top_n(10)
    tag$tf<-round(tag$tf,5)
    tag$idf<-round(tag$idf,5)
    tag$tf_idf<-round(tag$tf_idf,5)
    colnames(tag)<-c("tag","word","n","tf","idf","tf_idf")
    tag
    
    ggplot(tag, aes(x = reorder(word, -tf_idf), y = tf_idf)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "word")
    
  })
}
