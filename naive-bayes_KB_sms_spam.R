#####spam
#devtools::install_github("junhewk/RmecabKo", INSTALL_opts=c('--no-lock'))
library(tm)
library(e1071)
library(tidyverse)
library(tidytext)
library(caret)
library(class)
library(RmecabKo)

end_row <- 13000
tot_end_row <- end_row + 1626

#dataload
setwd("C:/Users/etern")
library(readr)
tt1  <- read_csv("train.csv")
#tt1_2018 <- tt1 %>% filter (year_month >= '2017-01') %>% select (id,year_month,text,smishing)
load_data  <- read_csv("train.csv")
load_data <- as.data.frame(load_data)
#load_data <- load_data %>% filter (year_month >= '2017-01') %>% select (id,year_month,text,smishing)
data_train <- as.data.frame(load_data[1:end_row,])
data_train <- data_train[,c(3,4)] #!!!!1
data_train <- rename ( data_train , type = smishing)

###real�׽�Ʈ
tt2 <- read_csv("public_test.csv")
data_test <- read_csv("public_test.csv")
data_test <- as.data.frame(data_test)
type <- array(0,1626)
data_test <- cbind(data_test, type)
data_test <- as.data.frame(data_test)
data_test <- data_test[,c(3,4)] #!!!!2

#cont <- data.frame(data_train$text)
#cont <- rename(cont , text=data_train.text)
#test_cont <- data.frame( data_test$text)
#test_cont <- rename(test_cont, text=data_test.text)

data_tot <- rbind(data_train, data_test)


#Ư������
data_tot$text <- str_replace_all(data_tot$text , "-" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\/" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\-" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\<" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\>" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\(" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\)" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\[" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\]" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\*" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\." , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\'" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\:" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\~" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\=" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\@" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\&" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\%" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\III" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\IV" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\o" , " ")
#data_tot$text <- str_replace_all(data_tot$text , "\\V" , " ") #�̰ſ־ȵ�?
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\!" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\;" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\?" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\^" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\_" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\`" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\{" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\}" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\+" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\ii" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\iii" , " ")
data_tot$text <- str_replace_all(data_tot$text , "ii" , " ")
data_tot$text <- str_replace_all(data_tot$text , "iii" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "\\��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "��" , " ")
data_tot$text <- str_replace_all(data_tot$text , "��" , " ")

#��������
data_tot$text <- gsub("\\d+" , "" , data_tot$text)

#��������
data_tot$text <- gsub("[A-Za-z]","",data_tot$text)

#�������̱�
data_tot$text <- str_replace_all(data_tot$text, "[[:space:]]{1,}", " ")
data_tot$text <- str_replace_all(data_tot$text , "\\." , " ")
data_tot$text <- trimws(data_tot$text, which = "left")


for (i in 1:tot_end_row){
#data_tot$text[i] <- paste(token_nouns(data_tot$text[i]),collapse=" ")
data_tot$text[i] <- paste(token_words(data_tot$text[i]),collapse=" ")
}

for (i in 1:tot_end_row){
data_tot$type[i] <- ifelse(data_tot$type[i] == 0,'no','yes')
}

#��������
data_tot$text <- gsub("\\d+" , "" , data_tot$text)

#��������
data_tot$text <- gsub("[A-Za-z]","",data_tot$text)

#�������̱�
data_tot$text <- str_replace_all(data_tot$text, "[[:space:]]{1,}", " ")
data_tot$text <- str_replace_all(data_tot$text , "\\." , " ")
data_tot$text <- trimws(data_tot$text, which = "left")

sms_raw <- data_tot

    # 2. ������ ��ó�� -------------------------------------
sms_raw$type <- factor(sms_raw$type)

sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_corpus_clean <- sms_corpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind="en")) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)

###!!!!!!!!!!dtm
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#sms_dtm <- DocumentTermMatrix(sms_corpus_clean, control=list(wordLengths=c(2,9)))
#sms_dtm <- DocumentTermMatrix(sms_corpus_clean, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))


# 3. ����н� ���� -------------------------------------
## 3.1. �Ʒ�/���� ������ -------------------------------
#train_index <- createDataPartition(sms_raw$type, p=0.75, list=FALSE)
train_index <- c(1:end_row)
# �Ʒ�/���� ������
sms_raw_train <- sms_raw[train_index,]
sms_raw_test <- sms_raw[-train_index,]

# �Ʒ�/���� �ؽ�Ʈ ���۽�
sms_corpus_clean_train <- sms_corpus_clean[train_index]
sms_corpus_clean_test <- sms_corpus_clean[-train_index]

# �Ʒ�/���� �����ܾ����
sms_dtm_train <- sms_dtm[train_index,]
sms_dtm_test <- sms_dtm[-train_index,]

## 3.2. ����������� -------------------------------
sms_dict <- findFreqTerms(sms_dtm_train, lowfreq=5)
sms_train <- DocumentTermMatrix(sms_corpus_clean_train, list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_clean_test, list(dictionary=sms_dict))

convert_counts <- function(x) {
    x <- ifelse(x > 0, 1, 0)
    x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}

sms_train <- sms_train %>% apply(MARGIN=2, FUN=convert_counts)
sms_test <- sms_test %>% apply(MARGIN=2, FUN=convert_counts)

sms_train %>% tbl_df %>% 
    sample_n(100) %>% 
    dplyr::select(1:10) %>% 
    DT::datatable()
    
    
    
## 3.3. ��������� ���� -------------------------------
ctrl <- trainControl(method="cv", number=10)
nb_grid <- data.frame(.fL=c(1), .usekernel=c(FALSE), .adjust=c(FALSE))

system.time(
sms_nb_mod <- train(sms_train, sms_raw_train$type, 
                    method="nb",
                    tuneGrid = nb_grid,
                    trControl=ctrl)
)

##rf
#sms_rf_mod <- train(sms_train, sms_raw_train$type, 
#                    method="rf",
#                    verbose = F,
#                    trControl=ctrl)
#)


## 3.4. ��������� ������ -------------------------------
sms_naive_pred_cl <- predict(sms_nb_mod, sms_test ,type = 'raw')
sms_naive_pred_pb <- predict(sms_nb_mod, sms_test ,type = 'prob')


write.csv(sms_naive_pred_cl, file = 'sms_naive_pred_cl.csv')
write.csv(sms_naive_pred_pb, file = 'sms_naive_pred_pb.csv')



sms_naive_pred_pb %>% 
  gather(spam_ham, prob) %>% 
  ggplot(., aes(x=prob)) +
    geom_density(aes(color=spam_ham, fill=spam_ham, alpha=0.3)) +
    theme_bw(base_family = "NanumGothic") +
    scale_x_continuous(labels = scales::percent) +
    labs(x="", y="�е�", title="�ܹ����� ���� �з��� ����") +
    theme(legend.position = "top") +
    guides(alpha=FALSE)