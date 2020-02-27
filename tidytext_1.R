text <- c("I like apple", "I like banana", "nice to meet you", "good bye")
text
#[1] "I like apple"     "I like banana"    "nice to meet you" "good bye"        

library(dplyr)
tibble(line = 1:4, text= text)
# A tibble: 4 x 2
#   line text            
#  <int> <chr>           
#1     1 I like apple    
#2     2 I like banana   
#3     3 nice to meet you
#4     4 good bye        

text_tibble <- tibble(line = 1:4, text= text)
text_tibble
# A tibble: 4 x 2
#   line text            
#  <int> <chr>           
#1     1 I like apple    
#2     2 I like banana   
#3     3 nice to meet you
#4     4 good bye  


library(tidytext)
text_tibble %>% unnest_tokens(word, text)
# A tibble: 12 x 2
#    line word  
#   <int> <chr> 
# 1     1 i     
# 2     1 like  
# 3     1 apple 
# 4     2 i     
# 5     2 like  
# 6     2 banana
# 7     3 nice  
# 8     3 to    
# 9     3 meet  
#10     3 you   
#11     4 good  
#12     4 bye