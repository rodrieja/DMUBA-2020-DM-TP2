library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)
library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tm)
library(stringr)
library(stringi)
library(arules)
library(Rcpp)
library(tokenizers)

mongon_retweet_quoted_spawn_time_TP2 <- mongo(collection = "retweet_quoted_spawn_time_TP2", db = "DMUBA")
retweet_quoted_spawn_time =  mongon_retweet_quoted_spawn_time_TP2$find(query = '{}')
summary(retweet_quoted_spawn_time)

lengths(retweet_quoted_spawn_time$hashtags, )

retweet_quoted_spawn_time$hashtags_num=retweet_quoted_spawn_time$hashtags

# Convierto los arrays en numero
lengt_of <- function(x){
  elements = 0
  if(!is.na(x)){
    elements = lengths(x)
  }
  elements
}

retweet_quoted_spawn_time["hashtags_num"] = apply(retweet_quoted_spawn_time["hashtags_num"], MARGIN = 1, lengt_of)

# Discretización de variables con distribución similar

####################   TWEET   ####################
retweet_quoted_spawn_time["cat_followers_count"] <- discretize(log10(retweet_quoted_spawn_time$followers_count),
                                                                method = "fixed",
                                                                breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_followers_count"])
# > summary(retweet_quoted_spawn_time["followers_count"])
# followers_count
# muy pocos : 15902   
# pocos     :250993   
# medio     : 78403   
# muchos    :  8273   
# muchisimos:  1195  

retweet_quoted_spawn_time["cat_friends_count"] <- discretize(log10(retweet_quoted_spawn_time$friends_count), 
                                                              method = "fixed", 
                                                              breaks = c(-Inf, 1, 3, 4, 5, Inf), 
                                                              labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))

summary(retweet_quoted_spawn_time["cat_friends_count"])
# > summary(retweet_quoted_spawn_time["friends_count"])
# friends_count
# muy pocos :  3368  
# pocos     :247134  
# medio     :101326  
# muchos    :  2858  
# muchisimos:    80  


####################   RETWEET   ####################
retweet_quoted_spawn_time["cat_retweet_favorite_count"] <- discretize(log10(retweet_quoted_spawn_time$retweet_favorite_count),
                                                                       method = "fixed",
                                                                       breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                       labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_retweet_favorite_count"])
# > summary(retweet_quoted_spawn_time["retweet_favorite_count"])
# retweet_favorite_count
# muy pocos : 78070          
# pocos     :174872          
# medio     : 59131          
# muchos    : 14871          
# muchisimos:  1017          
# NA's      : 26805 

retweet_quoted_spawn_time["cat_retweet_retweet_count"] <- discretize(log10(retweet_quoted_spawn_time$retweet_retweet_count),
                                                                      method = "fixed",
                                                                      breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                      labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_retweet_retweet_count"])
# > summary(retweet_quoted_spawn_time["retweet_retweet_count"])
# retweet_retweet_count
# muy pocos : 90033         
# pocos     :189797         
# medio     : 41771         
# muchos    :  6360         
# muchisimos:     0         
# NA's      : 26805 

retweet_quoted_spawn_time["cat_retweet_followers_count"] <- discretize(log10(retweet_quoted_spawn_time$retweet_followers_count), 
                                                                        method = "fixed", 
                                                                        breaks = c(-Inf, 1, 3, 4, 5, Inf), 
                                                                        labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_retweet_followers_count"])
# > summary(retweet_quoted_spawn_time["retweet_followers_count"])
# retweet_followers_count
# muy pocos :  1055           
# pocos     : 46421           
# medio     : 75090           
# muchos    : 95468           
# muchisimos:109927           
# NA's      : 26805  

retweet_quoted_spawn_time["cat_retweet_friends_count"] <- discretize(log10(retweet_quoted_spawn_time$retweet_friends_count), 
                                                                      method = "fixed", 
                                                                      breaks = c(-Inf, 1, 3, 4, 5, Inf), 
                                                                      labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_retweet_friends_count"])
# > summary(retweet_quoted_spawn_time["retweet_friends_count"])
# retweet_friends_count
# muy pocos :  5215         
# pocos     :177771         
# medio     :124527         
# muchos    : 19217         
# muchisimos:  1231         
# NA's      : 26805 


#################### QUOTE ####################
retweet_quoted_spawn_time["cat_quoted_favorite_count"] <- discretize(log10(retweet_quoted_spawn_time$quoted_favorite_count),
                                                                       method = "fixed",
                                                                       breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                       labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_quoted_favorite_count"])
# > summary(retweet_quoted_spawn_time["quoted_favorite_count"])
# quoted_favorite_count
# muy pocos : 10102         
# pocos     : 39064         
# medio     : 18366         
# muchos    :  3064         
# muchisimos:   346         
# NA's      :283824 

retweet_quoted_spawn_time["cat_quoted_retweet_count"] <- discretize(log10(retweet_quoted_spawn_time$quoted_retweet_count),
                                                                      method = "fixed",
                                                                      breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                      labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_quoted_retweet_count"])
# > summary(retweet_quoted_spawn_time["quoted_retweet_count"])
# quoted_retweet_count
# muy pocos : 14018        
# pocos     : 42088        
# medio     : 13348        
# muchos    :  1402        
# muchisimos:    86        
# NA's      :283824   

retweet_quoted_spawn_time["cat_quoted_followers_count"] <- discretize(log10(retweet_quoted_spawn_time$quoted_followers_count),
                                                                       method = "fixed",
                                                                       breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                       labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_quoted_followers_count"])
# > summary(retweet_quoted_spawn_time["quoted_followers_count"])
# quoted_followers_count
# muy pocos :   168          
# pocos     :  8148          
# medio     : 12043          
# muchos    : 16167          
# muchisimos: 34416          
# NA's      :283824 

retweet_quoted_spawn_time["cat_quoted_friends_count"] <- discretize(log10(retweet_quoted_spawn_time$quoted_friends_count),
                                                                     method = "fixed",
                                                                     breaks = c(-Inf, 1, 3, 4, 5, Inf),
                                                                     labels = c("muy pocos", "pocos", "medio", "muchos", "muchisimos"))
summary(retweet_quoted_spawn_time["cat_quoted_friends_count"])
# > summary(retweet_quoted_spawn_time["quoted_friends_count"])
# quoted_friends_count
# muy pocos :  1543        
# pocos     : 43304        
# medio     : 21969        
# muchos    :  3522        
# muchisimos:   604        
# NA's      :283824 

# Discretizo la cantidad de hashtags
retweet_quoted_spawn_time["cat_n_hashtags"] <- discretize(retweet_quoted_spawn_time$hashtags_num,
                                                           method = "fixed",
                                                           breaks = c(-Inf, 1, 2, 3, 8, Inf),
                                                           labels=c("sin", "unico","pocos", "varios", "muchos"))
summary(retweet_quoted_spawn_time["cat_n_hashtags"])
# > summary(retweet_quoted_spawn_time["n_hashtags"])
# n_hashtags
# unico :328384  
# pocos : 17556  
# varios:  8666  
# muchos:   160 

table(retweet_quoted_spawn_time$n_hashtags)
# unico = 1
# pocos = 2-3
# varios = 4-8
# muchos = 8-Inf

# unico  pocos varios muchos 
# 328384  17556   8666    160 

# Probamos con una distribución siguiendo una secuenco Fibonacci 
retweet_quoted_spawn_time["cat_hours_until_retweet"] = arules::discretize(retweet_quoted_spawn_time$hours_until_retweet,
                                                                          method = "fixed",
                                                                          breaks = c(-Inf, 1, 2, 8, 21, 55, 120, 233, 610, 2584, Inf),
                                                                          labels = c("hora", "2hs", "8hs", "dia", "2dias", "semana", "2semanas", "mes", "trimeste", "+3meses"))
summary(retweet_quoted_spawn_time["cat_hours_until_retweet"])
# > summary(retweet_quoted_spawn_time["hours_until_retweet"])
# hours_until_retweet
# hora   :98617           
# 8hs    :86201           
# dia    :69116           
# 2hs    :33380           
# 2dias  :28835           
# (Other):11812           
# NA's   :26805

retweet_quoted_spawn_time["cat_hours_until_quoted"] = arules::discretize(retweet_quoted_spawn_time$hours_until_quoted,
                                                                     method = "fixed",
                                                                     breaks = c(-Inf, 1, 2, 8, 21, 55, 120, 233, 610, 2584, Inf),
                                                                     labels = c("hora", "2hs", "8hs", "dia", "2dias", "semana", "2semanas", "mes", "trimeste", "+3meses"))
summary(retweet_quoted_spawn_time["cat_hours_until_quoted"])
# > summary(retweet_quoted_spawn_time["hours_until_quoted"])
# hours_until_quoted
# 8hs    : 17977         
# dia    : 17890         
# hora   : 12779         
# 2dias  :  9487         
# 2hs    :  5647         
# (Other):  7162         
# NA's   :283824

retweet_quoted_spawn_time["cat_verified"] = as.factor(retweet_quoted_spawn_time$verified)
summary(retweet_quoted_spawn_time["cat_verified"])

retweet_quoted_spawn_time["cat_retweet_verified"] = as.factor(retweet_quoted_spawn_time$retweet_verified)
summary(retweet_quoted_spawn_time["cat_retweet_verified"])

retweet_quoted_spawn_time["cat_quoted_verified"] = as.factor(retweet_quoted_spawn_time$quoted_verified)
summary(retweet_quoted_spawn_time["cat_quoted_verified"])

summary(retweet_quoted_spawn_time)

# -----------------------------------------------------------------------------------

df_tweets_tuples = retweet_quoted_spawn_time %>% 
  pivot_longer(
    cols = starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("status_id", "feat", "val")

df_tweets_tuples

# Se generan los pares TID ITEM (el TID es el user_id)
df_tuples = df_tweets_tuples %>% 
  mutate("item" = paste0(feat,"=",val)) %>% 
  select("status_id", "item")

df_tuples


# Generamos las transacciones
trans <- as(split(df_tuples$item, df_tuples$status_id), "transactions")
arules::inspect(trans[1:10])

# Buscamos reglas con min_sup=0.005 y min_conf=0.5
# Además, se limitan la cantidad de ítems (orden) entre 2 y 9
rules = arules::apriori(trans, parameter=list(target="rule", support=0.01, confidence=0.6, minlen=2, maxlen=15))
print(rules)

arules::inspect(sort(rules, by="lift", decreasing = TRUE)[1:20])


# --------------------- SUBSETS --------------------------

# Buscar que el consecuente contenga el hashtag chile
rhs_retweet_hora = subset(rules, subset = rhs  %pin% "hours_until_retweet=hora")
print(rhs_retweet_hora)
arules::inspect(sort(rhs_retweet_hora, by="lift", decreasing = TRUE)[1:100])


lhs_hashtag = subset(rules, subset = lhs  %in% "n_hashtags=varios")
print(lhs_hashtag)
arules::inspect(sort(lhs_hashtag, by="lift", decreasing = TRUE)[1:20])

# set of 0 rules
# muchos hashtags no parecen ser una tendencia

lhs_hashtag = subset(rules, subset = lhs %pin% "n_hashtags=" & rhs %in% "retweet_retweet_count=muchos")
print(lhs_hashtag)
arules::inspect(sort(lhs_hashtag, by="lift", decreasing = TRUE))
arules::inspect(sort(lhs_hashtag, by="lift", decreasing = TRUE)[1:20])


lhs_muchisimos = subset(rules, subset = rhs %in% "retweet_retweet_count=muchisimos")
print(lhs_muchisimos)
arules::inspect(sort(lhs_muchisimos, by="lift", decreasing = TRUE))
arules::inspect(sort(lhs_muchisimos, by="lift", decreasing = TRUE)[1:20])




subset = subset(rules, subset = lhs  %in% "hours_until_retweet=hora")
print(subset)
arules::inspect(sort(subset, by="lift", decreasing = TRUE)[1:20])

