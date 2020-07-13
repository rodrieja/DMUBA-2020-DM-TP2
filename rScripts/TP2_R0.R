install.packages("mongolite")
install.packages("tm")
install.packages("mice")
install.packages("arules")
install.packages('rlang')

# Save your workspace
save.image(file = "workspace_r0.RData")
# Load the workspace again
load("workspace_r0.RData")

# --------------------------------------------------------------------------------------------

library(mongolite)
library(arules)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(tm)
library(rlang)

# --------------------- Importacion de colecciones --------------------------------------------

# tweets
# Cargo las collections de MongoDB y las instancias
tweetsCollection0 = mongo(collection = "tweets_mongo_covid19", db = "DMUBA")
tweets_orig = tweetsCollection0$find('{}') 
tweetsCollection1 = mongo(collection = "tweets_mongo_covid19_TP2_reducido", db = "DMUBA")
tweets_red = tweetsCollection1$find('{}')
tweetsCollection2 = mongo(collection = "tweets_mongo_covid19_TP2_quoteados_completo", db = "DMUBA")
tweets_quot = tweetsCollection2$find('{}')
tweetsCollection3 = mongo(collection = "tweets_mongo_covid19_TP2_retweeteados", db = "DMUBA")
tweets_ret = tweetsCollection3$find('{}')

# users
# Cargo las collections de MongoDB y las instancias
usersCollection1 = mongo(collection = "users_mongo_covid19_TP2_reducido", db = "DMUBA")
users_red = usersCollection1$find('{}')
usersCollection2 = mongo(collection = "users_mongo_covid19_TP2_quoteados", db = "DMUBA")
users_quot = usersCollection2$find('{}')
usersCollection3 = mongo(collection = "users_mongo_covid19_TP2_retweeteados", db = "DMUBA")
users_ret = usersCollection3$find('{}')


head(tweets_red[tweets_red$is_retweet == TRUE & tweets_red$is_quote == TRUE,'status_url'],1)

# ----------------------- Preparacion de Dataset Tweets --------------------------------------

# TWEETS
tb_tw = tweets_red[tweets_red$is_retweet == FALSE & tweets_red$is_quote == FALSE,]
tb_tw = tb_tw[c('status_id','text','hashtags','source','favorite_count','retweet_count','urls_url','mentions_user_id','media_type')]

# RETWEETS
tb_rt = tweets_ret[c('retweet_status_id','retweet_text','hashtags','retweet_source','retweet_favorite_count','retweet_retweet_count','urls_url','mentions_user_id','media_type')]
#renombro columnas de retweet
colnames(tb_rt) = c('status_id','text','hashtags','source','favorite_count','retweet_count','urls_url','mentions_user_id','media_type')

# QUOTES
tb_qt = tweets_quot[c('quoted_status_id','quoted_text','hashtags','quoted_favorite_count','quoted_retweet_count','urls_url','mentions_user_id')]
# creo campos faltantes en quoted
tb_qt['quoted_source']=NA
tb_qt['media_type']=NA
# reordeno columnas en quoted
tb_qt = tb_qt[c('quoted_status_id','quoted_text','hashtags','quoted_source','quoted_favorite_count','quoted_retweet_count','urls_url','mentions_user_id','media_type')]
# renombro columnas de retweet
colnames(tb_qt) = c('status_id','text','hashtags','source','favorite_count','retweet_count','urls_url','mentions_user_id','media_type')

# genero columnas de tipo de tweet
tb_tw['type']='T'
tb_rt['type']='R'
tb_qt['type']='Q'

colnames(tb_tw)
colnames(tb_rt)
colnames(tb_qt)

# agrupo dataframes
tweets = rbind(tb_tw,tb_rt,tb_qt)

rm(tb_qt,tb_rt,tb_tw)

# ----------------------- Variables nuevas ---------------------------------------------------

# creo columna si tiene o no archivos multimedia
tweets['with_media'] = 'N'
tweets[!is.na(tweets$media_type),'with_media']='Y'

# creo columna % de caracteres usados
tweets['text_used'] = nchar(tweets$text)*100/280
hist(tweets$text_used)
hist(log10(tweets$text_used))
tweets['text_used'] = discretize(log10(tweets$text_used),method = "fixed", breaks = c(-Inf, 1, 2, Inf), labels=c("pocos","medio", "mucho"))
plot((tweets$text_used))

# cantidad de menciones
tweets['mentions_qty']=lengths(tweets$mentions_user_id)

# cantidad de urls
tweets['hashtags_qty']=lengths(tweets$hashtags)

# cantidad de hashtags
tweets['urls_qty']=lengths(tweets$urls_url)



# ----------------------- Discretizacion de numericas ----------------------------------------
# favorite_count
hist(tweets$favorite_count)
hist(log10(tweets$favorite_count))
tweets['favorite_count'] = discretize(log10(tweets$favorite_count),method = "fixed", breaks = c(-Inf, 1, 2, 3, 4, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
plot((tweets$favorite_count))

# retweet_count
hist(tweets$retweet_count)
hist(log10(tweets$retweet_count))
tweets['retweet_count'] = discretize(log10(tweets$retweet_count),method = "fixed", breaks = c(-Inf, 1, 2, 3, 4, Inf), labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos"))
plot((tweets$retweet_count))

# mentions_qty
table(tweets$mentions_qty)
hist(tweets$mentions_qty)
hist(log10(tweets$mentions_qty))
tweets['mentions_qty'] = discretize(tweets$mentions_qty,method = "fixed", breaks = c(0, 1, 2, 3, 4, Inf),right = F, labels=c("ninguno","uno", "dos", "tres", "cuatro o mas"))
plot((tweets$mentions_qty))

# hashtags_qty
table(tweets$hashtags_qty)
hist(tweets$hashtags_qty)
hist(log10(tweets$hashtags_qty))
tweets['hashtags_qty'] = discretize(tweets$hashtags_qty,method = "fixed", breaks = c(0, 1, 2, 4, 7, Inf),right = F, labels=c("ninguno","uno", "dos o tres", "cuatro a 6", "siete o mas"))
plot((tweets$hashtags_qty))

# urls_qty
table(tweets$urls_qty)
hist(tweets$urls_qty)
hist(log10(tweets$urls_qty))
tweets['urls_qty'] = discretize(tweets$urls_qty,method = "fixed", breaks = c(0, 1, 2, 3, Inf),right = F, labels=c("ninguno","uno", "dos", "tres o mas"))
plot((tweets$urls_qty))

# ----------------------- Limpieza de datos --------------------------------------------------

# source
sum(is.na(tweets$source))
a=data.frame(table(tweets$source)).
tweets$source[tweets$source %in% a[a$Freq<50,1]] = 'other'
tweets$source[is.na(tweets$source)] = 'other'
length(unique(tweets$source))
rm(a)

# ----------------------- Generacion de tablas individuales ----------------------------------

colnames(tweets)
# status_id | text |  hashtags | source | favorite_count | retweet_count | urls_url | mentions_user_id | 
# media_type | type | with_media | text_used | mentions_qty | hashtags_qty | urls_qty

a=c('source','favorite_count','retweet_count','urls_url', 'mentions_user_id','media_type','type','with_media','text_used','mentions_qty','hashtags_qty','urls_qty')
b=c('mentions_qty','hashtags_qty','urls_qty')
names_list = b
for (p in names_list) {
  temp = tweets[!is.na(tweets[p]),]
  temp = temp %>% unnest(!!sym(p))
  temp = temp %>% mutate("item" = paste0(p,"=",!!sym(p))) 
  temp = temp[c("status_id", "item")]
  assign(paste0('tweets_',p),temp) # hace p<-df_aux
}

rm(df_aux,temp,tweets_favorite_count,tweets_mentions_user_id,tweets_retweet_count,tweets_source,tweets_tuples)

tweets_favorite_count
tweets_hashtags
tweets_media_type
tweets_mentions_qty
tweets_mentions_user_id
tweets_mentions
tweets_retweet_count
tweets_source
tweets_text
tweets_text_used
tweets_type
tweets_with_media
tweets_hashtags_qty
tweets_urls_qty

# ----------------------- Tratamiento de Hashtags --------------------------------------------
# exploto hashtags en filas
tweets_hashtags = tweets[!is.na(tweets$hashtags),]
tweets_hashtags = tweets_hashtags %>% unnest(hashtags)

# limpieza
# se quitan acentos
tweets_hashtags$hashtags = stri_trans_general(tweets_hashtags$hashtags, "Latin-ASCII")
# se pasa a minúsculas
tweets_hashtags$hashtags = tolower(tweets_hashtags$hashtags)
# se quita todo lo que no es alfanumerico
tweets_hashtags$hashtags= gsub("[^a-zA-z0-9]", "", tweets_hashtags$hashtags)
# elimino los terminos de busqueda de los hashtags
tweets_hashtags = tweets_hashtags[-grep(x = tweets_hashtags$hashtags, pattern = "^(covid|coronav|cuarentena)"),]

head(tweets_hashtags[c('status_id','hashtags')],20)

# se agrega prefijo de tipo de item hashtag:
tweets_hashtags$item = paste0("hashtag=", tweets_hashtags$hashtags)
tweets_hashtags = tweets_hashtags[c("status_id", "item")]
head(tweets_hashtags[c('status_id','item')],20)

# ----------------------- Tratamiento de Textos ----------------------------------------------
tweets[c('status_id','text')]
tweets_text = tweets[c('status_id','text')]


# Se quitan caracteres no alfanumericos (por cuestiones de errores en RStudio)
tweets_text$text = gsub("[^[:alnum:][:blank:]?&/\\-]", "", tweets_text$text)
tweets_text$text = gsub("U00..", "", tweets_text$text)

# limpieza 
# Se quitan acentos
tweets_text$text = stri_trans_general(tweets_text$text, "Latin-ASCII")
# Se pasa a minusculas
tweets_text$text = tolower(tweets_text$text)
# Se quita puntuacion
tweets_text$text = removePunctuation(tweets_text$text)
# Se quitan numeros
tweets_text$text = removeNumbers(tweets_text$text)
# se quitan espacios extras
tweets_text$text =  stripWhitespace(tweets_text$text)
# se quitan espacios al principio y final de la cadena
tweets_text$text = str_trim(tweets_text$text)
# sin stop words
tweets_text$text = removeWords(tweets_text$text, stopwords("spanish"))
# se separa el texto en terminos
tweets_text$words = tokenizers::tokenize_words(tweets_text$text, simplify = T)
# se pasa a formato pares: id-termino
tweets_text = tweets_text %>% select("status_id", "words")  %>% unnest(words) %>%  distinct()
# se agrega prefijo de tipo de item
tweets_text$item = paste0("word=", tweets_text$words)
# se pasa a formato pares: id-termino
tweets_text = tweets_text %>% select("status_id", "item")


# ----------------------- Generacion de Tuplas (template) ------------------------------------

# Pivot de columnas seleccionadas
tweets_tuples = tweets %>% 
  pivot_longer(
    cols = c('source','favorite_count','retweet_count','urls_url','mentions_user_id','media_type','type','with_media','text_used'),
    names_to = "feat", 
    values_to = "val", 
    values_drop_na = TRUE) %>% 
  select("status_id", "feat", "val")

# se agrega prefijo de tipo de item:
df_user_tuples = df_user_tuples %>% 
  mutate("item" = paste0(feat,"=",val)) %>% 
  select("status_id", "item")

# ----------------------- Reglas de asociacion (template) ------------------------------------

# armo las transacciones que quiero analizar
tweets_tuples = rbind(df1, df1)

# reglas de asociacion
trans <- as(split(tweets_tuples$item, tweets_tuples$status_id), "transactions")
inspect(trans[100])
rules = apriori(trans, parameter=list(target="rule", support=0.001, confidence=0.5))
print(rules)
inspect(sort(rules, by="lift", decreasing = TRUE)[1:20])
inspect(head(rules, 20))