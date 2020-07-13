# Buscar que el consecuente contenga items de tipo hashtag
# (%pin% indica matching parcial sobre el string del item)
subset(rules, subset = rhs  %pin% "hashtag=")

# Buscar que el consecuente contenga el hashtag chile
subset(rules, subset = rhs  %in% "hashtag=chile")

# Buscar que el antecedente contenga el hashtag chile O cuarentena
subset(rules, subset = lhs  %in% c("hashtag=cuarentena", "hashtag=chile"))

# Buscar que el antecedente contenga el hashtag chile Y cuarentena
subset(rules, subset = lhs  %ain% c("hashtag=argentina", "hashtag=chile"))

# Buscar que el antecedente contenga algún hashtag y en el consecuente
# la cantidad de amigos discretizada
subset(rules, subset = lhs  %pin% "hashtag=" & rhs  %pin% "friends=")

# Buscar que el antecedente contenga algún hashtag O
# en el consecuente la cantidad de amigos discretizada
subset(rules, subset = lhs  %pin% "hashtag=" | rhs  %pin% "friends=")

# Buscar que el antecedente contenga algún hashtag y que 
# el consecuente NO contenga el hashtag coronavirus
subset(rules, subset = lhs  %pin% "hashtag=" & !(rhs  %in% "hashtag=coronavirus"))


# Buscar que el itemset (antecedente o concecuente) tenga un hashtag
subset(rules, subset = items  %pin% "hashtag=")

# Buscar que el itemset (antecedente o concecuente) tenga un hashtag Y que el lift sea mayor a 3
subset(rules, subset = items  %pin% "hashtag=" & lift > 3)
