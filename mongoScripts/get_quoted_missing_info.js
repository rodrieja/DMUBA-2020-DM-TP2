// Extrae los hashtags, urls y menciones del campo quoted_text y los asigna a nuevos atributos

db.getCollection('tweets_mongo_covid19_TP2_reducido').aggregate([
    {$project: {'quoted_user_id': 1,
                'quoted_status_id': 1,
                'quoted_text': 1,
                'quoted_created_at': 1,
                'quoted_favorite_count': 1,
                'quoted_retweet_count': 1,

                //Elimina los saltos de linea representados por '\n'

                 texto_filtrado : {$reduce: {
                                        input: { $split: ["$quoted_text", "\n"] },
                                        initialValue: "",
                                        in: { $concat : ["$$value", " ", "$$this"] }
                                        }
                                 }
                    }
    },

    // { $rtrim: { input: $ltrim: { input: $split: ["$texto_filtrado", " "],  chars: "#" },  chars: "." } }

    {$project: {'quoted_user_id': 1,
                'quoted_status_id': 1,
                'quoted_text': 1,
                'quoted_created_at': 1,
                'quoted_favorite_count': 1,
                'quoted_retweet_count': 1,           
                 hashtags:  { $filter: {
                                    input: { $split: ["$texto_filtrado", " "] },
                                    as: "word",
                                    cond: { $gt: [ {$indexOfCP:[ "$$word", "#" ]},-1] }
                                    }
                            },
                 mentions_user_id:  { $filter: {
                                    input: { $split: ["$texto_filtrado", " "] },
                                    as: "word",
                                    cond: { $gt: [ {$indexOfCP:[ "$$word", "@" ]},-1] }
                                    }
                            },
                 urls_url:      { $filter: {
                                    input: { $split: ["$texto_filtrado", " "] },
                                    as: "word",
                                    cond: { $gt: [ {$indexOfCP:[ "$$word", "http" ]},-1] }
                                    }   
                            }
                }
    }, 
    {$out : 'tweets_mongo_covid19_quoteados_completo'}
])

    

