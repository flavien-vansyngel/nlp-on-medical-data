## Exercice 3
library(dplyr)
library(stringr)
library(tm)
library(randomForest)
setwd("~/Documents/interviewZcase/kapCode/Test_technique_R_Kap_Code.V1")

###########
# Load data

corpus <- read.csv("GS_oeil_sec_phrase.anonymized.csv", sep=";")
load("intake_lexical_fields.simple.RData")


# Fonction pour ajouter une colonne contenant les phrases dans lesquelles
# il y a tous les noms de médicament (i.e. toutes les balises nom_medicament
# sont remplacées par le médicament correspondant)
add_phrase_complete <- function(df){
        df$phrase_complete <- mapply(gsub,
                                     'nom_medicament',
                                     tolower(df$produit_name),
                                     tolower(df$phrase))
        df
}


#####################################################
# Ajout de la feature nombre de balise nom_médicament

# La colonne phrase du data frame corpus comporte des doublons.
# Pour compter le nombre de balises :
#  - on groupe par phrase,
#  - on ajoute une colonne phrase_complete avec la fonction add_phrase_complete
#  - on groupe par phrase_complete en comptant leur nombre d'occurences
# La feature sera rajoutée au data frame corpus au moment de la construction
# des observations, cf. plus bas.
corpus_phrase <- corpus %>%
        group_by(phrase) %>%
        summarise(produit_name=first(produit_name))

corpus_phrase <- add_phrase_complete(corpus_phrase)

corpus_balise <- corpus_phrase %>%
        group_by(phrase_complete) %>%
        summarise(n_balise=n())


#########################################
# Ajout de la feature longueur du message

# Ajout des phrases complètes pour calculer la longeur des messages.
# Utiliser la colonne phrase au lieu de phrase_complete donnerait
# des valeurs différentes pour chaque occurence de la phrase
corpus            <- add_phrase_complete(corpus)
corpus$phrase_len <- lapply(corpus$phrase_complete, nchar) %>% unlist()


#####################
# Compter les pronoms

# dictionnaires de prénoms
pronoms_1 <- c(" je "," j "," me "," m ")
pronoms_2 <- c(" tu "," t "," te "," vous ")

# regex pattern : insertion d'opertor "ou" (|) entre les pronoms
pattern1 <- paste0("(",paste(pronoms_1, collapse="|"),")")
pattern2 <- paste0("(",paste(pronoms_2, collapse="|"),")")

# Fonction pour remplacer tous les pronoms 1ere (2e) personne par
# pronom1 (pronom2)
# La subsitution est faite deux fois de suite pour que les occurences de pronoms
# qui se trouvent côte à côte soient toutes remplacées
replace_pronoms <- function(phrase){
        phrase <- gsub(pattern1, " pronom1 ", phrase)
        phrase <- gsub(pattern1, " pronom1 ", phrase)
        phrase <- gsub(pattern2, " pronom2 ", phrase)
        phrase <- gsub(pattern2, " pronom2 ", phrase)
        phrase
}

# Ajout d'une colonne contenant les phrases où les pronoms sont remplacés
corpus$phrase_pronoms <- lapply(corpus$phrase_complete, replace_pronoms) %>% unlist()

# Ajout de colonne avec le nombre de pronoms à la 1ere et 2e personne
corpus$n_pronom1 <- lapply(corpus$phrase_pronoms, function(p) str_count(p, "pronom1")) %>% unlist()
corpus$n_pronom2 <- lapply(corpus$phrase_pronoms, function(p) str_count(p, "pronom2")) %>% unlist()


###################
# Calculer le score

# On remplace toutes les expressions valant un score de 1 (ou 3) par 
# la chaine de caractère score1 (ou score3) puis on compte le score total
# à partir du nombre d'occurences de ces chaînes de caractères
lex_field <- intake_lexical_field_fr_stem.simple
expr1     <- lex_field[lex_field$score == 1,1] %>% unlist()
expr3     <- lex_field[lex_field$score == 3,1] %>% unlist()
pattern1  <- paste0("(",paste(expr1, collapse="|"),")")
pattern3  <- paste0("(",paste(expr3, collapse="|"),")")
replace_score <- function(phrase){
        phrase <- gsub(pattern1, " score1 ", phrase, ignore.case=TRUE)
        phrase <- gsub(pattern1, " score1 ", phrase, ignore.case=TRUE)
        phrase <- gsub(pattern3, " score3 ", phrase, ignore.case=TRUE)
        phrase <- gsub(pattern3, " score3 ", phrase, ignore.case=TRUE)
        phrase
}
corpus$phrase_score <- lapply(corpus$phrase_complete,replace_score) %>% unlist()
corpus$score        <- lapply(corpus$phrase_score,
                              function(p) 3*str_count(p,"score3") + str_count(p,"score1")) %>% unlist()


###################################################
# Construction de la DTM avec les phrases complètes

# Conversion en objet corpus
messages_corpus <- VCorpus(VectorSource(corpus_balise$phrase_complete))

# Retrait de la ponctuation et caractères spéciaux
messages_corpus <- tm_map(messages_corpus, removePunctuation, preserve_intra_word_dashes=TRUE)
remove_ufffd    <- function(m) gsub('ufffd','',m)
messages_corpus <- tm_map(messages_corpus, content_transformer(remove_ufffd))

# Retrait des tags utilisés pour l'anonymisation
tags            <- c('<duree>', '<sequence>', '<mail>', '<nom>',
                     '<prenom>', '<ville>', '<age>', '<adresse>')
pattern         <- paste0("(",paste(tags, collapse="|"),")")
remove_tags     <- function(m) gsub(pattern,'',m)
messages_corpus <- tm_map(messages_corpus, content_transformer(remove_tags))

# retrait de stop words (ou mots vides)
messages_corpus <- tm_map(messages_corpus, removeWords, stopwords("fr"))

# retirer les caractères numériques
messages_corpus <- tm_map(messages_corpus, removeNumbers)

# raciniser les mots
messages_corpus <- tm_map(messages_corpus, stemDocument, language='french')

# Document-term matrix.
# Les termes apparaissant dans moins de 3 documents sont
# automatiquement retirés lors de la construction de la DTM.
# Il reste alors 1164 tokens
bnds <- list(global = c(3, Inf))
dtm  <- DocumentTermMatrix(messages_corpus,
                           control=list(bounds=bnds))
dtm_df <- as.data.frame(as.matrix(dtm))


###############################
# Construction des observations

# Concat des features n_balise et fréquence des mots de la DTM
corpus_balise <- cbind(corpus_balise, dtm_df)
corpus <- inner_join(corpus,corpus_balise,by='phrase_complete')

# Retirer les features non pertinentes
irrelevant <- c("post_id", "presence_id", "phrase_id", "phrase",
                "phrase_complete", "phrase_pronoms", "phrase_score")
data <- corpus %>% select(!c(irrelevant,produit_name))


################################################
# Application d'une classification random forest

data.rf <- randomForest(x     = data[1:200,]   %>% select(!prise_bool),
                        xtest = data[201:377,] %>% select(!prise_bool),
                        y     = as.factor(data[  1:200,]$prise_bool),
                        ytest = as.factor(data[201:377,]$prise_bool),
                        importance=TRUE)

# Plot du pouvoir explicatif des features (30 premières variables)
varImpPlot(data.rf, type=2)

# Matrice de confusion
print("Matrice de confusion :")
print(data.rf$confusion)

# Calcul du F1-score
tp        <- data.rf$confusion['TRUE',  'TRUE' ]
fp        <- data.rf$confusion['FALSE', 'TRUE' ]
fn        <- data.rf$confusion['TRUE',  'FALSE']
precision <- tp / (tp + fp)
recall    <- tp / (tp + fn)
f1        <- 2  / (1/precision + 1/recall)
print("F1-score :")
print(f1)

# Top 100 important features
features_by_importance <- sort(data.rf$importance[,"MeanDecreaseGini"],
                               decreasing=TRUE)
important_features <- names(features_by_importance[1:100])
print("Top 100 des features expliquant le mieux les données :")
print(important_features)
