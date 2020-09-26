## Exercice 2
library(dplyr)
library(tm)
library(stopwords)
library(topicmodels)
library(tidytext)
library(ggplot2)
setwd("~/Documents/interviewZcase/kapCode/Test_technique_R_Kap_Code.V1")

# création du corpus
corpus <- read.csv("GS_oeil_sec_post.anonymized.csv", sep=";")
corpus_umess <- corpus %>%
        group_by(post_id) %>%
        summarise(med     = first(produit_name),
                  message = first(message_for_analyse))
umess <- mapply(gsub, 'nom_medicament', corpus_umess$med, corpus_umess$message)
messages_corpus <- VCorpus(VectorSource(umess))

# retrait de la ponctuation
messages_corpus <- tm_map(messages_corpus, removePunctuation, preserve_intra_word_dashes=TRUE)
remove_ufffd <- function(m) gsub('UFFFD','',m)
messages_corpus <- tm_map(messages_corpus, content_transformer(remove_ufffd))

# Retrait des tags utilisés pour l'anonymisation
tags <- c('DUREE', 'SEQUENCE', 'MAIL', 'NOM', 'PRENOM', 'VILLE', 'AGE', 'ADRESSE')
messages_corpus <- tm_map(messages_corpus, removeWords, tags)

# passage en minuscule
messages_corpus <- tm_map(messages_corpus, content_transformer(tolower))

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
print("Inspect de la DTM :")
inspect(dtm)

# Fréquence des termes
n_tokens <- 25
df <- as.data.frame(as.matrix(dtm))
freq <- sort(colSums(df), dec=TRUE)
first_tokens = freq[1:n_tokens]
barplot(first_tokens, las=2)

# Latent Dirichlet Allocation
n_topics <- 3
n_top <- 10
lda_model <- LDA(dtm, k=n_topics)
lda_tidy <- tidy(lda_model)
top_terms <- lda_tidy %>%
        group_by(topic) %>%
        top_n(n_top, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)

plt <- top_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~ topic, scales = "free") +
                coord_flip() +
                scale_x_reordered()

print(plt)