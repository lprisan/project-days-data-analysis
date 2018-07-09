library(ggplot2)
library(tidyverse)
library(tidytext)
library(tm)
library(lda)
library("topicmodels")
library("ldatuning")
library(LDAvis)
library(servr)


plotNrTopics <- function(docids,texts,addstop=character()){
  
  stop_words <- c(stopwords("SMART"),addstop)
  
  comm <- data.frame(id=docids,text=texts, stringsAsFactors = F)
  
  # split into words
  comm_word <- comm %>%
    unnest_tokens(word, text)
  
  # TODO: remove words that appear less than 3-5 times in total?
  rare_words <- comm_word %>%
    count(word, sort = TRUE) %>%
    ungroup() %>%
    filter(n<3)
  
  # find document-word counts
  word_counts <- comm_word %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    filter(!(word %in% stop_words)) %>%
    filter(!(word %in% rare_words$word))
  
  dtm <- word_counts %>%
    cast_dtm(id, word, n)
  
  # TODO: Re-run to be sure which number is right... not it seems 6
  result <- FindTopicsNumber(
    dtm,
    topics = seq(from = 2, to = 15, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 1L,
    verbose = TRUE
  )
  FindTopicsNumber_plot(result)
  
  
}


getLDA <- function(docids, texts, k, n=4, addstop=character()){
  
  stop_words <- c(stopwords("SMART"),addstop)
  
  comm <- data.frame(id=docids,text=texts, stringsAsFactors = F)
  
  # split into words
  comm_word <- comm %>%
    unnest_tokens(word, text)
  
  # TODO: remove words that appear less than 3-5 times in total?
  rare_words <- comm_word %>%
    count(word, sort = TRUE) %>%
    ungroup() %>%
    filter(n<3)
  
  # find document-word counts
  word_counts <- comm_word %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    filter(!(word %in% stop_words)) %>%
    filter(!(word %in% rare_words$word))
  
  dtm <- word_counts %>%
    cast_dtm(id, word, n)
  
  #Basic LDA with k topics
  comm_lda <- LDA(dtm, k = k, control = list(seed = 1234))
  comm_topics <- tidy(comm_lda, matrix = "beta")
  #top_n(comm_topics, 10) # Top terms contributing to the different topics
  top_terms <- comm_topics %>%
    group_by(topic) %>%
    top_n(n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  print("Top topic terms")
  print(top_terms)
  
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  comm_lda
  
}

plotLDATopics <- function(comm_lda, n=4){
  
  comm_topics <- tidy(comm_lda, matrix = "beta")
  
  top_terms <- comm_topics %>%
    group_by(topic) %>%
    top_n(n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
}


getTopicLabels <- function(comm_lda, n=4){
  
  comm_topics <- tidy(comm_lda, matrix = "beta")
  
  # We create some topic labels:
  label_words <- comm_topics %>%
    group_by(topic) %>%
    top_n(n, beta) 
  
  labels_terms <- aggregate(term ~ topic, data = label_words, paste, collapse = ",")
  labels <- paste("Topic",labels_terms$topic,labels_terms$term, sep=".")
  
  
}

interactiveLDAVis <- function(docids, texts, k){
  
  
  # Visualization of results, once number of topics is set
  # MCMC and model tuning parameters:
  G <- 5000
  alpha <- 0.02
  eta <- 0.02
  # Fit the model:
  set.seed(357)
  t1 <- Sys.time()
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = k, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  t2 <- Sys.time()
  print(t2 - t1)  # about 24 minutes on laptop
  # Visualizing topics with LDAvis
  theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
  comments <- list(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)
  # create the JSON object to feed the visualization:
  json <- createJSON(phi = comments$phi, 
                     theta = comments$theta, 
                     doc.length = comments$doc.length, 
                     vocab = comments$vocab, 
                     term.frequency = comments$term.frequency)
  serVis(json, out.dir = 'vis', open.browser = T) # This opens a browser with an interactive visualization!
  
}
