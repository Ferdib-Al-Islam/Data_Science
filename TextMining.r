library(pubmed.mineR)
library(RISmed)

keyword <- "Deep Learning"
search_query <- EUtilsSummary(keyword, retmax = 5)
summary(search_query)
extractedResult <- EUtilsGet(search_query)
pmid <- PMID(extractedResult)
years <- YearPubmed(extractedResult)
title <- Title(extractedResult)
articleTitle <- ArticleTitle(extractedResult)
abstracts <- AbstractText(extractedResult)
abstracts[5]
dat <- data.frame(pmid,years,title,articleTitle,abstracts,stringsAsFactors = FALSE)

library(tm)
AbstractCorpus <- Corpus(VectorSource(abstracts))
AbstractCorpus <- tm_map(AbstractCorpus,removePunctuation)
AbstractCorpus <-tm_map(AbstractCorpus, removeNumbers)
AbstractCorpus <- tm_map(AbstractCorpus,content_transformer(tolower))

Stopwords <- c(stopwords("english"))
AbstractCorpus <- tm_map(AbstractCorpus,removeWords,Stopwords)
AbstractCorpus <- tm_map(AbstractCorpus, stemDocument)
termDocMat <- TermDocumentMatrix(AbstractCorpus, control = list(minWordLength=1))
