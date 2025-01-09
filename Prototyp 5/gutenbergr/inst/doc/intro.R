## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----basics-------------------------------------------------------------------
library(gutenbergr)
library(dplyr)
gutenberg_metadata

## ----filter-------------------------------------------------------------------

gutenberg_metadata %>%
  filter(title == "Persuasion")

## ----works--------------------------------------------------------------------
gutenberg_works()

## ----Austen-------------------------------------------------------------------
gutenberg_works(author == "Austen, Jane")

# or with a regular expression

library(stringr)
gutenberg_works(str_detect(author, "Austen"))

## ----load 1 file, echo=FALSE--------------------------------------------------
f105 <- system.file("extdata", "105.zip", package = "gutenbergr")
persuasion <- gutenberg_download(105,
  files = f105,
  mirror = "http://aleph.gutenberg.org"
)

## ----load 1 from web, eval = FALSE--------------------------------------------
#  persuasion <- gutenberg_download(105)

## ----display persuasion-------------------------------------------------------
persuasion

## ----load 2 from file, echo=FALSE---------------------------------------------
f109 <- system.file("extdata", "109.zip", package = "gutenbergr")
books <- gutenberg_download(c(109, 105),
  meta_fields = "title",
  files = c(f109, f105),
  mirror = "http://aleph.gutenberg.org"
)

## ----load 2 from web, eval = FALSE--------------------------------------------
#  books <- gutenberg_download(c(109, 105), meta_fields = "title")

## ----display books------------------------------------------------------------
books

## ----count books--------------------------------------------------------------
books %>%
  count(title)

## ----subjects-----------------------------------------------------------------
gutenberg_subjects

## ----filter subjects----------------------------------------------------------
gutenberg_subjects %>%
  filter(subject == "Detective and mystery stories")

gutenberg_subjects %>%
  filter(grepl("Holmes, Sherlock", subject))

## ----authors------------------------------------------------------------------
gutenberg_authors

## ----tidytext-----------------------------------------------------------------
library(tidytext)

words <- books %>%
  unnest_tokens(word, text)

words

word_counts <- words %>%
  anti_join(stop_words, by = "word") %>%
  count(title, word, sort = TRUE)

word_counts

