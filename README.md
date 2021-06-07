# Dispersion metrics for computational corpus linguistics 

### Overview

Three measures of dispersion/adjusted frequency are written and used on a test corpus of news articles. The measures, TF-IDF, DP and ARF each provide notionally similar albeit different ways of calculating relative word importance, salience and dispersion in corpora. 

In computational corpus linguistics, efforts are consistently made to determine the relative 'importance' (i.e. how much new or identifying information is conveyed) of a word. Word salience, importance or information can be (loosely) thought of as the extent to which that word is dispersed throughout a corpus. The word 'the' for example, is the most commonly occurring word in English and is additionally quite evenly dispersed. This shows us that typically 'the' does not convey any new information and as such is deemed as less important (in a general sense). Conversely, a word which is concentrated in certain sections within a corpus may illustrate potential underlying topics in those sections. In this sense, these measures can be used to not only gain a better understanding of language, but also provide tools which facilitate information extraction and topic modelling pursuits.

### Usage 

Each .R file contains the same loading and cleaning functions. To use these measures simply format your corpus as .txt files in a directory, where each .txt file is a document/article/conversation etc and load in as is done in the three .R scripts. The files will be loaded in and subsequently 'cleaned' (cleaned not in an NLP sense, but more so basic formatting). 

