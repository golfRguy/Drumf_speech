# Load packages------------------------------------------------------------
# load 
packs <-c("tm", "wordcloud", "dplyr", "rvest, "RcolorBrewer")
lapply(packs, require, character.only = TRUE)

# Load the speech----------------------------------------------------------

html_doc <- read_html("http://www.vox.com/2016/7/21/12253426/donald-trump-acceptance-speech-transcript-republican-nomination-transcript")

speech <- html_doc %>% html_nodes("p > span") %>% html_text

speech <- speech[c(-1,-2,-3)]


# Put data in a corpus ----------------------------------------------------
my_corpus <- Corpus(VectorSource(speech))

# clean data ---------------------------------------------------------------
clean_corp <- function(corp) {
  corp = tm_map(corp,  content_transformer(tolower))
  corp = tm_map(corp, removePunctuation)
  corp = tm_map(corp, removeNumbers)
  corp = tm_map(corp, removeWords, stopwords("english"))
  return(corp)
}

my_corpus <- clean_corp(my_corpus)

# create term document matrix ---------------------------------------------

my_tdm = TermDocumentMatrix(my_corpus)

#make it a matrix
my_tdm_mat <- as.matrix(my_tdm)

# get word frequencies ----------------------------------------------------

freqs <- sort(rowSums(my_tdm_mat), decreasing = TRUE)

# create the word cloud ---------------------------------------------------

# The order of the words in the plot is random, 
# so setting the seed lets you recreate the same plot
# if you don't like the look, rerun wordcloud without
# resetting the seed or change the random order option

set.seed(1225)

 palette <- brewer.pal(n = 4, name = "PRGn")
 
 png("drumf_wordcloud.png", width=5,height=5, units='in', res=300)
 wordcloud(names(freqs), freqs, min.freq = 5, max.words = 200, color = palette,
           random.color = T, use.r.layout = F, scale=c(9,.2))
dev.off()
