
Average_word_per_sen = function(novel){
novel = tm::Corpus(DirSource(directory = "novels//Adventure_Stories",recursive = TRUE))
n = length(novel)
list_novel = unlist(novel)
list_novel = list_novel[1:n]
sent_per_doc = tokenizers::count_sentences(list_novel)
total_words = tokenizers::count_words(list_novel)

Average = total_words/sent_per_doc
return(Average)
}