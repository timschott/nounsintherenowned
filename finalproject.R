#tim schott
#final project....
setwd("~/Hacking")
library("e1071", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("ape", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("NLP", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("openNLP", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("mallet")
library("stylo")
library("wordcloud")
source('Rtools.R')

#load in texts

crusoe <- scan("robinson-crusoe.txt",what="character",sep="\n")
lighthouse <- scan("to-the-lighthouse.txt",what="character",sep="\n")
crusoe<-crusoe[-(1:30)]
crusoe<-gsub("_","",crusoe)
crusoe<-unlist(strsplit(tolower(crusoe),"\\W"))
crusoe<-crusoe[which(crusoe!="")]

lighthouse<-lighthouse[-(1:34)]
lighthouse<-gsub("_","",lighthouse)
lighthouse<-unlist(strsplit(tolower(lighthouse),"\\W"))
lighthouse<-lighthouse[which(lighthouse!="")]

length(crusoe)
length(lighthouse) #ligthouse is a little less than half the length of crusoe
#i'm going to break each up into pieces
x<-seq_along(crusoe)
y<-seq_along(lighthouse)

#chunk them into 2000 word long segments
bitsofcrusoe<-split(crusoe,ceiling(x/2000))
bitsofwoolf<-split(lighthouse,ceiling(y/2000))
bits.l<-c(bitsofcrusoe,bitsofwoolf)

bitsofcrusoe.freq.l<-lapply(bitsofcrusoe,table)
bitsofcrusoe.relfreq.l<-lapply(bitsofcrusoe.freq.l,prop.table)
names(bitsofcrusoe.relfreq.l)<-paste0("Crusoe",seq_along(bitsofcrusoe.relfreq.l))
bitsofwoolf.freq.l<-lapply(bitsofwoolf,table)
bitsofwoolf.relfreq.l<-lapply(bitsofwoolf.freq.l,prop.table)
names(bitsofwoolf.relfreq.l)<-paste0("Woolf",seq_along(bitsofwoolf.relfreq.l))


toDF <- function(x,j){
  data.frame(ID = j,
             Word = unlist(dimnames(x)),
             Freq = as.data.frame(x)$Freq,
             stringsAsFactors = FALSE)
}

woolf.freqs.l <- mapply(toDF,
                         x = bitsofwoolf.relfreq.l,
                         j = names(bitsofwoolf.relfreq.l),
                         SIMPLIFY = FALSE) 

defoe.freqs.l <- mapply(toDF,
                        x = bitsofcrusoe.relfreq.l,
                        j = names(bitsofcrusoe.relfreq.l),
                        SIMPLIFY = FALSE)
woolf.freqs.l[[2]][1:10,]
defoe.freqs.l[[2]][1:10,]

woolf.freqs.df<-do.call(rbind.data.frame,woolf.freqs.l)
row.names(woolf.freqs.df)<-NULL
defoe.freqs.df<-do.call(rbind,defoe.freqs.l)
row.names(defoe.freqs.df)<-NULL

alltogethernow.freqs.df<-rbind(woolf.freqs.df,defoe.freqs.df)
classificationdf<-alltogethernow.freqs.df
dim(alltogethernow.freqs.df)
result.t<-xtabs(Freq ~ ID+Word,data=alltogethernow.freqs.df)
dim(result.t)
final.df<-as.data.frame.matrix(result.t)
View(final.df)


chunks<-rownames(final.df) 
chunks[1:61]<-"Defoe" 
chunks[62:97]<-"Woolf"
final.df<-cbind(final.df,chunks)
View(final.df)
row.names(final.df)
final.df[1:97,c("providence", "i", "chunks")] # peek at it, looking at just 3 columns
dim(final.df) # 97 9884 -- That's p big!

freqmeans.v<-colMeans(final.df[,1:9883]) # find average frequency, across corpus, for each word
greaterthans.v<-which(freqmeans.v>=.001)

smaller.df<-final.df[,c(names(greaterthans.v),names(final.df[9884]))]
dim(smaller.df) #97 137

randomrows<-sample(nrow(smaller.df),nrow(smaller.df)/2)
train<-smaller.df[randomrows,] # train on these texts
test<-smaller.df[-randomrows,] # test on these
dim(train)
#?svm
model.svm<-svm(train[1:136],train$chunks)
summary(model.svm)

pred.svm<-predict(model.svm,train[1:136])
as.data.frame(pred.svm)
summary(pred.svm)
table(pred.svm,train$chunks)

testing<-predict(model.svm,test[1:136], decision.values=T)
predict(model.svm,test[1:136], decision.values=T) 
as.data.frame(testing)
summary(testing)
table(testing,test[,137])
#i'll test 10 times
#1 28 Defoe 21 Woolf
#2 34 D 15 W
#3 28 D 21 W
#4 32 D 17 W
#5 38 D 11 W

w = t(model.svm$coefs) %*% model.svm$SV # get feature weights
weights.df<-sort(as.data.frame(w))
View(weights.df)

#most heavily weighted Woolf words are She, her, Mr. and Mrs.
#most heavily weighted Defoe words are where, found, than
#refer to weights.df to see the ones that are in between

#clustering

rownames(classificationdf)<-NULL

# convert to a wide-form table
result<-xtabs(Freq ~ ID+Word,data=classificationdf)

# convert xtabs table to a matrix
bits.m<-apply(result,2,as.numeric)

# winnow, as necessary
reduced.m<-bits.m[,apply(bits.m,2,mean)>=.001]
reduced.m<-bits.m[,apply(bits.m,2,mean)>=.005]

# clustering
dm<-dist(reduced.m) # compare bits.m
cluster<-hclust(dm)
cluster$labels<-row.names(final.df)
plot(cluster,col.main = "#45ADA8",hang=-1,cex=.5)

#ok, so here are the crusoe cluster's that are
#hanging out (hahahaha) with lighthouse (relatively speaking--they share parents..)
#48,59, 53, 54
#what are those clusters?
#these are there line numbers. refer to them later, make text files out of them
#and close read for analysis--why are they similar to woolf?
bitsofcrusoe[48] # 7686 to 7837 pg. 245 while my man to 249 his Father's.
tabled48 <- sort(table(bitsofcrusoe[48]),decreasing=T) #sorted bitsofwoolf2[16]
tabled48
View(tabled48)
dbitsofcrusoe[53] # 8477 to 8635 pg. 266 By this means to 270 they came back to us

bitsofcrusoe[54] # 8635 to 8794 pg. 270 we had nothing to 274 where Friday was to take it

bitsofcrusoe[59] # 9443 to 9603 pg. 292 According, we all to 296 and stands stil. # bear attack
tabled59 <- sort(table(bitsofcrusoe[59]),decreasing=T) #sorted bitsofwoolf2[16]
tabled59
View(tabled59)

#prettier cluster?
library(ape)
plot(as.phylo(cluster), type = "fan")

#use nlp to tag & then remove all words that aren't nouns

crusoestring<-as.String(crusoe)
lighthousestring<-as.String(lighthouse)

#weird nlp boilerplate things
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

#tagging crusoe
a2cru <- annotate(crusoestring, list(sent_token_annotator, word_token_annotator))
a3cru <- annotate(crusoestring, pos_tag_annotator, a2cru)
a3cru <- subset(a3cru, type == "word")
tagcru <- sapply(a3cru$features, '[[', "POS")
crujustnouns<-grep("NN|NNS|NNP|NNPS",tagcru) #those are the positions of all the nouns
crusoejustnouns<-crusoe[crujustnouns] #ok. so this is crusoe with only nouns
tabledcru<- sort(table(crusoejustnouns),decreasing=T)
tabledcru[1:20]
#tagging lighthouse
a2light <- annotate(lighthousestring, list(sent_token_annotator, word_token_annotator))
a3light <- annotate(lighthousestring, pos_tag_annotator, a2light)
a3light <- subset(a3light, type == "word")
taglight <- sapply(a3light$features, '[[', "POS")
lightjustnouns<-grep("NN|NNS|NNP|NNPS",taglight)
lighthousejustnouns<-lighthouse[lightjustnouns] #this is lighthouse with only nouns.

#table lighthouse in order to find the most frequent noun
tabledlight<- sort(table(lighthousejustnouns),decreasing=T) #sorted bitsofwoolf2[16]
tabledlight[10:35]
View(tabledlight)
# mrs  something     things       eyes         mr     people    nothing     moment        man       time 
#215        170        114        105        103        100         98         94         91         88 
#way       room   children       hand      house        sea       life       mind        one lighthouse 
#86         82         81         76         75         75         73         72         69         67 
#hmmm. things is #3 which is p cool. what else, what else. sea? 

#before doing svm again, i'll take the character names out
lighthousecharacters<-grep("ramsay|briscoe|lily|carmichael|tansley|bankes|james|paul|rayley|doyle|prue|augustus|andrew|jasper|roger|macalister|rose|william|charles|minta", lighthousejustnouns)
lighthousejustnouns[lighthousecharacters[1:10]]
lighthousejustnouns<-lighthousejustnouns[-lighthousecharacters]

#write them to text files for when I need to look at them

#write(lighthousejustnouns, "lighthousenouns.txt", sep="\n")
#write(crusoejustnouns, "crusoenouns.txt", sep="\n") #(already done dont need to do it again)
#now it's clean.... time to do svm again
#i think i'll break it into tinier, 500 word chunks since i shaved off a fair amt of each text.
#just going to put a 2 on the end of all my variables

x2<-seq_along(crusoejustnouns)
y2<-seq_along(lighthousejustnouns)
bitsofcrusoe2<-split(crusoejustnouns,ceiling(x2/500))
bitsofwoolf2<-split(lighthousejustnouns,ceiling(y2/500))
bits.l2<-c(bitsofcrusoe2,bitsofwoolf2)

bitsofcrusoe.freq.l2<-lapply(bitsofcrusoe2,table)
bitsofcrusoe.relfreq.l2<-lapply(bitsofcrusoe.freq.l2,prop.table)
names(bitsofcrusoe.relfreq.l2)<-paste0("Crusoe",seq_along(bitsofcrusoe.relfreq.l2))
bitsofwoolf.freq.l2<-lapply(bitsofwoolf2,table)
bitsofwoolf.relfreq.l2<-lapply(bitsofwoolf.freq.l2,prop.table)
names(bitsofwoolf.relfreq.l2)<-paste0("Woolf",seq_along(bitsofwoolf.relfreq.l2))


woolf.freqs.l2 <- mapply(toDF,
                        x = bitsofwoolf.relfreq.l2,
                        j = names(bitsofwoolf.relfreq.l2),
                        SIMPLIFY = FALSE) 

defoe.freqs.l2 <- mapply(toDF,
                        x = bitsofcrusoe.relfreq.l2,
                        j = names(bitsofcrusoe.relfreq.l2),
                        SIMPLIFY = FALSE)
woolf.freqs.l2[[2]][1:10,]
defoe.freqs.l2[[2]][1:10,]

woolf.freqs.df2<-do.call(rbind.data.frame,woolf.freqs.l2)
row.names(woolf.freqs.df2)<-NULL
defoe.freqs.df2<-do.call(rbind,defoe.freqs.l2)
row.names(defoe.freqs.df2)<-NULL

alltogethernow.freqs.df2<-rbind(woolf.freqs.df2,defoe.freqs.df2) # one big happy family
#classificationdf2<-alltogethernow.freqs.df2
dim(alltogethernow.freqs.df2) 
result.t2<-xtabs(Freq ~ ID+Word,data=alltogethernow.freqs.df2)
dim(result.t2)
final.df2<-as.data.frame.matrix(result.t2)
View(final.df2)

chunks2<-rownames(final.df2) 
chunks2[1:44]<-"Defoe" 
chunks2[45:70]<-"Woolf"
final.df2<-cbind(final.df2,chunks2)
View(final.df2)
row.names(final.df2) #yalreadyknowitschefschottwiththepotBOI 
dim(final.df2) #70 5063
freqmeans.v2<-colMeans(final.df2[,1:5062]) # find average frequency, across corpus, for each word
#honey I shrunk the data!!!!!!
greaterthans.v2<-which(freqmeans.v2>=.001)
smaller.df2<-final.df2[,c(names(greaterthans.v2),names(final.df2[5063]))]
dim(smaller.df2) #70 177
#svm time.
randomrows2<-sample(nrow(smaller.df2),nrow(smaller.df2)/2)
train2<-smaller.df2[randomrows2,] # train on these texts
test2<-smaller.df2[-randomrows2,] # test on these
model.svm2<-svm(train2[1:176],train2$chunks)
summary(model.svm2)

pred.svm2<-predict(model.svm2,train2[1:176])
as.data.frame(pred.svm2)
summary(pred.svm2)
table(pred.svm2,train2$chunks)

testing2<-predict(model.svm2,test2[1:176], decision.values=T)
predict(model.svm2,test2[1:176], decision.values=T) 
as.data.frame(testing2)
summary(testing2)
table(testing2,test2[,177])

#1 25 Defoe 10 Woolf
#2  woolf's that were misclassfied. 25 defoe, 10 woolf. here are the
#woolf chunks that were misclassified: Woolf 16, 20, 23, 26
#3 25 defoe 10 woolf. misclassified: Woolf 26
#4 25 defoe, 10 woolf. none.
#5 #22 defoe, 13 woolf. misclassified: Woolf 16, 26.
#6 24 defoe, 11 woolf. misclassified: Woolf 7, 14, 16, 19, 20, 26

#word numbers in lighthousejustnouns / what pages they run from
bitsofwoolf2[16] #7501 to 8000 pg. 131 BUT WHAT to the bracketed sentences on 137 #time passes!!
#sorted16<-as.data.frame(bitsofwoolf2[16])
#View(sorted16)
tabled16 <- sort(table(bitsofwoolf2[16]),decreasing=T) #sorted bitsofwoolf2[16]
tabled16
View(tabled16)
tabled16[1:5]
bitsofwoolf2[20] #9501 to 10000 pg. 161 with a curious to 170 the island
bitsofwoolf2[23] #11001 to 11500 pg. 185 so fine to 193 give her?
bitsofwoolf2[26] #12501 to 13000 pg. 190 There he sat to 211 END #the amazing end!!
#these are there line numbers. refer to them later, make text files out of them
#and close read for analysis--why are they similar to woolf?

w2 = t(model.svm2$coefs) %*% model.svm2$SV # get feature weights
weights.df2<-sort(as.data.frame(w2))
print(weights.df2) #cant use view bc its too wide
#Most //Crusoe// weighted nouns: I, kind, manner, creatures, time, gun, providence
#Most //Lighthouse// weighted nouns: something, waves, moment, lawn, woman, table, eyes, air
#print it out to see the ones in the middle

#stylo

lightjustnormalnouns<-grep("NN|NNS",taglight)
crusoejustnormalnouns<-grep("NN|NNS",tagcru)
lighthousejustnormalnouns<-lighthouse[lightjustnormalnouns]
lighthousejustnormalnouns<-lighthousejustnormalnouns[-lighthousecharacters]
crusoejustnormalnouns<-crusoe[crusoejustnormalnouns]
write(lighthousejustnormalnouns, "lighthousejustnormalnouns.txt", sep="\n")
write(crusoejustnormalnouns, "crusoenormaljustnouns.txt", sep="\n")
#stylo on just the nouns
texts<-stylo(gui=FALSE)

#table w/ 100 Most frequent nouns and their relative frequencies
#idk if that's heplful tbqh
corpfreq.df<-data.frame(texts$table.with.all.freqs)
View(corpfreq.df)

crusoenouns.t<-table(crusoejustnormalnouns)
crusoenouns.sort.t<-sort(crusoenouns.t, decreasing=T)
crusoe10nouns.t<-crusoenouns.sort.t[1:10]
lighthousenouns.t<-table(lighthousejustnormalnouns)
lighthousenouns.sort.t<-sort(lighthousenouns.t, decreasing=T)
lighthouse10nouns.t<-lighthousenouns.sort.t[1:10]

crusoe10nouns.t
lighthouse10nouns.t

#topic modeling.... should I do it on the nouns or on all of the words? let's do nouns. since
texts.l<-getcorpus("corpus")
names(texts.l)

textsegmenter<-function(text,chunksize=500){ #arguments, give it a text, specify chunk size #i made mine 500
  x<-seq_along(text)
  chunks.l<-split(text,ceiling(x/chunksize))
  chunks.l<-lapply(chunks.l,paste,collapse=" ") #collapses bags of words together w/ blank space
  chunks.m<-do.call(rbind,chunks.l) #does it across the list, in this case by row
}
test.m<-textsegmenter(texts.l[[1]])
testcol.m<-cbind(paste(names(texts.l)[1],segment=1:nrow(test.m),sep="_"),test.m) #throw the text's name onto the matrix

topic.m<-NULL 
for(i in 1:length(texts.l)){ #a loop to populate an empty matrix
  text<-texts.l[[i]]
  chunked.m<-textsegmenter(text)
  textname<-names(texts.l)[i]
  segments.m<-cbind(paste(textname,segment=1:nrow(chunked.m),sep="_"),chunked.m)
  topic.m<-rbind(topic.m,segments.m)
}
rownames(topic.m)<-NULL

# Store the labeled text segments in a dataframe
documents<-as.data.frame(topic.m,stringsAsFactors=F)
colnames(documents)<-c("id","text")
dim(documents) # 70 document chunks, organized in 2 columns
View(documents)

mallet.instances<-mallet.import(documents$id, #h i d e o u s
                                documents$text,
                                "stoplist.csv",
                                FALSE,
                                token.regexp="[\\p{L}]+"
)

topic.model<-MalletLDA(num.topics=50) # Create topic trainer object, how many dif topics
topic.model$loadDocuments(mallet.instances) #489 max tokens, total tokens 30851

vocabulary<-topic.model$getVocabulary() # terms
word.freqs<-mallet.word.freqs(topic.model) # term frequencies (word, tf, and df)
head(word.freqs)
head(word.freqs[with(word.freqs, order(-word.freqs$term.freq)),]) # most frequent terms in corpus

topic.model$train(500) 
topic.words.m<-mallet.topic.words(topic.model,
                                  smoothed=TRUE,
                                  normalized=TRUE)
dim(topic.words.m) # 50 4976
colnames(topic.words.m)<-vocabulary

keywords<-c("table","boots", "corn", "pot")
topic.words.m[,keywords]
imp.row<-which(rowSums(topic.words.m[,keywords])==max(rowSums(topic.words.m[,keywords])))
imp.row #IT WILL BE DIFF EACH TIME
mallet.top.words(topic.model,topic.words.m[imp.row,],10) # top ten words in topic 
mallet.top.words(topic.model,colSums(topic.words.m),10) # top ten words in the model.. they're nautical for #7... how #UVa

topic.top.words<-mallet.top.words(topic.model,
                                  topic.words.m[7,],100)
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8),rot.per=0,random.order=F)
#7 was the nautical one.
#14 gave me a plot with THINGS! 
#20 also things!
#36 gave me a survival-ish.. one

doc.topics.m<-mallet.doc.topics(topic.model,
                                smoothed=T,
                                normalized=T) 
rownames(doc.topics.m)<-documents$id #each column a topic, each row a doc
View(doc.topics.m)
