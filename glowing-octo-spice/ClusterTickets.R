## These load libraries for dealing with JSON files,
## commented out to avoid re-loading several times during code manipulation.
install.packages('RCurl')
install.packages("RJSONIO")
library(RJSONIO)
library(RCurl)

x <- fromJSON(getURL(url, userpwd = password))
i = 0
end = as.integer(x$count/100)

## Initializes empty column vectors. 
ticket_original_contents <- c()
ticket_ids <- c()
y <- c()

while (i <= end){
  i = i + 1
  url = paste0(url,i)
  y[i] <- fromJSON(getURL(url, userpwd = password))
  print(i)
}
print('done GET request')
N <- 100
last_page <- end-1
c = 0

while (c<= last_page){
  c = c+1
  for (i in 1:N){
    f = i+(c-1)*100
    ticket_original_contents[f] <- y[[c]][[i]]$description 
    ticket_ids[f] <- y[[c]][[i]]$id
  }
}

lp_count = x$count%%100
for(i in 1:lp_count){
  f = i+(end)*100
  ticket_original_contents[f] <- y[[end+1]][[i]]$description
  ticket_ids[f] <- y[[end+1]][[i]]$id
}
print ('done creating lists')
## This cleans up the character/string information in the ticket descriptions to make it
## easier to process. (This could be improved further.)
ticket_contents_clean <- tolower(ticket_original_contents)
ticket_contents_clean <- gsub("\n","", ticket_contents_clean)
ticket_contents_clean <- gsub(">","", ticket_contents_clean)
ticket_contents_clean <- gsub("/"," ", ticket_contents_clean)
ticket_contents_clean <- gsub("_","", ticket_contents_clean)
ticket_contents_clean <- gsub("\\."," ", ticket_contents_clean)
ticket_contents_clean <- gsub(","," ", ticket_contents_clean)
ticket_contents_clean <- gsub(":"," ", ticket_contents_clean)
ticket_contents_clean <- gsub("!"," ", ticket_contents_clean)
ticket_contents_clean <- gsub(";"," ", ticket_contents_clean)


ticket_contents_clean <- gsub("please describe below the problem you are having so we can improve your shazam experience","", ticket_contents_clean)
ticket_contents_clean <- gsub("please describe your problem so that we can improve your experience withshazam","", ticket_contents_clean)
ticket_contents_clean <- gsub("please describe your problem so that we can improve your experience with shazam","", ticket_contents_clean)
ticket_contents_clean <- gsub("please describe below the problem you are having so we can improveyour shazam experience","", ticket_contents_clean)
ticket_contents_clean <- gsub("the following details relate to your application and device","", ticket_contents_clean)
ticket_contents_clean <- gsub("and helps us diagnose any technical issues which your comments might relate to","", ticket_contents_clean)
ticket_contents_clean <- gsub("and help us diagnose any technical issues which your comments might relate to","", ticket_contents_clean)
ticket_contents_clean <- gsub("and helps usdiagnose any technical issues which your comments might relate to","", ticket_contents_clean)
ticket_contents_clean <- gsub("however  you can remove them if you prefer","", ticket_contents_clean)
ticket_contents_clean <- gsub("following details relate to your application and device, and help us diagnose any technical issues which your comments might relate to","", ticket_contents_clean)

## Splits the processed strings into a list of words at every space between characters.
ticket_contents <- strsplit(ticket_contents_clean, "\\ ")
print('done clean-up')

## Initializes word_bank as an empty list.
## We will build a word_bank consisting of every unique word that shows up in the 
## ticket descriptions.
word_bank <- list()
## Cycles through every ticket discription,
for(i in 1:length(ticket_contents)){
  ## and every word in that description, 
  if(length(ticket_contents[[i]])>0){
    for(j in 1:length(ticket_contents[[i]])){
      ## checks whether that word is already in the word_bank,
      if(! ticket_contents[[i]][[j]] %in% word_bank){
        ## and adds it if it is not.
        word_bank[[length(word_bank) + 1]] <- ticket_contents[[i]][[j]]
      }
    }
  }
}

print('done word bank')

## Initializes a matrix of zeros with N columns and as many rows as words in the word_bank.
ticket_contents_matrix = matrix(0, x$count, length(word_bank))

## Cycle through every ticket,
for(i in 1:x$count){
## compares the list of words for that ticket description to the word_bank,
## returning a vector which has a 1 for the nth entry if the nth word in word_bank 
## shows up anywhere in the ticket description.
## This vector is then added as a row in the matrix ticket_contents_matrix
  #print(i)
  ticket_contents_matrix[i,] <- as.vector(as.numeric(!is.na(pmatch(word_bank, ticket_contents[[i]]))))
}
##ticket_contents_frame <- data.frame(ticket_contents_matrix)
print('done creating matrix')

## Now we will run k-means clustering for several choices of k clusters,
## then try to evaluate which value of k is most meaningful.

## Initialize two empty lists.
kClusts <- list()
scores <- vector()

## For i between 1 and 15, try clustering with k=i clusters, and store the
## 'cluster object' and the numeric 'score' as the ith entry in the lists kClusts and scores.
for(i in 1:15){
##  kClusts[[i]] <- kmeans(ticket_contents_frame,centers=i,nstart=10)
  kClusts[[i]] <- kmeans(ticket_contents_matrix,centers=i,nstart=10)
  scores[[i]] <- kClusts[[i]]$betweenss/kClusts[[i]]$totss
  print('done creating', i)
}
print('done creating clusters')
## Graphically show how well each successive choice of number of clusters performed.
plot(1:15, scores, pch=16, xlab="Clusters", ylab="Score")
print('done plot')
### Based on plot, it looks like 6 is a reasonable estimate for the number
### of clusters. We proceed now with k = 6 clusters.

k <- 5

## Create a data frame which lists each ticket's id, description, and which cluster to 
## which it was assigned.
output <- data.frame(ids = ticket_ids, contents = ticket_contents_clean, clusters = kClusts[[k]]$cluster)

## This will output a .csv file.
## Commented out until wanted.
write.csv(output, "cleanclusterdata.csv")
print('output saved!')