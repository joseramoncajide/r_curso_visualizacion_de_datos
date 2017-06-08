# install.packages('jsonlite')
# install.packages('dplyr')

library(jsonlite)
library(dplyr)

create_artist_query_url_lfm <- function(artist_name){
  prefix <- "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist="
  postfix <- "&api_key=c2e57923a25c03f3d8b317b3c8622b43&format=json"
  encoded_artist <- URLencode(artist_name)
  return(paste0(prefix, encoded_artist, postfix))
}


get_tag_frame_lfm <- function(an_artist){
  print(paste0("Attempting to fetch: ", an_artist))
  artist_url <- create_artist_query_url_lfm(an_artist)
  json <- fromJSON(artist_url)
  return(as.vector(json$toptags$tag[,"name"]))
}
artist_list <- c('madonna', 'u2','ub40','queen','metallica','beatles','george michael', 'britney spears', 'cher', 'jennifer lopez', 'acdc', 'dire straits', 'pink floyd', 'red hot chili pepers', 'bob marley')
artists_tags <- sapply(artist_list, get_tag_frame_lfm)
names(artists_tags) <- artist_list
cmbs <- combn(artist_list, 2)
comparisons <- data.frame(t(cmbs))
names(comparisons) <- c("artist1", "artist2")

jaccard_index <- function(tags1, tags2){
  length(intersect(tags1, tags2))/length(union(tags1, tags2))
}

comparisons$similarity <- apply(comparisons, 1,
                                function(arow){
                                  jaccard_index(artists_tags[[unlist(arow[1])]],
                                                artists_tags[[unlist(arow[2])]])
                                })



get_top_n <- function(comparisons, N, artist, threshold){
  comparisons %<>%
    filter(artist1==artist | artist2==artist) %>%
    arrange(desc(similarity))
  other_artist <- ifelse(comparisons$similarity>threshold,
                         ifelse(comparisons$artist1==artist,
                                comparisons$artist2, comparisons$artist1),
                         "None")
  return(other_artist[1:N])

}
nodes <- sapply(artist_list, function(x) get_top_n(comparisons, 3, x, 0.25))
nodes <- data.frame(t(nodes))
names(nodes) <- c("first", "second", "third")
nodes$name <- row.names(nodes)
row.names(nodes) <- NULL
nodes$group <- 1


lookup_number <- function(name) which(name==artist_list)-1

strong_links <- comparisons %>%
  filter(similarity > 0.25) %>%
  rename(node1 = artist1, node2 = artist2, weight=similarity)
strong_links$source <- sapply(strong_links$node1, lookup_number)
strong_links$target <- sapply(strong_links$node2, lookup_number)

object <- list("nodes"=nodes,
               "links"=strong_links)

sink("lastfm/artists.json")
toJSON(object, dataframe="rows", pretty=TRUE)
sink()

browseURL("http://jrcajide.com/artists/artists.html")
