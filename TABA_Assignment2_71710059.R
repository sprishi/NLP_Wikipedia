rm(list = ls())   # clears workspace

# Install or load packages
library("openNLP")
library("NLP")
library(rvest)

page = read_html('https://en.wikipedia.org/wiki/Apple_Inc.') # URL of the target page 
text <- page %>% html_nodes('p , p+ ul li') %>% html_text() # Extract text from the page

text <- text[which(text != "")] # Remove empty lines
text <- paste(text, collapse = " ") # Merge all the texts to one
text <- gsub("\\[[0-9]]|\\[[0-9][0-9]]|\\[[0-9][0-9][0-9]]","",text) # Remove references [111] type
text = gsub("[^[:ascii:]]","",text, perl = T) # Remove non ascii characters
text <- as.String(text) # Convert character to text

#Function to clean unwanted characters
clean <- function(x){
  x <- gsub("[^[:alnum:]]", " ", x)
  x <- removeNumbers(x) 
  x <- gsub("\\n", "", x)
  x <- gsub("^\\s+|\\s+$", "", x) 
  x <- x[which(x != "")]
  return(x)
}

#Apply all the NLP annotators to the text
sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
entity_annotator_loc = Maxent_Entity_Annotator(kind = "location")
entity_annotator_prs = Maxent_Entity_Annotator(kind = "person")
annot.l1 = NLP::annotate(text, list(sent_token_annotator, word_token_annotator, entity_annotator_loc, entity_annotator_prs))

head(annot.l1); tail(annot.l1)

#Filter & clean the locations and names 
k <- sapply(annot.l1$features, `[[`, "kind")
locations <- (text[annot.l1[k == "location"]]) %>% unique() %>% clean()
persons <- (text[annot.l1[k == "person"]]) %>% unique() %>% clean()

#Locations extracted from the page
print(locations)

#Names or Persons mentioned in the page
print(persons)

#Plot the extracted locations to map
locs <- geocode(locations)
newmap <- getMap(resolution = "high")
plot(newmap, asp = 1) + points(locs$lon, locs$lat, col = "blue", cex = 1.0, pch = 18)

#Extract all the numbers with dollars mention in article
dollar_nums = unique(unlist(strapplyc(text,"[US]*\\${1}\\d*[.,]*\\d[\\sm]*[million]*[billion]*")))
print(dollar_nums)

#Extract all the years mention in article
year = unique(unlist(strapplyc(text,"[12]{1}[0789]{1}\\d{2}")))
print(year)
