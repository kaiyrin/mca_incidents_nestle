
# Load required packages
library(dplyr)
library(tidytext)
library(readr)
library(NLP)
library(tm)
library(topicmodels)
library(sentimentr)
library(cld2)
library(textcat)
library(stringr)
library(SnowballC)
library(ClusterR)
library(cluster)

library(tm)   # For text preprocessing
library(ggplot2)  # For data visualization
#Import
incident_data <- incident
incident_data <- incident_data[,c(1:4,6:8,10:14,18,19)]
incident_data <- na.omit(incident_data)
df<- incident_data 
incident_data$description <-incident_data$short_description

# Convert text to lowercase
incident_data <- incident_data %>%
  mutate(description = tolower(description))

# Remove punctuation and numbers
incident_data <- incident_data %>%
  mutate(description = gsub("[^[:alpha:][:space:]]*", "", description))

# Remove in brackets
incident_data <- incident_data %>%
  mutate(short_description = gsub("\\[.*?\\]", "", description))


# Tokenize text into individual words
incident_data_tokens <- incident_data %>%
  unnest_tokens(word, description)

# Remove stop words
incident_data_tokens <- incident_data_tokens %>%
  anti_join(stop_words)

# Remove custom stop words (if any)
custom_stop_words <- c("example", "stopword", "anotherword")
incident_data_tokens <- incident_data_tokens %>%
  filter(!word %in% custom_stop_words)

# Stem words
incident_data_tokens <- incident_data_tokens %>%
  mutate(word_stem = wordStem(word))

# Remove rare words
word_counts <- incident_data_tokens %>%
  count(word)
rare_words <- word_counts %>%
  filter(n == 1) %>%
  pull(word)
incident_data_tokens <- incident_data_tokens %>%
  filter(!word %in% rare_words)

# Remove leading spaces
incident_data <- incident_data %>%
  mutate(description = trimws(description, "left"))

incident_data <- incident_data %>%
  mutate(description = tolower(description),
         words = strsplit(description, "\\s+")) 

incident_data

#THATS ALL I CHECKED
# Create a vector of Unicode code points for Han characters
han_code_points <- seq(from = 0x4E00, to = 0x9FFF)

# Convert code points to Unicode characters
han_chars <- intToUtf8(han_code_points)

# Convert vector to list, excluding empty strings
han_list <- as.list(han_chars[han_chars != ""])

#incident_data <- incident_data %>%
# mutate(description <- gsub(paste0("\\b\\S*(", paste(paste0(han_list, collapse = "|"), ")", "\\S*\\b")), " ", incident_data$description)
#)
#incident_data <- incident_data %>%
#mutate(description = gsub(han_list, " ", description))
#incident_data <- incident_data %>%
#mutate(description = str_replace_all(incident_data$description, "[\\b\\S*\\\u0000-\u007F]", " "))

incident_data <- incident_data %>%
  filter(!grepl("^zgccn", description))
  
# define a regular expression to match Chinese characters
chinese_pattern <- "[\u4e00-\u9fff]"
# c2<-"[\u0000-\u007F]"

# remove all Chinese characters from the description column
incident_data$description <- str_replace_all(incident_data$description, chinese_pattern, " ")
# incident_data$description <- str_replace_all(incident_data$description, c2, " ")
# remove all Chinese characters from the description column
incident_data <- incident_data %>%
  mutate(description = str_replace_all(incident_data$description, "[\u4e00-\u9fff]", " "))
incident_data <- incident_data %>%
  mutate(description = str_replace_all(incident_data$description, "[\\b\\S*\\\u0000-\u007F]", " "))
# Define a simpler regular expression to match any Chinese character
regex <- "[\u4e00-\u9fff]+"

# Use gsub() function with the simpler regular expression
incident_data <- incident_data %>%
  mutate(description = gsub(regex, " ", description))
#OTHER


# Create a new column for language type

incident_data$lang <- textcat(incident_data$description)
num_descs <- incident_data %>% distinct(lang) %>% nrow()
print(num_descs)
unique_descs <- incident_data %>% distinct(lang) %>% pull()
print(unique_descs)
# Split data frame into multiple data frames based on language type
# Define a vector of language names
language_names <- c("spanish", "danish", "esperanto", "french", "polish", "indonesian", "portuguese",
                    "english", "scots", "catalan", "middle_frisian", "german", "rumantsch", "dutch", "latin",
                    "italian", "frisian", "afrikaans", "breton", "basque", "romanian", "bosnian", "estonian",
                    "swedish", "irish", "serbian-ascii", "manx", "slovenian-ascii", "icelandic", "croatian-ascii",
                    "lithuanian", "finnish", "scots_gaelic", "slovak-windows1250", "welsh", "latvian", "norwegian")

# Subset the incident_data data frame to include only rows where wordlang is one of the specified languages
latin_data <- incident_data[incident_data$lang %in% language_names, ]
latin_data <- latin_data %>%
  filter(!grepl("^лицензия", description))
latin_data <- latin_data %>%
  filter(!grepl("^prime mp", description))
latin_data <- latin_data %>%
  filter(!grepl("^Yushan Lin", assigned_to))
latin_data <- incident_data %>% filter(!grepl("zgc", as.character(description)))
latin_data <- latin_data %>%
  filter(!grepl("^敬子 垣内", assigned_to))
latin_data <- latin_data %>% filter(!grepl("@cn.nestle.com", caller_id))
latin_data <- latin_data %>% filter(!grepl("@CN.nestle.com", caller_id))
latin_data <- latin_data %>% filter(!grepl("@wyethnutrition.com", caller_id))
latin_data <- latin_data %>% filter(!grepl("@wyethnutrition.com", caller_id))
latin_data <- latin_data %>% filter(!grepl("@rd.nestle.com", caller_id))
chinese_data <- incident_data %>% filter(lang %in% c("slovenian-iso8859_2", "slovak-ascii", "czech-iso8859_2", "nepali"))
cyrillic_data <- incident_data %>% filter(lang %in% c("russian-koi8_r", "russian-windows1251", "bulgarian-iso8859_5", "ukrainian-koi8_r"))
other_data <- incident_data %>% filter(lang %in% c("serbian-ascii", "hungarian", "malay", "croatian-ascii", "tagalog", "lithuanian", "hebrew-iso8859_8", "middle_frisian", "estonia", "turkish", "sanskrit", "albanian", "swahili"))
latin_data <- latin_data[-c(2993, 6980,10399,10769,18531,29562,30447,30932,32608,35074,37623,37719,
                            41284,41707,41716,42162,42593,43014, 42227,42228,47805,52588,61003,
                            62264,62498,65017,72448), ]
print(latin_data)
df<-latin_data


#DATA CLENING FINISHED
























































# Create document term matrix
# dtm <- DocumentTermMatrix(Corpus(VectorSource(df$words[1:40000])),
#                           control = list(minWordLength = 3, tolower = TRUE))
#DONE
# df$words <-sample(df$words, 73997)
dtm <- DocumentTermMatrix(df$words)

# Preprocess data
#dtm <- removeSparseTerms(dtm, 0.99)

# Perform hierarchical clustering
#hc <- hclust(dist(dtm_sampled))

# Visualize clustering
#df <- data.frame(x = hc$merge[,1], y = hc$merge[,2])
#ggplot(df, aes(x = x, y = y)) + 
  #  geom_segment(aes(xend = x, yend = y + 0.1)) + 
  #  geom_point(size = 2) +  ggtitle("Hierarchical clustering")
#cluster_assignments <- hc$cluster
# file_list <- list.files("VScode_python/")
# output_df<-df
# output_df <- data.frame(file = file_list, cluster = cluster_assignments)
# write.csv(output_df, "VScode_python/output.csv", row.names = FALSE)


# # Perform clustering using K-means
# set.seed(123)
# k <- 3 # Number of clusters
# km <- kmeans(x = dtm, centers = k)
# cluster_assignments <- km$cluster
# file_list <- list.files("VScode_python/")
# # Read the files
# file_list <- list.files("VScode_python/")
# text_data <- lapply(file_list, function(file) readLines(file))
# cluster_assignments <- km$cluster

# Export the clustered information
# output_df <- data.frame(file = file_list, cluster = cluster_assignments)
# write.csv(output_df, "path/to/output.csv", row.names = FALSE)
# 
# write.csv(output_df, "VScode_python/output.csv", row.names = FALSE)


# # Visualize clustering
# df<-df
# df <- df(x = km$centers[,1], y = km$centers[,2], cluster = factor(1:k))
# ggplot(df, aes(x = x, y = y, color = cluster)) + 
#   geom_point(size = 4) +  ggtitle("K-means clustering (k=3)")
# 



#Another one trial
# Perform clustering
#k <- 3 # Number of clusters
#km <- kmeans(dtm, centers = k)
#cluster_assignments <- km$cluster
#result<-latin_data[1:73997,c(1,3,5,8)]
# Export the clustered information
#output_df <- data.frame(latin_data[1:73997,0], cluster = cluster_assignments)
#write.csv(output_df, "Downloads/output.csv", row.names = FALSE)
#write.csv(result, "Downloads/inc_res.csv", row.names = FALSE)

#MERGE THEM IN ONE BIG OUTPUT WITH CLUSTERI
#df1<-result
#df2<-output_df
#merged_df <- merge(df1, df2)
#write.csv(merged_df, "merged_file.csv", row.names = FALSE)




# Merge the data frames
#sampled_rows <- sample(nrow(result), 7000)
#sampled_df <- result[sampled_rows,7000 ]
#sampled_output_df <- output_df[sampled_rows, ]


#aaa <- merge(sampled_df, sampled_output_df)

#View(aaa)
