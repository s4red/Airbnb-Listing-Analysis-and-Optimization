#installing and loading the mongolite library to download the Airbnb data
install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
update.packages(ask = FALSE)


# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://skuqja:816wCLYEKgsWPWOg@cluster0.dsh7syd.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')

#2 writing an analytical query on the data::
mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')







#######################################################################################
########## GENERAL APPROACH FOR DIFFERENT BUSINESS insights ###########################
#######################################################################################
library(dplyr)
library(tidyr)
library(plotly)

# Combine text columns into one
airbnb_data <- airbnb_all %>%
  mutate(combined_text = paste(name, summary, space, description, sep = " "))

# Identifying common keywords
airbnb_data$keywords <- sapply(strsplit(as.character(airbnb_data$combined_text), " "), function(x) {
  keywords <- table(x)
  return(names(keywords[keywords == max(keywords)]))
})

# Let's split the combined text into words and calculate frequency
airbnb_data$word_list <- strsplit(as.character(airbnb_data$combined_text), "\\s+")
word_table <- table(unlist(airbnb_data$word_list))

# Convert to data frame and sort
keywords_df <- as.data.frame(word_table, stringsAsFactors = FALSE)
keywords_df <- keywords_df[order(-keywords_df$Freq), ]

# Remove stop words
stop_words <- stopwords("en")
keywords_df <- keywords_df[!keywords_df$Var1 %in% stop_words, ]

# Remove special characters and numbers
keywords_df <- keywords_df[!grepl("[^A-Za-z]", keywords_df$Var1), ]

# View the top non-stop words in the console
top_non_stop_words <- head(keywords_df, 20)

# Convert data to plotly format
plotly_data <- plot_ly(top_non_stop_words, x = ~reorder(Var1, Freq), y = ~Freq, type = 'bar', 
                       marker = list(color = ~Freq, colorscale = 'Blues')) %>%
  layout(xaxis = list(title = "Keyword"), 
         yaxis = list(title = "Frequency"),
         title = "Top 20 Non-Stop Words in Airbnb Listing Data")

# Display the interactive plot
plotly_data





#############################################################################################################
############################################### Ammenities #################################################
#############################################################################################################
# amenities is a list of amenities separated by commas

install.packages("ggiraph")
install.packages("viridis")
library(ggplot2)
library(ggiraph)
library(viridis)



# Flatten the amenities list to create a character vector
amenities_unlisted <- unlist(strsplit(as.character(airbnb_data$amenities), ",\\s*"))

# Remove  potential whitespace and convert to lowercase for consistency
amenities_cleaned <- tolower(trimws(amenities_unlisted))

# Create a frequency table of amenities
amenities_freq <- table(amenities_cleaned)

# Convert to a data frame for visualization, include only the top 10
top_amenities_df <- as.data.frame(sort(amenities_freq, decreasing = TRUE)[1:10])
names(top_amenities_df) <- c("amenity", "frequency")

# Plot the results with ggplot and ggiraph
gg <- ggplot(top_amenities_df, aes(x = reorder(amenity, frequency), y = frequency, tooltip = frequency)) +
  geom_bar_interactive(stat = "identity", fill = viridis(10)) +  # Using viridis color palette
  xlab("Amenity") +
  ylab("Frequency") +
  coord_flip() + # Flip the coordinates to make it easier to read long amenity names
  theme_minimal()

# make the  plot interactive
ggiraph(code = print(gg))

#############################################################################################################

#################################SENTIMENT ANALYSIS##############################################
#############################################################################################################

##########
#  create the combined_text column first
airbnb_data <- airbnb_data %>%
  mutate(combined_text = paste(name, summary, space, description, sep=" "))

library(tidytext)
library(dplyr)
library(tidyr)

# Retrieve the sentiment lexicon from the tidytext package
sentiments <- get_sentiments("bing")

# Perform sentiment analysis
airbnb_data_sentiment <- airbnb_data %>%
  unnest_tokens(word, combined_text) %>%
  inner_join(sentiments, by = "word") %>%
  group_by(listing_url) %>% # Replace with the correct identifier if necessary
  summarize(sentiment_score = sum(sentiment == "positive") - sum(sentiment == "negative"))

#  'listing_url'  the identifier, join the sentiment scores back to the airbnb_data
airbnb_data <- left_join(airbnb_data, airbnb_data_sentiment, by = "listing_url")



            







#############################################################################################################



#Pricing Strategy Insights:
#Correlate pricing information with text sentiments or keywords to understand what features might justify higher prices.

# Convert price to numeric if it's not already
airbnb_data <- airbnb_data %>%
  mutate(price_numeric = as.numeric(price))

# Calculate the correlation between price and sentiment_score
price_sentiment_correlation <- cor(airbnb_data$price_numeric, airbnb_data$sentiment_score, use = "complete.obs")

# Plot the relationship between price and sentiment score
ggplot(airbnb_data, aes(x = price_numeric, y = sentiment_score)) +
  geom_point(aes(color = sentiment_score > 0), alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Price") +
  ylab("Sentiment Score") +
  theme_minimal()

# Print out the correlation coefficient
print(price_sentiment_correlation)

############################Check the tokens associated with the different range of price ############################
################################################################################################################
# Install and load necessary libraries
install.packages("cld3")
install.packages("dplyr")
install.packages("tidytext")
install.packages("stringr")
library(cld3)
library(dplyr)
library(tidytext)
library(purrr)  
library(stringr)

# Function to detect language

detect_language <- function(text) {
  result <- cld3::detect_language(text)
  return(result)
}

# Filter out non-English text
english_data <- airbnb_data %>%
  filter(map_chr(combined_text, detect_language) == "en")


# Tokenize the English text
airbnb_words <- english_data %>%
  unnest_tokens(word, combined_text)

# Calculate word frequencies for each listing
word_counts <- airbnb_words %>%
  group_by(listing_url) %>%
  count(word) %>%
  ungroup()


# Now join this with the price data
word_counts <- word_counts %>%
  left_join(english_data %>% select(listing_url, price_numeric), by = "listing_url")

# Remove words that don't have at least two different counts across the listings
word_counts_filtered <- word_counts %>%
  group_by(word) %>%
  filter(n_distinct(n) > 1)

# Ensure there are no NA values in the price column before running correlation
word_counts_filtered <- word_counts_filtered %>%
  filter(!is.na(price_numeric))

# Filter out words with zero variability in either n or price_numeric
word_counts_filtered <- word_counts_filtered %>%
  group_by(word) %>%
  filter(sd(n) > 0 & sd(price_numeric) > 0) %>%
  ungroup()

# Calculate the correlation of each word with the price, using the filtered dataset
word_price_correlations <- word_counts_filtered %>%
  group_by(word) %>%
  summarize(correlation = cor(n, price_numeric, use = "complete.obs"), .groups = 'drop') %>%
  filter(!is.na(correlation)) %>%
  arrange(desc(correlation))


# Now filter for words with a high positive correlation
high_value_words <- word_price_correlations %>%
  filter(correlation > 0.2)  
############################ VIsualizing these tokens



library(ggplot2)
# Perform hierarchical clustering based on correlation coefficients
correlation_matrix <- outer(word_correlation_df$correlation, word_correlation_df$correlation, "-")
hclust_results <- hclust(as.dist(1 - correlation_matrix), method = "complete")

# Cut the dendrogram into clusters
num_clusters <- 5  
clusters <- cutree(hclust_results, k = num_clusters)

# Add cluster information to the data frame
word_correlation_df$cluster <- as.factor(clusters)

# Calculate mean correlation for each cluster
cluster_means <- word_correlation_df %>%
  group_by(cluster) %>%
  summarize(mean_correlation = mean(correlation))

# Create a bar plot for cluster means
ggplot(cluster_means, aes(x = reorder(factor(cluster), mean_correlation), y = mean_correlation)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(x = "Cluster", y = "Mean Correlation Coefficient", title = "Mean Correlation Coefficients by Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add cluster information to the original data frame
word_correlation_df$cluster <- as.factor(clusters)

# Print words in each cluster
for (i in 1:num_clusters) {
  cat("Cluster", i, ":\n")
  words_in_cluster <- word_correlation_df$word[word_correlation_df$cluster == i]
  print(words_in_cluster)
  cat("\n")
}


############################Identify and Investigate Outliers:
# Identify outliers using IQR
iqr <- IQR(airbnb_data$price_numeric)
upper_threshold <- quantile(airbnb_data$price_numeric, 0.75) + 1.5 * iqr
lower_threshold <- quantile(airbnb_data$price_numeric, 0.25) - 1.5 * iqr

# Add a column to flag outliers
airbnb_data <- airbnb_data %>%
  mutate(is_outlier = ifelse(price_numeric > upper_threshold | price_numeric < lower_threshold, TRUE, FALSE))

# Optionally, plot price with outliers highlighted
ggplot(airbnb_data, aes(x = price_numeric)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  geom_vline(xintercept = upper_threshold, color = "red", linetype = "dashed") +
  geom_vline(xintercept = lower_threshold, color = "red", linetype = "dashed") +
  labs(title = "Price Distribution with Outliers", x = "Price", y = "Count") +
  theme_minimal()

# Investigate outliers
outliers <- airbnb_data %>% filter(is_outlier)
View(outliers)

####################






#############################################################################################################
#####################Review Analysis:
########################### If you have review scores or number of reviews, you can analyze how textual sentiment correlates with these scores.
#############################################################################################################


# Since review_scores_rating is already a column in the review_scores data frame
airbnb_data$review_scores_rating <- as.numeric(airbnb_data$review_scores$review_scores_rating)

# Filter out NA values from both sentiment_score and review_scores_rating
airbnb_data <- airbnb_data %>%
  filter(!is.na(review_scores_rating) & !is.na(sentiment_score))

# Calculate the correlation between review scores and sentiment_score
review_sentiment_corr <- cor(airbnb_data$review_scores_rating, airbnb_data$sentiment_score, use = "complete.obs")

# Print the correlation coefficient
print(review_sentiment_corr)

# Visualize the relationship between review scores and sentiment score
ggplot(airbnb_data, aes(x = review_scores_rating, y = sentiment_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Review Score Rating") +
  ylab("Sentiment Score") +
  theme_minimal()



#############################################################################################################
#######################################################AFFINN NRC BING
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2) 


# Calculate AFINN sentiment scores

afinn_scores <- airbnb_data %>%
  unnest_tokens(word, combined_text) %>%
  inner_join(afinn, by = "word") %>%
  group_by(listing_url) %>%
  summarize(afinn_score = sum(value), .groups = "drop") %>%
  ungroup()



# NRC sentiment scores
nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative"))
nrc_scores <- airbnb_data %>%
  unnest_tokens(word, combined_text) %>%
  inner_join(nrc, by = "word") %>%
  group_by(listing_url) %>%
  summarize(nrc_score = sum(sentiment == "positive") - sum(sentiment == "negative")) %>%
  ungroup()

# Join AFINN and NRC scores back into the main dataframe
airbnb_data <- airbnb_data %>%
  left_join(afinn_scores, by = "listing_url") %>%
  left_join(nrc_scores, by = "listing_url")

# Define sentiment_score
airbnb_data <- airbnb_data %>%
  mutate(sentiment_score = case_when(
    !is.na(afinn_score) ~ afinn_score,
    !is.na(nrc_score) ~ nrc_score,
    TRUE ~ NA_real_
  ))


# Reshape the data for easier plotting
airbnb_long <- airbnb_data %>%
  select(listing_url, afinn_score, nrc_score, sentiment_score) %>%
  pivot_longer(cols = c(afinn_score, nrc_score, sentiment_score),
               names_to = "lexicon",
               values_to = "score") %>%
  mutate(lexicon = recode(lexicon, afinn_score = "AFINN", nrc_score = "NRC", sentiment_score = "Combined"))

# Boxplot for each lexicon
boxplot <- ggplot(airbnb_long, aes(x = lexicon, y = score, fill = lexicon)) +
  geom_boxplot() +
  scale_fill_manual(values = c("AFINN" = "steelblue", "NRC" = "purple", "Combined" = "blue")) +
  labs(x = "Lexicon", y = "Sentiment Score", title = "Boxplot of Sentiment Scores by Lexicon") +
  theme_minimal() +
  theme(legend.position = "none")

print(boxplot)

# Density plot for each lexicon
density_plot <- ggplot(airbnb_long, aes(x = score, fill = lexicon)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("AFINN" = "steelblue", "NRC" = "purple", "Combined" = "blue")) +
  labs(x = "Sentiment Score", y = "Density", title = "Density of Sentiment Scores by Lexicon") +
  theme_minimal() +
  theme(legend.position = "right")

print(density_plot)



###########################################################

################################Unique WORDS BY COUNTRY
###########################################################

library(dplyr)
library(tidytext)
library(ggplot2)

# Load stop words dataset
data(stop_words)


# Tokenize the text
words_by_country <- airbnb_data %>%
  unnest_tokens(word, combined_text) %>%
  filter(!word %in% stop_words$word)  # Remove stop words


# Count the frequency of each word by country
word_counts <- words_by_country %>%
  group_by(address_country = address$country, word) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(address_country, desc(count))

# Calculate TF-IDF scores
tfidf <- word_counts %>%
  bind_tf_idf(word, address_country, count) %>%
  arrange(desc(tf_idf))

# Select the top unique words for each country
top_unique_words <- tfidf %>%
  group_by(address_country) %>%
  top_n(30, tf_idf)

# Visualize the results
ggplot(top_unique_words, aes(x = reorder(word, tf_idf), y = tf_idf, fill = address_country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~address_country, scales = "free") +
  labs(x = "Word", y = "TF-IDF Score", title = "Most Unique Words by Country in Airbnb Listings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###########################################################quadrogram of most unique words

###########################################################
###########################################################
library(dplyr)
library(tidytext)
library(ggplot2)


# Tokenize the text and create four-grams
fourgrams_by_country <- airbnb_data %>%
  unnest_tokens(four_gram, combined_text, token = "ngrams", n = 4) %>%
  filter(!four_gram %in% stop_words$word)  # Remove stop words

# Count the frequency of each four-gram by country
fourgram_counts <- fourgrams_by_country %>%
  group_by(address_country = address$country, four_gram) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(address_country, desc(count))

# Calculate TF-IDF scores for four-grams
tfidf_fourgrams <- fourgram_counts %>%
  bind_tf_idf(four_gram, address_country, count) %>%
  arrange(desc(tf_idf))

# Select the top unique four-grams for each country
top_unique_fourgrams <- tfidf_fourgrams %>%
  group_by(address_country) %>%
  top_n(10, tf_idf)

# Visualize the results
ggplot(top_unique_fourgrams, aes(x = reorder(four_gram, tf_idf), y = tf_idf, fill = address_country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~address_country, scales = "free") +
  labs(x = "Four-gram", y = "TF-IDF Score", title = "Most Unique Four-grams by Country in Airbnb Listings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################ VISUALIZE THE BIGRAM ###########################################################

######################################################################################################################
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

# Tokenize the text and create bigrams
bigrams <- airbnb_data %>%
  unnest_tokens(bigram, combined_text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)  # Remove stop words

# Count the frequency of each bigram
bigram_counts <- bigrams %>%
  count(word1, word2, sort = TRUE)

# Visualize the bigram graph
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
############too messy and there are words that are not in english###############
library(dplyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
library(textdata)

# Load the English words dataset
data("words")

# Tokenize the text and create bigrams
bigrams <- airbnb_data %>%
  unnest_tokens(bigram, combined_text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word)  # Remove stop words

# Filter only the bigrams where both words are English
english_bigrams <- bigrams %>%
  filter(word1 %in% words$word & word2 %in% words$word)

# Count the frequency of each English bigram
english_bigram_counts <- english_bigrams %>%
  count(word1, word2, sort = TRUE)

# Visualize the English bigram graph
english_bigram_graph <- english_bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

ggraph(english_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)





######################################################
####### TF-IDF framework in Airbnb #######
######################################################
library(dplyr)
library(stringr)
library(tidytext)

# Unnest tokens, count words by country, and remove stopwords
airbnb_token <- airbnb_data %>%
  unnest_tokens(word, description) %>%  # Replace 'description' with your text column name
  count(address_country = address$country, word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  ungroup()

# Calculate total words by country
total_words <- airbnb_token %>%
  group_by(address_country ) %>%
  summarise(total = sum(n))

# Join the total words back and filter for significant countries
airbnb_words <- left_join(airbnb_token, total_words) %>%
  filter(address_country %in% c("United States", "Portugal", "Turkey"))  # Modify as per your dataset

# Calculate TF-IDF
airbnb_words <- airbnb_words %>%
  bind_tf_idf(word, address_country, n)

# Filter and arrange by TF-IDF values
airbnb_tfidf_top <- airbnb_words %>%
  arrange(desc(tf_idf)) %>%
  group_by(address_country) %>%
  top_n(15, tf_idf)

library(ggplot2)
# Plot the top TF-IDF words for each country
ggplot(airbnb_tfidf_top, aes(x = reorder(word, tf_idf), y = tf_idf, fill = address_country)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "TF-IDF") +
  facet_wrap(~address_country, ncol = 2, scales = "free") +
  coord_flip()  # Makes it easier to read the words


######################################
########## ZIPF's law ################
######################################



# Calculate term frequency and rank by country
freq_by_rank <- airbnb_words %>%
  group_by(address_country = address$country) %>%
  mutate(rank = row_number(),  # Rank words by frequency
         term_frequency = n / total) %>%
  arrange(address_country, desc(term_frequency))

# Plotting Zipf's Law
library(ggplot2)
freq_by_rank %>%
  ggplot(aes(x = rank, y = term_frequency, color = address_country)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = 'gray50', linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +  # Changed to show legend to distinguish countries
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Zipf's Law in Airbnb Listings' Text Data",
       x = "Log of Rank",
       y = "Log of Term Frequency",
       color = "Country") +
  theme_minimal()




######################################
########## Prediction Model ################
######################################
# Check if package is already installed, if not, install it
if (!requireNamespace("quanteda", quietly = TRUE)) {
  install.packages("quanteda")
}

library(quanteda)

# Combine text columns into one
airbnb_data$combined_text <- paste(airbnb_data$name, airbnb_data$summary, airbnb_data$description, sep = " ")

# Convert text to tokens
tokens_texts <- tokens(airbnb_data$combined_text, what = "word", tolower = TRUE)

# Remove stopwords
tokens_texts <- tokens_remove(tokens_texts, stopwords("en"))

# Create a document-feature matrix from the tokens
dfm_texts <- dfm(tokens_texts)

# Optionally,I had to do this again, remove punctuation and numbers if still needed
dfm_texts <- dfm_remove(dfm_texts, pattern = "\\p{P}", valuetype = "regex")
dfm_texts <- dfm_remove(dfm_texts, pattern = "\\d+", valuetype = "regex")

# My assumption is thta  if a listing has reviews, it's considered 'booked'
airbnb_data$booked <- as.factor(ifelse(airbnb_data$number_of_reviews > 0, 1, 0))

# Check the distribution of the target variable
print(table(airbnb_data$booked))

############# Split the Data
library(caret)

set.seed(123)

# Stratified sampling to maintain class ratio in both training and testing sets
train_indices <- createDataPartition(y = airbnb_data$booked, p = 0.8, list = FALSE)

dfm_train <- dfm_texts[train_indices, ]
dfm_test <- dfm_texts[-train_indices, ]
train_labels <- airbnb_data$booked[train_indices]
test_labels <- airbnb_data$booked[-train_indices]

# Convert 'booked' to a factor
train_labels <- as.factor(train_labels)
test_labels <- as.factor(test_labels)

# Check the distribution of the target variable in training and testing datasets
print(table(train_labels))
print(table(test_labels))

############# Train a Naive Bayes Model
library(quanteda.textmodels)

# Train the Naive Bayes model
nb_classifier <- textmodel_nb(dfm_train, y = train_labels)

# Check the model summary
summary(nb_classifier)

####################### Predict and Evaluate the Model
# Predict on the test set
predictions <- predict(nb_classifier, newdata = dfm_test)

# Evaluate the model using confusion matrix and other relevant metrics
confusion_matrix <- confusionMatrix(as.factor(predictions), test_labels)
print(confusion_matrix)

# Additional metrics
print(accuracy <- sum(predictions == test_labels) / length(test_labels))
