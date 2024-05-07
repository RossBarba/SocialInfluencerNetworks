library(igraph)
library(signnet)
library(dplyr)
library(tidyverse)
library(stringr)
library(psych)

##########################PART 1: Pre-processing#################################

setwd("C:/Users/barba/OneDrive/Documents/MSBA2/IDS_564/FinalProject")

#Import all related instagram data from kaggle
instagram <- read.csv("social media influencers - instagram.csv", header=TRUE)
instagramJun <- read.csv("social media influencers-instagram june 2022 - june 2022.csv", header=TRUE)
instagramSep <- read.csv("social media influencers - instagram sep-2022.csv", header=TRUE)
instagramNov <- read.csv("social media influencers-instagram - -nov 2022.csv", header=TRUE)
instagramDec <- read.csv("social media influencers-INSTAGRAM - -DEC 2022.csv", header=TRUE)

#Creating pricing schedule fnction from example data to derive price per post feature for influencers
insta.pricing.sched = data.frame(
  Followers = c(5000, 20000, 100000, 1000000),
  costPerPost = c(41, 85, 296, 1804)
)

#youtube pricing sched link: https://www.shopify.com/blog/influencer-pricing
plot(insta.pricing.sched$Followers, insta.pricing.sched$costPerPost, 
     main = "Cost per Instagram Promotion",
     xlab = "Followers",
     ylab = "Cost",
     pch = 19,
     col = "blue")

#This is proably the only insta model I will ever work with
insta.model <- lm(costPerPost ~ Followers, data = insta.pricing.sched)
insta.coef <- coef(insta.model)

#Test code
#transformed_df$estCostPerPost <- coef(insta.model)[1] + coef(insta.model)[2] * transformed_df$Followers

yout.pricing.sched = data.frame(
  Followers=c(50000, 100000, 500000, 1000000),
  costPerVideo=c(200, 5000, 10000, 20000)
)

plot(yout.pricing.sched$Followers, yout.pricing.sched$costPerVideo, 
     main = "Cost per YouTube Promotion",
     xlab = "Subscribers",
     ylab = "Cost",
     pch = 19,
     col = "red")

yout.model <- lm(costPerVideo ~ Followers, data = yout.pricing.sched)
yout.coef <- coef(yout.model)


#Rename columns so cleaning function works later
instagram <- instagram %>%
  rename(influencer_name = Influencer.insta.name,
         country = Audience.country.mostly.,
         auth_engagement_avg = Authentic.engagement.,
         engagement_avg = Engagement.avg.)
instagramJun <- instagramJun %>%
  rename(influencer_name = instagram.name,
         Followers = followers,
         category_1 = Category_1,
         category_2 = Category_2,
         auth_engagement_avg = Eng...Auth..,
         engagement_avg = Eng...Avg..)
instagramSep <- instagramSep %>%
  rename(influencer_name = Instagram.name,
         Followers = Subscribers,
         category_1 = Category_1,
         category_2 = Category_2,
         country = Audience.country,
         auth_engagement_avg = Authentic.engagement.,
         engagement_avg = Engagement.average.)
instagramNov <- instagramNov %>%
  rename(influencer_name = Name,
         Followers = Followers,
         category_1 = Category.1,
         category_2 = Category.2,
         country = X.Country,
         auth_engagement_avg = Eng...Auth..,
         engagement_avg = Eng...Avg..)
instagramDec <- instagramDec %>%
  rename(influencer_name = name,
         Followers = followers,
         category_1 = Category_1,
         category_2 = Category_2,
         auth_engagement_avg = Eng...Auth..,
         engagement_avg = Eng...Avg..)

#Import YouTube related data
youtube <- read.csv("social media influencers - youtube.csv", header = TRUE) #This is the only df that only has one category column
youtubeJun <- read.csv("social media influencers-youtube june 2022 - june 2022.csv", header=TRUE)
youtubeSep <- read.csv("social media influencers - Youtube sep-2022.csv", header=TRUE)
youtubeNov <- read.csv("social media influencers-youtube - --nov 2022.csv", header=TRUE)
youtubeDec <- read.csv("social media influencers-YOUTUBE - --DEC 2022.csv", header=TRUE)

#Rename columns so cleaning function works later
youtubeJun <- youtubeJun %>%
  rename(influencer_name = channel.name,
         Followers = Subscribers.count,
         category_1 = Category,
         category_2 = Category_2,
         country = Country,
         avg_views = Views.avg.,
         avg_likes = Likes.avg,
         avg_comments = Comments.avg.)
youtubeSep <- youtubeSep %>%
  rename(influencer_name = Youtuber,
         Followers = Subscribers,
         category_1 = Category_2,
         category_2 = Category_3,
         country = Country,
         avg_views = Avg..views.,
         avg_likes = Avg..likes,
         avg_comments = Avg.Comments)
youtubeNov <- youtubeNov %>%
  rename(influencer_name = Youtube.channel,
         Followers = Followers,
         category_1 = Category,
         category_2 = Category.2,
         country = Country,
         avg_views = X.Views..Avg..,
         avg_likes = Likes..Avg..,
         avg_comments = Comments..Avg..)
youtubeDec <- youtubeDec %>%
  rename(influencer_name = Youtube.channel,
         Followers = Followers,
         category_1 = Category,
         category_2 = Category.2,
         country = Country,
         avg_views = X.Views..Avg..,
         avg_likes = Likes..Avg..,
         avg_comments = Comments..Avg..)

#Function to transform string encodings of numbers to actual numbers
# Function to convert '56.6M' or '112.9k' into numeric values
convert_numeric_impute <- function(x) {
  
  x <- tolower(x)
  # Match and separate the numeric and letter parts
  nums <- as.numeric(str_extract(x, "[0-9.]+"))
  suffixes <- str_extract(x, "[mk]$")
  
  # Multiply by the appropriate factor depending on the suffix
  multiplier <- case_when(
    suffixes == "m" ~ 1e6,
    suffixes == "k" ~ 1e3,
    TRUE ~ 1
  )
  
  # Replace NAs in nums with 0 (imputes erroneous or non-numeric strings and nulls with 0)
  nums <- ifelse(is.na(nums), 0, nums)
  
  return(nums * multiplier)
}

# Function to transform and predict
# Function to transform and predict
transform_and_predict <- function(df, model, num_columns, new_col_name) {
  # Convert specified numeric columns
  df <- df %>%
    mutate(across(all_of(num_columns), ~convert_numeric_impute(.)))
  
  # Predict using the provided model and add as a new column
  df[[new_col_name]] <- predict(model, newdata = df)
  
  return(df)
}
#Final cleaning for insta csv files
instagramDec <- transform_and_predict(instagramDec, insta.model, c("Followers", "auth_engagement_avg", "engagement_avg"), "estCostPerPost")
instagramDec <- instagramDec %>%
  filter(!is.na(auth_engagement_avg) & auth_engagement_avg != 0 & auth_engagement_avg != "") %>%
  filter(!is.na(estCostPerPost) & estCostPerPost != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerPost / auth_engagement_avg) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
instagramJun <- transform_and_predict(instagramJun, insta.model, c("Followers", "auth_engagement_avg", "engagement_avg"), "estCostPerPost")
instagramJun <- instagramJun %>%
  filter(!is.na(auth_engagement_avg) & auth_engagement_avg != 0 & auth_engagement_avg != "") %>%
  filter(!is.na(estCostPerPost) & estCostPerPost != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerPost / auth_engagement_avg) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
instagramNov <- transform_and_predict(instagramNov, insta.model, c("Followers", "auth_engagement_avg", "engagement_avg"), "estCostPerPost")
instagramNov <- instagramNov %>%
  filter(!is.na(auth_engagement_avg) & auth_engagement_avg != 0 & auth_engagement_avg != "") %>%
  filter(!is.na(estCostPerPost) & estCostPerPost != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerPost / auth_engagement_avg) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
instagramSep <- transform_and_predict(instagramSep, insta.model, c("Followers", "auth_engagement_avg", "engagement_avg"), "estCostPerPost")
instagramSep <- instagramSep %>%
  filter(!is.na(auth_engagement_avg) & auth_engagement_avg != 0 & auth_engagement_avg != "") %>%
  filter(!is.na(estCostPerPost) & estCostPerPost != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerPost / auth_engagement_avg) %>%
  mutate(CPI_inverse = 1 / costPerImpression)

#Final cleaning for youtube files
youtubeDec <- transform_and_predict(youtubeDec, yout.model, c("Followers", "avg_views", "avg_likes", "avg_comments"), "estCostPerVideo")
youtubeDec <- youtubeDec %>%
  filter(!is.na(avg_views) & avg_views != 0 & avg_views != "") %>%
  filter(!is.na(estCostPerVideo) & estCostPerVideo != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerVideo / avg_views) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
youtubeJun <- transform_and_predict(youtubeJun, yout.model, c("Followers", "avg_views", "avg_likes", "avg_comments"), "estCostPerVideo")
youtubeJun <- youtubeJun %>%
  filter(!is.na(avg_views) & avg_views != 0 & avg_views != "") %>%
  filter(!is.na(estCostPerVideo) & estCostPerVideo != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerVideo / avg_views) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
youtubeNov <- transform_and_predict(youtubeNov, yout.model, c("Followers", "avg_views", "avg_likes", "avg_comments"), "estCostPerVideo")
youtubeNov <- youtubeNov %>%
  filter(!is.na(avg_views) & avg_views != 0 & avg_views != "") %>%
  filter(!is.na(estCostPerVideo) & estCostPerVideo != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerVideo / avg_views) %>%
  mutate(CPI_inverse = 1 / costPerImpression)
youtubeSep <- transform_and_predict(youtubeSep, yout.model, c("Followers", "avg_views", "avg_likes", "avg_comments"), "estCostPerVideo")
youtubeSep <- youtubeSep %>%
  filter(!is.na(avg_views) & avg_views != 0 & avg_views != "") %>%
  filter(!is.na(estCostPerVideo) & estCostPerVideo != "") %>%
  filter(!is.na(Followers) & Followers != "" & Followers > 0) %>%
  mutate(costPerImpression = estCostPerVideo / avg_views) %>%
  mutate(CPI_inverse = 1 / costPerImpression)

##############TOP INFLUENCERS#####################
describe(youtubeDec, fast=TRUE)
describe(instagramDec, fast=TRUE)


youtube_music <- youtubeDec %>%
  filter(category_1 == 'Music & Dance' | category_2 == 'Music & Dance') %>%
  arrange(costPerImpression) %>%
  slice_head(n = 10)
write.csv(youtube_music, "youtube_music.csv", row.names = FALSE)
insta_sports <- instagramDec %>%
  filter(category_1 == 'Sports with a ball' | category_2 == 'Sports with a ball') %>%
  arrange(costPerImpression) %>%
  slice_head(n = 10)  
write.csv(insta_sports, "insta_sports.csv", row.names = FALSE)

####################PART 2##############################################
generate_edge_list <- function(df, attribute_cols) {
  # Initialize an empty list to store edges
  edges <- vector("list", 0)
  
  # Define all columns needed for attributes, including the influencer name
  all_cols <- c("influencer_name", attribute_cols)
  
  # Initialize a data frame to store node attributes
  node_attributes <- df[ , all_cols, drop = FALSE]
  node_attributes <- unique(node_attributes)  # Make sure there are no duplicate rows
  
  # Loop through each row of the dataframe to create edges
  for (i in 1:nrow(df)) {
    # Check if 'category_1' is populated (not NA or blank)
    if (!is.na(df[['category_1']][i]) && df[['category_1']][i] != "") {
      edges <- c(edges, list(c(df[['influencer_name']][i], df[['category_1']][i])))
    }
    
    # Check if 'category_2' is populated (not NA or blank)
    if (!is.na(df[['category_2']][i]) && df[['category_2']][i] != "") {
      edges <- c(edges, list(c(df[['influencer_name']][i], df[['category_2']][i])))
    }
  }
  
  # Convert the edge list to a data frame
  edges_df <- do.call(rbind, edges)
  colnames(edges_df) <- c("from", "to")
  
  # Remove duplicate edges
  edges_df <- unique(edges_df)
  
  return(list(edges = edges_df, nodeAttributes = node_attributes))
}

instaAttrs <- c('Followers', 'auth_engagement_avg', 'engagement_avg', 'estCostPerPost', 'costPerImpression', 'CPI_inverse')
youtAttrs <- c('Followers', 'avg_views', 'avg_likes', 'avg_comments', 'estCostPerVideo', 'costPerImpression', 'CPI_inverse')

instaDecNet <- generate_edge_list(instagramDec, instaAttrs)
instaJunNet <- generate_edge_list(instagramJun, instaAttrs)
instaNovNet <- generate_edge_list(instagramNov, instaAttrs)
instaSepNet <- generate_edge_list(instagramSep, instaAttrs)

youtDecNet <- generate_edge_list(youtubeDec, youtAttrs)
youtJunNet <- generate_edge_list(youtubeJun, youtAttrs)
youtNovNet <- generate_edge_list(youtubeNov, youtAttrs)
youtSepNet <- generate_edge_list(youtubeSep, youtAttrs)

###########FUNCTIONIZATION OF ABOVE########################################## 
plot_network <- function(net_data, size_attr, legend = FALSE, title = "Network Graph") {
  net <- graph_from_data_frame(net_data[["edges"]], directed = FALSE)
  map <- bipartite_mapping(net)
  
  # Set node types based on bipartite mapping
  V(net)$type <- map$type
  
  # Categories are TRUE, People are FALSE
  num_categories <- sum(V(net)$type)  # Number of category nodes
  category_colors <- rainbow(num_categories)
  
  # Extract names for category nodes from the network
  category_names <- V(net)$name[V(net)$type]
  
  # Assign names to these colors corresponding to category node names
  names(category_colors) <- category_names
  
  # Create a color vector for all nodes
  vertex_colors <- rep("white", vcount(net))  # Default white for people
  vertex_colors[V(net)$type] <- category_colors  # Colors for categories
  
  # Prepare node attributes, ensuring unique names
  node_attributes <- net_data[["nodeAttributes"]]
  node_attributes <- node_attributes[!duplicated(node_attributes$influencer_name), ]
  
  # Set node sizes; scale 'size_attr' for people nodes, categories have fixed size
  if (size_attr %in% names(node_attributes) && !all(is.na(node_attributes[[size_attr]]))) {
    followers_scaled <- sqrt(node_attributes[[size_attr]])  # Scale size by sqrt of attribute
    size_factor <- max(followers_scaled, na.rm = TRUE) / 20
    node_sizes <- rep(15, vcount(net))  # Fixed size for categories
    node_sizes[!V(net)$type] <- followers_scaled / size_factor  # Adjusted sizes for people
  } else {
    node_sizes <- rep(15, vcount(net))  # Fixed sizes for categories if 'size_attr' is not available
    node_sizes[!V(net)$type] <- 10  # Default size for people if attribute not available
  }
  
  if (!legend) {
    # Plot without legend; labels on category nodes
    plot(net,
         vertex.color = vertex_colors,
         vertex.shape = ifelse(V(net)$type, "square", "circle"),
         vertex.size = node_sizes,
         vertex.label = ifelse(V(net)$type, V(net)$name, NA),
         main = title,
         vertex.label.cex = 0.5,
         vertex.label.color = "black")
  } else {
    # Plot with legend; no labels on nodes
    plot(net,
         vertex.color = vertex_colors,
         vertex.shape = ifelse(V(net)$type, "square", "circle"),
         vertex.size = node_sizes,
         vertex.label = NA,
         main = title,
         vertex.label.cex = 0.8,
         vertex.label.color = "black")
    
    # Adding a legend for category colors
    legend("bottomright",
           legend = names(category_colors),
           col = category_colors,
           pch = 20,
           title = "Categories",
           cex = 0.6,  # Text size in the legend
           pt.cex = 1.5)  # Point size in the legend
  }
}

#The different categories that can be used for node sizes are the following

#Instagram:
##Followers
##auth_engagement_avg
##engagement_avg
##estCostPerPost (from linear regression)
##costPerImpression (estCostPerPost / auth_engagement_avg)
##CPI_inverse (1 / estCostPerPost)

#Youtube
##Followers
##avg_views
##avg_likes
##avg_comments
##costPerImpression (estCostPerVideo / auth_engagement_avg) (big circles are bad)
##CPI_inverse (1 / estCostPerPost) (big circles are good)

plot_network(instaDecNet, "CPI_inverse", legend = FALSE, title = "Instagram Network - December CPI Inverse")
plot_network(youtDecNet, "CPI_inverse", legend = FALSE, title = "YouTube Network - December CPI Inverse")

