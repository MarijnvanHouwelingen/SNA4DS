## Load the data ####
setwd("~/School/SNA4DS/Project")
rm(list=ls())
# Loading in the data ####
stemwijzer_df <- read.csv("Stemwijzer_Data_4_11 - data_16_11.csv")

# Exploratory Data Analysis Basics ####
cols_to_convert <- 4:29  # Columns to convert
stemwijzer_df[cols_to_convert] <- lapply(stemwijzer_df[cols_to_convert], as.numeric) # Convert to numeric

statement_scores <- as.matrix(stemwijzer_df[, 5:29])
parties <- colnames(stemwijzer_df)[5:29]

#

matrix_of_agreement <- outer(parties, parties, 
                             Vectorize(function(x, y) {
                               agree_count <- sum(statement_scores[, x] == 2 & statement_scores[, y] == 2)
                               neutral_count <- sum(statement_scores[, x] == 1 & statement_scores[, y] == 1)
                               disagree_count <- sum(statement_scores[, x] == 0 & statement_scores[, y] == 0)
                               agree_and_disagree_sum <- agree_count + disagree_count + neutral_count
                               
                               # If x and y are the same party (on the diagonal), add 30 to account for all statements
                               if (x == y) {
                                 agree_and_disagree_sum <- 31
                               }
                               
                               agree_and_disagree_sum
                             }))

# Set diagonal elements to 0 (parties agreeing with themselves)

# Print the agreement matrix
print(matrix_of_agreement)
par(mar = c(3,3, 3, 8))
color_palette <- colorRampPalette(c("red", "lightgreen","darkgreen"))(n = 3)

parties_short <- parties
parties_short[c(19, 22, 23, 24)] <- c("PP/DGR","LP", "SvNLD", "NLmetPlan")
parties_long <- parties
parties_long[c(19, 22, 23, 24)] <- c("Piratenpartij...De.Groenen (PP/DGR)",
                                     "Libertaire.Partij (LP)",
                                     "Samen.voor.Nederland (SvNLD)",
                                     "Nederland.met.een.Plan (NLmetPlan)")

# Plot a heatmap of the agreement matrix
agreement_heatmap <- heatmap(
  matrix_of_agreement,
  col = colors,   # Use the custom color palette
  Rowv = NA,             # No row clustering
  Colv = NA,          # No column clustering
  margins = c(6,6),
  labRow = parties_long,
  labCol = parties_short,
  main = "Party Agreement Heatmap"
) 
legend("left", legend = c("0-14", "15-29", "30"), 
       fill = c("red", "lightgreen","darkgreen"), 
       title = "Agreements")


# Exploratory Data Analysis Specific (Big 5 + Categories) ####

# Categories
library(ggplot2)

ggplot(stemwijzer_df, aes(x = Tag, fill = Tag)) +
  geom_bar(stat = "count", color = "black") +
  labs(title = "Frequency of statements within Stemwijzer 2023", 
       x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Calculate Minimum (In a model where the diagonal is 30)
min(matrix_of_agreement) # 3 # BIJ1 - JA21 
# Calculate Maximum (In a model where the diagonal is 0)
max(matrix_of_agreement) # 25 # NSC - CDA // GL.PvdA - LEF // GL.PvdA - PvdD
# Summary
summary(matrix_of_agreement) 
### Highest Mean: NEDmePlan (17.08), 50Plus (16.8), NSC (16.56)
### Lowest Mean: Volt (14.04), JA21/SGP (14.68), VVD/BIJ1 (14.96)

### Highest Median: 17 (FvD, 50PLUS, BBB, NLmetPlan)
### Lowest Median: 13 (Volt)

# Remove 3 statements that are hard to classify as they are neither right-left ####
# 1. The own risk within health insurance should be abolished. (row 3)
# 2. The government should invest more in underground CO2 storage. (row 7)
# 3. There should be more nuclear power plants in the Netherlands. (row 10)

stemwijzer_df_clean <- stemwijzer_df[-c(3, 7, 10), ]

## Now 27 statements are included, with 9 out of them being 'right'

# Transforming the data into a network ####

# First, we want to transform the stemwijzer_df into a different format
# We chose for a long format, where each row contains a combination of two 
# parties. This will make it easier to create an edge list and compare
# whether two parties agree on a certain statement or not. 
library(tidyr)
library(tidyverse)
library(dplyr)
long_df <- stemwijzer_df_clean %>% pivot_longer(cols=colnames(.)[5:29],
                                                names_to='party',
                                                values_to='answer')

# Create unique pairs of political parties for each statement
pairs_df <- long_df %>%
  select(Statement, Abbreviation, Tag, Right) %>%
  distinct() %>%
  expand(Statement, Party1 = long_df$party, Party2 = long_df$party) %>%
  filter(Party1 != Party2) %>%
  filter(row_number(Party1) < row_number(Party2)) ## To ensure unique rows

# Merge the pairs with the original tibble to get the corresponding answer values
result_df <- pairs_df %>%
  left_join(long_df %>% select(Statement, party, answer), 
            by = c("Statement", "Party1" = "party")) %>%
  rename(Value_Party1 = answer) %>%
  left_join(long_df %>% select(Statement, party, answer), 
            by = c("Statement", "Party2" = "party")) %>%
  rename(Value_Party2 = answer) 

# Print the result dataframe
print(result_df)

statement_right <- stemwijzer_df %>%
  select(Statement, Right)
result_df <- left_join(result_df, statement_right, by = "Statement")

stemwijzer_long_df <- result_df %>%
  mutate(agreement = ifelse(Value_Party1 == Value_Party2, 1, 0)) %>%
  mutate(right_agreement = ifelse((Right == 1 & Value_Party1 == 2 & 
                                     Value_Party2 == 2), 1, 0)) %>%
  mutate(non_left_agreement = ifelse((Right == 0 & Value_Party1 == 0 & 
                                        Value_Party2 == 0), 1, 0))

# Calculate how often parties agree on right_wing statements ####
right_agreement_count <- stemwijzer_long_df %>% 
  filter(right_agreement == 1 | non_left_agreement == 1) %>%
  group_by(Party1, Party2) %>%
  summarise(count = n())

right_edge_list <- right_agreement_count %>%
  filter(count > 13) %>%
  select(Party1, Party2)


# Same approach for agreement count ####
colnames(matrix_of_agreement) <- colnames(stemwijzer_df)[5:29]

agreement_count <- as.data.frame(matrix_of_agreement) %>%
  mutate(party2 = colnames(.))

col_names <- colnames(agreement_count)
agreement_count <- agreement_count[, c(length(col_names), 1:(length(col_names)-1))]

counts_of_agreement <- agreement_count %>% 
  pivot_longer(cols=colnames(.)[2:26],names_to='party', values_to='count') %>%
  filter(party2 != party) %>%
  select(party2, party, count) %>%
  rowwise() %>%
  mutate(combined_parties = paste(sort(c(party, party2)), collapse = "_")) %>%
  arrange(combined_parties) %>%
  distinct(combined_parties, .keep_all = TRUE) %>%
  select(-combined_parties)

# min(counts_of_agreement$count) ## 3
# max(counts_of_agreement$count) ## 25
agreement_count_ntwrks <- counts_of_agreement %>%
  mutate(more_2 = ifelse(count > 2, 1, 0)) %>%
  mutate(more_10 = ifelse(count > 10, 1, 0)) %>%
  mutate(more_15 = ifelse(count > 15, 1, 0)) %>%
  mutate(more_20 = ifelse(count > 20, 1, 0))


# Creating Networks ####
node_attributes <- read.csv("node_attribute_partijen.csv", sep = ";")

# Transpose Node Attributes 
node_attributes <- as.data.frame(t(node_attributes))
colnames(node_attributes) <- node_attributes[1, ]
node_attributes <- node_attributes[-1, ]
node_attributes$Party <- rownames(node_attributes)

node_attributes[1:7] <- lapply(node_attributes[1:7], as.numeric)



# Create a NodeList (keeping it unique) and Join Node Attributes
NodeList <- unique(c(agreement_count_ntwrks$party2, agreement_count_ntwrks$party))
NodeList_df <- as.data.frame(NodeList)
colnames(NodeList_df)[1] <- 'Party'
NodeList_attributes <- NodeList_df %>% 
  left_join(node_attributes, by = 'Party') %>%
  mutate(zittend = ifelse((node_attributes$Seats_2021 > 0), 1, 0))

# Making the right network for the ERGM study 
right_network <- igraph::graph_from_data_frame(right_edge_list, 
                                               NodeList, directed = FALSE)
# Add the node attributes to the network 
snafun::add_vertex_attributes(right_network,colnames(NodeList_attributes),value = NodeList_attributes) 
snafun::g_summary(right_network, directed = FALSE)

# converting it into a network opject
right_network_netpackage <- snafun::to_network(right_network)
plot(right_network_netpackage)


# Adding the network attributes
summary(right_network_netpackage)

# adding the network attributes
right_network_netpackage <- snafun::add_vertex_attributes(right_network_netpackage,value = NodeList_attributes)

# defining the vertex name
right_network_netpackage <- snafun::add_vertex_names(right_network_netpackage,value = NodeList_attributes$Party)

# checking the network attribute
summary(right_network_netpackage)

# summarize the statistics
kstar <- summary(right_network_netpackage ~ kstar(1:12))
kstar/sum(kstar)
summary(right_network_netpackage ~ degree(1:10))
summary(ergm::ergm(right_network_netpackage ~ edges))
ergm1 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023") + nodecov("left_right") 
                    + kstar(1:6) + degree(1:6) + isolates(),
                    control = ergm::control.ergm(MCMC.burnin = 5000,
                                                 MCMC.samplesize = 15000, seed = 234567, MCMLE.maxit = 20,
                                                 parallel = 4, parallel.type = "PSOCK"
                    ))
plot(right_network_netpackage)
right_network_netpackage
snafun::extract_all_vertex_attributes(right_network_netpackage)
