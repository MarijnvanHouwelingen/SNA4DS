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




# Edge list
EdgeList5 <- agreement_count_ntwrks %>%
  filter(count > 4) %>%
  select(party, party2)
EdgeList10 <- agreement_count_ntwrks %>%
    filter(count > 9) %>%
    select(party, party2)
EdgeList15 <- agreement_count_ntwrks %>%
    filter(count > 14) %>%
    select(party, party2)
EdgeList16 <- agreement_count_ntwrks %>%
  filter(count > 15) %>%
  select(party, party2)
EdgeList17 <- agreement_count_ntwrks %>%
  filter(count > 16) %>%
  select(party, party2)
EdgeList18 <- agreement_count_ntwrks %>%
  filter(count > 17) %>%
  select(party, party2)
EdgeList19 <- agreement_count_ntwrks %>%
  filter(count > 18) %>%
  select(party, party2)
EdgeList20 <- agreement_count_ntwrks %>%
    filter(count > 19) %>%
    select(party, party2)
EdgeList21 <- agreement_count_ntwrks %>%
  filter(count > 20) %>%
  select(party, party2)
EdgeList22 <- agreement_count_ntwrks %>%
  filter(count > 21) %>%
  select(party, party2)
EdgeList23 <- agreement_count_ntwrks %>%
  filter(count > 22) %>%
  select(party, party2)
EdgeList24 <- agreement_count_ntwrks %>%
  filter(count > 23) %>%
  select(party, party2)
EdgeList25 <- agreement_count_ntwrks %>%
  filter(count > 24) %>%
  select(party, party2)
# Visualizing networks ####

library(igraph)
library(snafun)
ntwrk5 <- igraph::graph_from_data_frame(EdgeList5, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk5)
ntwrk10 <- igraph::graph_from_data_frame(EdgeList10, NodeList, directed = FALSE)%>%
  to_network()
plot(ntwrk10)
ntwrk15 <- igraph::graph_from_data_frame(EdgeList15, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk15)
ntwrk16 <- igraph::graph_from_data_frame(EdgeList16, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk16)
ntwrk17 <- igraph::graph_from_data_frame(EdgeList17, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk17)
ntwrk18 <- igraph::graph_from_data_frame(EdgeList18, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk18)
ntwrk19 <- igraph::graph_from_data_frame(EdgeList19, NodeList, directed = FALSE) 
plot(ntwrk19)
ntwrk20 <- igraph::graph_from_data_frame(EdgeList20, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk20)
ntwrk21 <- igraph::graph_from_data_frame(EdgeList21, NodeList, directed = FALSE) 
plot(ntwrk21)
ntwrk22 <- igraph::graph_from_data_frame(EdgeList22, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk22)
ntwrk23 <- igraph::graph_from_data_frame(EdgeList23, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk23)
ntwrk24 <- igraph::graph_from_data_frame(EdgeList24, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk24)
ntwrk25 <- igraph::graph_from_data_frame(EdgeList25, NodeList, directed = FALSE) %>%
  to_network()
plot(ntwrk25)

# Summary Statistics of Networks ####
snafun::g_summary(ntwrk10)
snafun::plot_centralities(ntwrk20)
snafun::g_centralize(ntwrk20, measure = "betweenness")
snafun::v_eccentricity(ntwrk20)

# Right-Wing statement networks -- 20 EDGELISTS ####
Right_Edge_1 <- right_agreement_count %>%
  filter(count > 0) %>%
  select(Party1, Party2)
Right_Edge_2 <- right_agreement_count %>%
  filter(count > 1) %>%
  select(Party1, Party2)
Right_Edge_3 <- right_agreement_count %>%
  filter(count > 2) %>%
  select(Party1, Party2)
Right_Edge_4 <- right_agreement_count %>%
  filter(count > 3) %>%
  select(Party1, Party2)
Right_Edge_5 <- right_agreement_count %>%
  filter(count > 4) %>%
  select(Party1, Party2)
Right_Edge_6 <- right_agreement_count %>%
  filter(count > 5) %>%
  select(Party1, Party2)
Right_Edge_7 <- right_agreement_count %>%
  filter(count > 6) %>%
  select(Party1, Party2)
Right_Edge_8 <- right_agreement_count %>%
  filter(count > 7) %>%
  select(Party1, Party2)
Right_Edge_9 <- right_agreement_count %>%
  filter(count > 8) %>%
  select(Party1, Party2)
Right_Edge_10 <- right_agreement_count %>%
  filter(count > 9) %>%
  select(Party1, Party2)
Right_Edge_11 <- right_agreement_count %>%
  filter(count > 10) %>%
  select(Party1, Party2)
Right_Edge_12 <- right_agreement_count %>%
  filter(count > 11) %>%
  select(Party1, Party2)
Right_Edge_13 <- right_agreement_count %>%
  filter(count > 12) %>%
  select(Party1, Party2)
Right_Edge_14 <- right_agreement_count %>%
  filter(count > 13) %>%
  select(Party1, Party2)
Right_Edge_15 <- right_agreement_count %>%
  filter(count > 14) %>%
  select(Party1, Party2)
Right_Edge_16 <- right_agreement_count %>%
  filter(count > 15) %>%
  select(Party1, Party2)
Right_Edge_17 <- right_agreement_count %>%
  filter(count > 16) %>%
  select(Party1, Party2)
Right_Edge_18 <- right_agreement_count %>%
  filter(count > 17) %>%
  select(Party1, Party2)
Right_Edge_19 <- right_agreement_count %>%
  filter(count > 18) %>%
  select(Party1, Party2)
Right_Edge_20 <- right_agreement_count %>%
  filter(count > 19) %>%
  select(Party1, Party2)

right_network <- igraph::graph_from_data_frame(Right_Edge_15, 
                                               NodeList, directed = FALSE) %>%
  to_network()

agreement_network <- igraph::graph_from_data_frame(EdgeList20, 
                                                   NodeList, directed = FALSE) %>%
  to_network()



plot(agreement_network, vertex.color = "gold", vertex.label.dist = 0.1, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=1)

# CUG Tests ####
trans <- replicate(n = 2000,
                   snafun::create_random_graph(n_vertices = 25, 
                                               strategy = "gnm",
                                               m = 33,
                                               directed = FALSE,
                                               graph = "network") |> 
                     snafun::g_transitivity(),
                   simplify = TRUE
)
readr::write_rds(trans,"trans.rds")
## Transitivity ####
plot(density(trans), main = "Empirical transitivity distribution", 
     xlab = "Transitivity", ylab = "Density", xlim = c(0,0.6))
abline(v = snafun::g_transitivity(right_network), lty = "dashed", col = "red")
snafun::g_transitivity(right_network)


trans_a <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_transitivity(x)
}

cug_agreement_trans <- sna::cug.test(right_network, mode = "graph", 
                                     FUN = trans_a, 
                                     cmode = "edges", reps = 2000)
cug_agreement_trans

sna::plot.cug.test(cug_agreement_betw)


## Centrality measures ####
### Betweenness
library(sna)
library(snafun)
betw_a <- function(x, directed = FALSE){  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "betweenness", directed = directed)$centralization
}

cug_agreement_betw <- sna::cug.test(right_network, mode = "graph", FUN = betw_a, 
                                    cmode = "edges", 
                                    reps = 2000)
cug_agreement_betw

### Closeness
close_a <- function(x, directed = FALSE) { 
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "closeness", directed = directed)$centralization
}

cug_agreement_close <- sna::cug.test(agreement_network, mode = "graph", ## ntwrk16 significant
                                     FUN = close_a, 
                                     cmode = "edges", reps = 2000)
cug_agreement_close

### Degree
degree_a <- function(x, directed = FALSE) {  # note: directed = FALSE!
  x <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(x, measure = "degree", directed = directed)$centralization
}

cug_agreement_degree <- sna::cug.test(agreement_network, mode = "graph", FUN = degree_a, 
                                      cmode = "edges", 
                                      reps = 2000)
cug_agreement_degree


# Create a NodeList (keeping it unique) and Join Node Attributes
NodeList <- unique(c(agreement_count_ntwrks$party2, agreement_count_ntwrks$party))
NodeList_df <- as.data.frame(NodeList)
colnames(NodeList_df)[1] <- 'Party'
NodeList_attributes <- NodeList_df %>% 
  left_join(node_attributes, by = 'Party') %>%
  mutate(zittend = ifelse((node_attributes$Seats_2021 > 0), 1, 0)) %>% mutate(Age = 2023 - node_attributes$Year)

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
kstar
summary(right_network_netpackage ~degree(1:10))
plot(right_network_netpackage)
snafun::g_summary(right_network_netpackage)



# the exogenous models
m0 <- ergm::ergm(right_network_netpackage ~ edges)
readr::write_rds(m0,"ergm1.1.rds")
m1 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021"))
readr::write_rds(m1,"ergm1.2.rds")
m2 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2023"))
readr::write_rds(m2,"ergm1.3.rds")
m3 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Left_Right"))
readr::write_rds(m3,"ergm1.4.rds")
m4 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Age"))
readr::write_rds(m4,"ergm1.5.rds")
m5 <- ergm::ergm(right_network_netpackage ~ edges + nodefactor("is_coalition_2021"))
readr::write_rds(m5,"ergm1.6.rds")
m6 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023"))
readr::write_rds(m6,"ergm1.7.rds")
m7 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023") + nodecov("Left_Right"))
readr::write_rds(m7,"ergm1.8.rds")
m8 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023") + nodecov("Left_Right") + 
                   nodecov("Age"))
readr::write_rds(m8,"ergm1.9.rds")
m9 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023") + nodecov("Left_Right") + 
                   nodecov("Age") + nodefactor("is_coalition_2021"))
readr::write_rds(m8,"ergm1.10.rds")

# Get the summary of all models
 texreg::texreg(list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9))





# the final ergm
ergm1 <- ergm::ergm(right_network_netpackage ~ edges + nodecov("Seats_2021") + nodecov("Seats_2023") + nodecov("left_right") 
           + kstar(1:6) + degree(1:6) + isolates(),
           control = ergm::control.ergm(MCMC.burnin = 5000,
             MCMC.samplesize = 15000, seed = 234567, MCMLE.maxit = 20,
             parallel = 4, parallel.type = "PSOCK"
           ))
plot(right_network_netpackage)
right_network_netpackage
snafun::extract_all_vertex_attributes(right_network_netpackage)
