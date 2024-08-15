########################################
#
# Plot election cluster model results
#
########################################

library(ggplot2)
library(dplyr)
library(usmap)
library(scales)


####################
# Load data
####################
elections_state <- read.csv("data/state_election_history.csv")
elections_national <- read.csv("data/national_election_history.csv")

kmeans_clusters <- read.csv("data_derived/kmeans_clusters.csv")


####################
# Prep election data
####################
# Filter to elections we clustered (only Dem vs Rep elections)
elections_state <- 
  elections_state %>% 
  filter(election %in% kmeans_clusters$election) %>% 
  select(election, state, democratic, republican, third_party) %>% 
  mutate(margin = republican - democratic)

elections_national <- 
  elections_national %>% 
  filter(election %in% kmeans_clusters$election) %>% 
  select(election, democratic, republican, third_party) %>% 
  mutate(national_margin = republican - democratic)

# Add relative margin to state election results
elections_state <- elections_state %>% 
  merge(elections_national %>% select(election, national_margin), by = "election") %>% 
  mutate(relative_margin = margin - national_margin)


####################
# Plot K-Means results
####################
# Cluster 0
cluster0_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==0]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster0_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 1
cluster1_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==1]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster1_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 2
cluster2_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==2]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster2_states,
  values = "avg_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 3
cluster3_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==3]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster3_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "white"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 4
cluster4_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==4]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
    regions = "states",
    data = cluster4_states,
    values = "avg_relative_margin",
    linewidth = 0.6,
    color = "black"
  ) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 5
cluster5_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==5]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster5_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 6
cluster6_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==6]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster6_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 7
cluster7_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==7]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster7_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )

# Cluster 8
cluster8_states <- elections_state %>% 
  filter(election %in% kmeans_clusters$election[kmeans_clusters$cluster==8]) %>% 
  group_by(state) %>% 
  summarise(avg_margin = mean(margin),
            avg_relative_margin = mean(relative_margin))

plot_usmap(
  regions = "states",
  data = cluster8_states,
  values = "avg_relative_margin",
  linewidth = 0.6,
  color = "black"
) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    limits = c(-0.2, 0.2),
    oob = squish
  ) +
  theme(
    panel.background = element_blank()
  )




