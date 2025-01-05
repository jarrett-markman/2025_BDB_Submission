library(tidyverse)
library(cluster)
library(gganimate)
# Create a function to read in tracking data 
read_tracking <- function(wk) {
  # Read in tracking for a selected wk
  tracking <- read_csv(paste0("tracking_week_", wk, ".csv"))
  tracking <- tracking %>% 
    mutate(week = wk) %>%
    filter(frameType != 'AFTER_SNAP') # Remove observations post-snap
  # Add a new col. for the week
  return(tracking)
}
# Create a function to aggregate all data
create <- function() {
  # Read in csv's
  players <- read_csv("players.csv") %>%
    select(nflId, position) # Select nflId and position
  plays <- read_csv("plays.csv") %>% # Summarise cols. from plays 
    mutate(playType = ifelse(is.na(passResult), "rush", "pass")) %>%
    select(gameId, playId, possessionTeam, defensiveTeam, expectedPointsAdded, offenseFormation, pff_passCoverage, pff_manZone, playType)
  # Iterate over weeks 1-9 to read in all the data and bind rows to combine all tracking data
  tracking <- bind_rows(map(1:9, read_tracking)) %>%
    mutate(
      x = ifelse(playDirection == "left", 120 - x, x),
      y = ifelse(playDirection == "left", (160/3) - y, y)
    )
  # Get the games/plays that have man_in_motion as one of the events
  motion_plays <- tracking %>% 
    group_by(gameId, playId) %>%
    summarise(event) %>% 
    distinct() %>% 
    filter(event == "man_in_motion") %>%
    ungroup() %>%
    select(gameId, playId)
  # Create a football data frame that measures the football (los)
  football <- tracking %>%
    filter(displayName == "football" & frameId == 1) %>%  # Filter the football loc. and the first frame 
    # Assuming the first frame represents los
    select(gameId, playId, fb_x = x, fb_y = y, playDirection)
  # Create new tracking df
  tracking <- motion_plays %>% 
    left_join(tracking, by = c("gameId", "playId")) %>%
    group_by(gameId, playId) %>%
    # Get motion frame and ball snap
    mutate(first_frame = frameId[which(event == "man_in_motion")][1],
           last_frame = frameId[which(event == "ball_snap")][1]) %>%
    filter(frameId >= first_frame & frameId <= last_frame) # Filter frames from motion to snap
  # Left join data plays, player_play, and players
  data <- tracking %>% 
    left_join(plays, by = c("gameId", "playId")) %>% # Left join plays onto tracking_data via gameId and playId
    left_join(players, by = c("nflId")) %>% # Left join players on nflId
    left_join(football, by = c("gameId", "playId", "playDirection")) %>%    
    filter(position != "T" & position != "G" & position != "C" & position != "QB") %>% # Remove ol & qb positions (don't motion)
    return(data)
}
# Apply create() to create the working data frame data
data <- create()
#saveRDS(data, "data.rds")
data <- readRDS("data.rds")
model_data <- data %>%
  filter(club == possessionTeam) %>%
  group_by(gameId, playId, nflId) %>% # Group by game, play, and player
  arrange(frameId) %>% # Ensure frames are in ascending order
  # Get the first and last x and y coordinates at the man_in_motion frame
  mutate(
    x1 = x[which(event == "man_in_motion")][1],
    x2 = x[which(event == "ball_snap")][1],
    y1 = y[which(event == "man_in_motion")][1],
    y2 = y[which(event == "ball_snap")][1]
  ) %>%
  # Calculate the absolute value of the x and y coordinate change from "man_in_motion" to "ball_snap"
  mutate(x_diff = abs(x2 - x1), y_diff = abs(y2 - y1)) %>% 
  mutate(x_diff, y_diff, avg_speed = mean(s), avg_accel = mean(a), total_dist = sum(dis)) %>%
  ungroup() %>%
  select(gameId, playId, nflId, x_diff, y_diff, avg_speed, avg_accel, total_dist) %>% 
  distinct()
movements <- model_data %>% 
  select(-c(gameId, playId, nflId)) %>% 
  as.matrix()
# Set seed before running kmeans
set.seed(13210)
# Function to calculate the average silhouette score for a given k
find_best_k <- function(k) {
  cluster <- kmeans(movements, centers = k, nstart = 25)
  sil <- silhouette(cluster$cluster, dist(movements))
  avg_sil <- mean(sil[, "sil_width"])
  return(avg_sil)
}
# Create a sils data frame 
sils <- data.frame(
  k = 2:10,
  sil = 0
)
sils <- sils %>% 
  mutate(
    sil = map_dbl(k, find_best_k)
  )
silhouette_plot <- sils %>% 
  ggplot(aes(x = k, y = sil)) +
  geom_point() +
  geom_line() +
  # Add a highlighted circle
  geom_point(
    data = filter(sils, k == 6),
    aes(x = k, y = sil),
    shape = 1,        
    size = 10,
    stroke = 2,
    color = "red",  
    fill = "white"
  ) +
  labs(
    x = "K",
    y = "Average Silhouette Score",
    title = "Silhouette Score based on the Number of Clusters",
    caption = "2025 Big Data Bowl"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )
silhouette_plot # highest value is at k = 2 but it hits a local maximum at k = 6, which indicates it's a good k
cluster <- kmeans(movements, centers = 6, nstart = 25) # Set parameters for clusters 
cluster_labels <- cluster$cluster # Get cluster labels
# Plot the distribution of clusters
clusters <- cluster$cluster %>% 
  as.data.frame() %>% 
  summarise(cluster = as.factor(.))
clusters %>%
  filter(cluster != 1) %>% # No motion cluster
  ggplot() +
  geom_histogram(stat = "count", aes(x = cluster)) +
  labs(
    x = "Cluster", y = "Count",
    title = "Distribution of Model Clusters",
    subtitle = paste(length(clusters[clusters$cluster == 1, ]), 'Observations in "Cluster 1" - no motion'),
    caption = "2025 Big Data Bowl"
  ) + 
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5,),
        plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5)) -> motion_dist
# Create sequences df ordered by frameId
sequences <- data %>% 
  filter(club == possessionTeam) %>%
  group_by(gameId, playId, nflId) %>% # Group by game, play, and player
  arrange(frameId) %>%
  distinct(gameId, playId, nflId) %>%
  ungroup()
# Cbind sequences with cluster results
res <- cbind(sequences, cluster = cluster_labels)
saveRDS(res, "results.rds")
res <- readRDS("shiny/results.rds")
tracking <- read_tracking(1) %>% # Read in week 1 tracking data
  filter(gameId == 2022090800) # Filter gameId (first game of the NFL season)
# Read in plays w/ team data and yardsToGo
plays <- read_csv("plays.csv") %>% 
  select(gameId, playId, possessionTeam, defensiveTeam, yardsToGo)
df <- tracking %>%
  left_join(plays, by = c("gameId", "playId"))
# Create a function for an animation for a specific cluster 
create_animation <- function(cluster) {
  motion_type <- cluster$cluster
  ex <- df %>% 
    filter(gameId == cluster$gameId & playId == cluster$playId) %>%
    mutate(first_frame = frameId[which(event == "man_in_motion")][1],
           last_frame = frameId[which(event == "ball_snap")][1]) %>%
    filter(frameId >= first_frame & frameId <= last_frame) %>%
    mutate(
      pt_size = ifelse(club == "football", 2.7, 5.4),
      pt_fill_order = case_when(
        club == defensiveTeam ~ "2",
        club == possessionTeam & nflId == cluster$nflId ~ "3",
        club == possessionTeam & nflId != cluster$nflId ~ "1",
        club == "football" ~ "4"
      )
    )
  # Get team colors
  colors <- nflfastR::teams_colors_logos %>%
    select(team_abbr, team_color, team_color2)
  team_colors <- ex %>% 
    distinct(possessionTeam, defensiveTeam) %>%
    left_join(colors, by = c("possessionTeam" = "team_abbr")) %>%
    left_join(colors, by = c("defensiveTeam" = "team_abbr")) %>%
    rename(
      off_color = team_color.x, off_color2 = team_color2.x,
      def_color = team_color.y, def_color2 = team_color2.y
    )
  club <- ex %>% 
    filter(club != "football")
  football <- ex %>%
    filter(club == "football")
  path <- ex %>%
    filter(nflId == cluster$nflId) %>%
    mutate(motion_frame = frameId[which(event == "man_in_motion")][1],
           snap_frame = frameId[which(event == "ball_snap")][1]) %>% 
    mutate(motion_color = "black",
           motion_linetype = "solid",
           motion_linewidth = 1)
  motion <- path %>%
    filter(frameId >= motion_frame & frameId <= snap_frame) %>% 
    mutate(motion_color = "black",
           motion_linetype = "solid",
           motion_linewidth = 1)
  # los
  togo <- ifelse(ex$playDirection == "left", -1 * (ex$yardsToGo), ex$yardsToGo)
  # Set colors
  off_color <- ifelse(team_colors$off_color == team_colors$def_color, team_colors$off_color2, team_colors$off_color)
  def_color <- ifelse(team_colors$off_color == team_colors$def_color, team_colors$def_color, team_colors$def_color2)
  anim <- ggplot() +
    # Field
    annotate("rect",
             xmin = 160/3,
             xmax = 0,
             ymin = 10,
             ymax = 100,
             fill = scales::alpha("#21ae5f", 0.9)) +
    # Hashmarks
    annotate("text", 
             y = seq(20, 60, 10),
             x = 10,
             color = "white",
             family = "Chivo",
             label = seq(10, 50, 10),
             size = 6,
             angle = 90) +
    annotate("text", 
             y = seq(20, 60, 10),
             x = 40,
             color = "white",
             family = "Chivo",
             label = seq(10, 50, 10),
             size = 6,
             angle = 270) +
    annotate("text", 
             y = setdiff(seq(0, 100, 1), seq(0, 100, 10)),
             x = 160/3,
             color = "white",
             label = "—") +
    annotate("text", 
             y = setdiff(seq(0, 100, 1), seq(0, 100, 10)),
             x = 0,
             color = "white",
             label = "—") +
    annotate("text", 
             y = setdiff(seq(0, 100, 1), seq(0, 100, 10)),
             x = 23.36667,
             color = "white",
             size = 3,
             label = "—") +
    annotate("text", 
             y = setdiff(seq(0, 100, 1), seq(0, 100, 10)),
             x = 29.96667,
             color = "white",
             size = 3,
             label = "—") +
    annotate("segment", 
             y = 15,
             yend = 65,
             x = c(160/3, 0),
             xend = c(160/3, 0),
             color = "white") +
    geom_hline(yintercept = seq(0, 100, 10), color = "white") +
    annotate("segment", 
             y = football$x[1] + togo,
             yend = football$x[1] + togo,
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "#FDE725") +
    annotate("segment", 
             y = football$x[1],
             yend = football$x[1],
             x = 0,
             xend = 160/3,
             size = 1.5,
             color = "midnightblue") +
    geom_point(data = club, 
               aes(y, x, fill = pt_fill_order, size = pt_size, group = nflId), shape = 21) +
    geom_point(data = football, 
               aes(y, x, size = pt_size, group = nflId), fill = "#654321", shape = 21) +
    geom_line(data = motion,
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    geom_line(data = path, 
              aes(y, x, color = motion_color, linetype = motion_linetype, linewidth = motion_linewidth)) +
    scale_fill_manual(values = c(off_color, def_color, "white"),
                      labels = c(unique(ex$possessionTeam), unique(ex$defensiveTeam), unique(paste0(path$displayName, " (Cluster ", motion_type, ")")))) +
    scale_size_identity() +
    scale_color_identity() +
    scale_linetype_identity() +
    scale_linewidth_identity() +
    scale_x_reverse() +
    ylim(10, 100) + 
    labs(title = paste(ex$possessionTeam, "(OFF) v", ex$defensiveTeam, "(DEF)"),
         x = "", y = "",
         fill = NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 10.5),
          axis.text = element_blank(), axis.ticks = element_blank()) +
    transition_time(frameId) +
    transition_reveal(frameId) +
    ease_aes("linear") +
    guides(fill = guide_legend(override.aes = list(size = 5.4))) 
  return(anim)
}
# Filter same gameId as tracking
gm <- res %>% filter(gameId == 2022090800)
# Get first observation for each n cluster
get_cluster <- function(n) {
  c <- gm %>% filter(cluster == n) %>% slice(1)
  return(c)
}
# Apply animation and cluster functions to identify the unique motion types
motion1 <- create_animation(gm %>% filter(cluster == 1) %>% slice(2)) # No motion, better example
motion2 <- create_animation(get_cluster(2)) # Better example, quick shifts (typically horizontal)
motion3 <- create_animation(get_cluster(3)) # Half-field motions (typically include slot-motions to the other side of line or motions out wide or into the backfield)
motion4 <- create_animation(gm %>% filter(cluster == 4) %>% slice(4)) # Quarter-field motions (typically include TE motions across the line, shorter motions out wide/into the backfield)
motion5 <- create_animation(get_cluster(5)) # Slow shifts (typically vertical)
motion6 <- create_animation(get_cluster(6)) # Motions across the field
# Look at res clusters along with model_data to find what each cluster looks like:
library(gt)
model_data %>% 
  inner_join(res, by = c("gameId", "playId", "nflId")) %>%
  mutate(
    cluster = ifelse(cluster == 1, "No motion", ifelse(
      cluster == 2, "Quick shifts", ifelse(
        cluster == 3, "Half-field motions", ifelse(
          cluster == 4, "Quarter-field motions", ifelse(
            cluster == 5, "Slow shifts", ifelse(
              cluster == 6, "Motions across the field", cluster
            )
          )
        )
      )
    ))
  ) %>%
  select(-c(gameId, playId, nflId)) %>%
  group_by(cluster) %>%
  mutate(n = n()) %>%
  summarise_all(., mean) %>%
  group_by(cluster) %>%
  summarise_all(., round, digits = 2) %>%
  ungroup() %>%
  mutate(plays = sum(n), freq = round(n/plays * 100, digits = 2)) %>%
  select(-c(n, plays)) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  data_color(columns = -cluster, colors = scales::col_numeric("Blues", domain = NULL)) %>%
  cols_label(
    cluster = "Motion Type",
    x_diff = "Average Change in X",
    y_diff = "Average Change in Y",
    avg_speed = "Average Player Speed",
    avg_accel = "Average Player Acceleration",
    total_dist = "Average Total Distance",
    freq = "Motion Frequency"
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(title = "Average Model Feature by Motion Type") %>%
  tab_source_note(
    source_note = "2025 Big Data Bowl"
  ) -> model_features
# Save images and animations
ggsave("Distribution of Motion Classes.png", motion_dist)
ggsave("Silhouette Scores.png", silhouette_plot)
gtsave(model_features, "Model Features.png")
anim_save("no motion.gif", motion1)
anim_save("quick shifts.gif", motion2)
anim_save("half-field motions.gif", motion3)
anim_save("quarter-field motions.gif", motion4)
anim_save("slow shifts.gif", motion5)
anim_save("motions across the field.gif", motion6)
