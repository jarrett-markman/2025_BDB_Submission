library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
# Find all plays with no motion from the datasets
# read_tracking <- readRDS("tracking.rds") # Read in read_tracking function from code.R
# Read in all tracking data
# tracking <- bind_rows(map(1:9, read_tracking))
# Find the unique games in which no motion occurs
# no_motions <- tracking %>% 
#   group_by(gameId, playId) %>%
#   summarise(event) %>% 
#   distinct() %>%
#   # Use pivot_wider to widen the data by each distinct event
#   pivot_wider(id_cols = c(gameId, playId),
#               names_from = event, values_from = event) %>% 
#   filter(is.na(man_in_motion)) %>% # Filter the game and play when man in motion is NA (not present on the play)
#   select(gameId, playId) %>%
#   # Initialize the no motion col. for plays with no man_in_motion event
#   mutate(c1 = 1, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0) 
#saveRDS(no_motions, "no_motions.rds")
# Save no_motions as a .rds object so that the data takes up less storage
no_motions <- readRDS("no_motions.rds")
# Read in plays .csv
plays <- read_csv("plays.csv") %>% # Summarise cols. from plays 
  mutate(playType = ifelse(is.na(passResult), "rush", "pass")) %>%
  select(gameId, playId, possessionTeam, defensiveTeam, expectedPointsAdded, offenseFormation, pff_passCoverage, pff_manZone, playType)
# Read in results .rds file
res <- readRDS("results.rds")
# Create motions df with the unique motions on a play
# Gets the unique number of motions based on whatever motion(s) occur on that play
motions <- res %>% 
  group_by(gameId, playId) %>%
  mutate( 
    c1 = ifelse(cluster == 1, 1, 0), 
    c2 = ifelse(cluster == 2, 1, 0),
    c3 = ifelse(cluster == 3, 1, 0),
    c4 = ifelse(cluster == 4, 1, 0),
    c5 = ifelse(cluster == 5, 1, 0), 
    c6 = ifelse(cluster == 6, 1, 0)
  ) %>%
  select(-c(nflId, cluster)) %>%
  distinct() %>% 
  summarise(c1 = sum(c1), c2 = sum(c2), c3 = sum(c3), c4 = sum(c4), c5 = sum(c5), c6 = sum(c6)) %>% 
  ungroup()
motions <- bind_rows(motions, no_motions)
# Change colnames
motions <- motions %>%
  rename( # Label motion types
    "Motion across the field" = c6,
    "Quarter-field motions" = c4,
    "Half-field motions" = c3,
    "Quick shifts" = c2,
    "No motion" = c1,
    "Slow shifts" = c5
  )
# Join plays data frame on motions
motions_w_coverages <- motions %>%
  left_join(plays, by = c("gameId", "playId")) %>%
  mutate(
    pffCoverage = paste(pff_passCoverage, pff_manZone, sep = ", "),
    # Use gsub to change patter for manZone = Other, and NA plays
    pffCoverage = gsub(", Other", "", pffCoverage),
    pffCoverage = gsub("NA, NA", "Uncharted", pffCoverage)
  )
motions_w_coverages %>%
  group_by(offenseFormation, pffCoverage, playType,
           `No motion`, `Motion across the field`, `Quarter-field motions`, `Half-field motions`,
           `Quick shifts`, `Slow shifts`) %>%
  summarise(plays = n(), 
            avg_epa = round(mean(expectedPointsAdded), digits = 2)) %>% 
  ungroup() %>%
  distinct() -> df_by_play_type
motions_w_coverages %>%
  group_by(offenseFormation, pffCoverage,
           `No motion`, `Motion across the field`, `Quarter-field motions`, `Half-field motions`,
           `Quick shifts`, `Slow shifts`) %>%
  mutate(rush = ifelse(playType == "rush", 1, 0),
         rushes = sum(rush)) %>%
  summarise(plays = n(), 
            r_freq = round(rushes/plays, digits = 2),
            avg_epa = round(mean(expectedPointsAdded), digits = 2),
            epa_if_rush = round(r_freq * avg_epa, digits = 2),
            epa_if_pass = round((1-r_freq) * avg_epa, digits = 2)) %>% 
  ungroup() %>%
  distinct() -> df
formations_by_freq <- df %>%
  group_by(offenseFormation) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ungroup() %>%
  pull(offenseFormation)
# Build shiny ui 
ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(
    # Sidebar with filters (unchanged)
    checkboxGroupInput("motions", "Select Motion Types:", 
                       choices = c("No motion", "Motion across the field", "Quarter-field motions", "Half-field motions", "Quick shifts", "Slow shifts")),
    selectInput("formation", "Select Offensive Formation:", 
                choices = na.omit(formations_by_freq)),
    selectInput("playType", "Guess Play Type:", 
                choices = c("probabilities", "rush", "pass"))
  ),
  dashboardBody(
    # Add tabs for Main Page and References
    tabsetPanel(
      tabPanel("Main Page",  # Use plain text for the tab title
               fluidRow(
                 box(textOutput("plays")) # Display number of times that motion and formation occurred
               ),
               fluidRow(
                 box(textOutput("coverage")) # Coverage recommendation w/ lowest avg_epa
               ),
               br(),
               fluidRow(
                 (DTOutput("epa")) # DT with table for inputs/coverage epa w/ those inputs
               )
      ),
      tabPanel("References",  # Use plain text for the tab title
               tags$ul(
                 tags$li("Michael Lopez, Thompson Bliss, Ally Blake, Paul Mooney, and Addison Howard. NFL Big Data Bowl 2025. https://kaggle.com/competitions/nfl-big-data-bowl-2025, 2024. Kaggle."),
                 tags$li("The code used for this shiny app can be found via github.com/jarrett-markman/2025_BDB_Submission.")
               )
      )
    )
  )
)
# Build server
server <- function(input, output, session) {
  # Reactive value for filtered data
  filtered_df <- reactive({
    req(input$motions, input$formation, input$playType)  # Ensure inputs are provided
    # Create a data frame with motion inputs
    vec <- input$motions
    m_inputs <- data.frame(
      c1 = ifelse("No motion" %in% vec, 1, 0),
      c2 = ifelse("Quick shifts" %in% vec, 1, 0),
      c3 = ifelse("Half-field motions" %in% vec, 1, 0),
      c4 = ifelse("Quarter-field motions" %in% vec, 1, 0),
      c5 = ifelse("Slow shifts" %in% vec, 1, 0),
      c6 = ifelse("Motion across the field" %in% vec, 1, 0)
    )
    # Account for cases for playType display
    if (input$playType == "probabilities") {
      filtered_df <- df %>%
        filter(offenseFormation == input$formation & 
                 `Motion across the field` == m_inputs$c6 & `Quarter-field motions` == m_inputs$c4 & 
                 `Half-field motions` == m_inputs$c3 & `Quick shifts` == m_inputs$c2 & 
                 `No motion` == m_inputs$c1 & `Slow shifts` == m_inputs$c5) %>%
        select("Coverage (via PFF)" = pffCoverage, Plays = plays, "Rush Frequency" = r_freq, "EPA/Play" = avg_epa, "EPA/Play if Rush" = epa_if_rush, "EPA/Play if Pass" = epa_if_pass)
    } else {
      filtered_df <- df_by_play_type %>%
        filter(offenseFormation == input$formation & playType == input$playType & 
                 `Motion across the field` == m_inputs$c6 & `Quarter-field motions` == m_inputs$c4 & 
                 `Half-field motions` == m_inputs$c3 & `Quick shifts` == m_inputs$c2 & 
                 `No motion` == m_inputs$c1 & `Slow shifts` == m_inputs$c5) %>%
        arrange(avg_epa) %>%
        select("Coverage (via PFF)" = pffCoverage, Plays = plays, "Average EPA" = avg_epa)
    }
    
    return(filtered_df)
  })
  # Set server outputs
  output$plays <- renderText({paste("This motion and formation has occurred:", sum(filtered_df()$Plays), "times.")})
  output$coverage <- renderText({paste("The best coverage response based on the selected inputs is:",
                                       filtered_df() %>% slice(1) %>% pull(`Coverage (via PFF)`))})
  output$epa <- renderDT({
    filtered_df()
  })
}
# Deploy app
shinyApp(ui, server)
