# 2025 Big Data Bowl Submission

Link to the competition: https://www.kaggle.com/competitions/nfl-big-data-bowl-2025

For our submission "showing your hand", we specifically focused on the distinct motions – or tells – that offenses use, and how defenses can “best respond”, based on the information presented to them based on the offensive formation and motion. We built a k-means cluster model to identify the unique motion types for each player on each play in each game pre-snap. We defined distinct motion types and used those results to build a shiny app that lets a user select the motion and formation by the offense (and the option to guess rush/pass) and look at the historical EPA/play by defensive coverage, to identify the best way(s) for defenses to respond based on the pre-snap information given by the offense.

Our full write-up can be found at: https://www.kaggle.com/code/jarrettmarkman/showing-your-hand

Our shiny app with motion types, formations, and run/pass inputs can be found at: https://jarrett-markman.shinyapps.io/bdb_2025/
