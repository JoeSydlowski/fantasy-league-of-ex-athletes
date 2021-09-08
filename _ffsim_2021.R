
# Libraries ---------------------------------------------------------------

library(ffscrapr)
library(tidyverse)
library(ffsimulator)


# Load Data ---------------------------------------------------------------

flea_conn <- ff_connect(platform = "sleeper", league_id = "739986523688230912", season = 2021)

ff_sim <- ff_simulate(flea_conn, n_seasons = 10000, gp_model = "simple", verbose = TRUE, seed = 815)

png(file="plots/win_plot.png")
autoplot(ff_sim, "wins") + tantastic::theme_tantastic()
dev.off()

png(file="plots/rank_plot.png", width = 800, height = 600)
autoplot(ff_sim, "rank") + tantastic::theme_tantastic() + theme(legend.position="bottom")
dev.off()

png(file="plots/points_plot.png")
autoplot(ff_sim, "points") + tantastic::theme_tantastic()
dev.off()
