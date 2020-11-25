########################################
#
# Some analyses using the English data from https://github.com/jalapic/engsoccerdata
# for a particular football club
#
########################################
#
# Install packages
#
library(devtools)
#install_github('jalapic/engsoccerdata', 
#               username = "jalapic")
### install from source to get the most recent version of the data
library(engsoccerdata)
library(tidyverse)
library(ggpubr) # Arranging multiple ggplots
library(tidyquant) # We can perform moving averages
library(scales)#### scales for proportion plots ### 
#

#
data(package = "engsoccerdata")    # lists datasets currently available
#
########################################
col.tier<- rainbow(8)[c(1,2,6,5)] ###
### choose four contrasting colours to represent the four tiers
### red, gold, purple, lightblue

########################################
#
data(england, 
     package = "engsoccerdata")
#
#england
#
names(england)
head(england)
#
###########################################################
#
# ****** Plymouth Argyle Football Club (PAFC) ******
#
# List of clubs
#
with(england, sort(unique(c(home, visitor))))
#
# Show some clubs in alphabetical order,
# with their first and last season
#
england %>% 
  group_by(home) %>% # Group by club
  summarize(first = min(Season), # First year in season
            last = max(Season)) %>% # Last year in season
  filter(home %in% c("Plymouth Argyle", # Select some clubs of interest
                     "Exeter City",
                     "Portsmouth",
                     "Southampton",
                     "Manchester United",
                     "Wolverhampton Wanderers", "Arsenal"))
#
# Specify the club of interest for which graphs are required
#

football_plots<- function(club_of_interest = 'Plymouth Argyle',
                          club_colour = '#005045', save_plots = FALSE,
                          season_limits = c(1921, 2019))
{
  # season_limits can be changed 
  #club_of_interest <- "Plymouth Argyle"
  # Specify its colour for curve in graph
  ## club_colour <- "#005045" ### this is the PAFC shade of green according to
  # https://encycolorpedia.com/004237#:~:text=In%20the%20RGB%20color%20model,logo%20and%20Leicester%20Tigers%20logo.
  #
  # Get the data for the club of interest
  season_breaks<- c(1921, 1925, 1938, 1946, 1958, 1981, 1986, 1992, 2019)
  if(season_limits[1]< 1921) season_breaks<- c(season_limits[1], season_breaks)
  #
  my_club <- england %>%
    filter(home ==  club_of_interest | visitor == club_of_interest, 
           Season <= season_limits[2] & Season >= season_limits[1])
  #
  # Low scoring matches, high scoring matches, identify draws
  #
  my_club_2 <- my_club %>%
    mutate(one_or_zero = ifelse(totgoal <= 1, 1, 0), # 1 or 0
           two_one_or_zero = ifelse(totgoal <= 2, 1, 0), # 2, 1 or 0
           four_or_more = ifelse(totgoal >= 4, 1, 0), # 4 or more
           five_or_more = ifelse(totgoal >= 5, 1, 0), # 5 or more
           is_draw = ifelse(result == "D", 1, 0)) # Is it a draw?
  #
  # Summarize by season
  #
  my_club_summary <- my_club_2 %>%
    # filter(Season >= 1958) %>% # Use this line  to restric the season range
    group_by(Season) %>% # Results for each season
    summarize(tier = tier[1], # Same tier across all Seasons
              average_total = mean(totgoal), # Average number of goals
              one_or_zero_proportion = mean(one_or_zero), # Proportion of matches with 1 or 0 goals
              two_one_or_zero_proportion = mean(two_one_or_zero),
              four_or_more_proportion = mean(four_or_more),
              five_or_more_proportion = mean(five_or_more),
              draw_proportion = mean(is_draw), # Proportion of draws
              n = length(one_or_zero)) # Sample size
  #
  #my_club_summary
  # find the tiers in which the club has been
  tier_values <- as.character(sort( unique(my_club_summary$tier)))
  unique.tier<- as.numeric(tier_values)
  tier_colours <- col.tier[unique.tier]
  #
  # Put into the long format
  #
  my_club_summary_long <- my_club_summary %>%
    dplyr::select(-n) %>% # Ignore the column containing n
    pivot_longer(3:8, # Move the information in columns 3 to 8
                 names_to = "Type",
                 values_to = "Goals_proportion") %>%
    mutate(Type_f = # Re-define Type with nicer names
             factor(Type,
                    levels = c("average_total",
                               "draw_proportion",
                               "two_one_or_zero_proportion",
                               "one_or_zero_proportion",
                               "four_or_more_proportion",
                               "five_or_more_proportion"),
                    labels = c("Goals per match",
                               "Draws",
                               "2, 1 or 0 goals",
                               "1 or 0 goals",
                               "4 or more goals",
                               "5 or more goals")))
  #
  #my_club_summary_long
  #
  # ----------------------------------------------
  #
  # Plot the Goals per match
  #
  my_club_goals <- my_club_summary_long %>% 
    filter(Type_f == "Goals per match")
  
  
  my_club_goals$tier_colour<- col.tier[my_club_goals$tier] ### 
  labs.legend<- paste('Tier ', unique.tier, sep='')### 
  cols.legend<- col.tier[unique.tier] ### 
  
  
  gg_my_club_goals <- ggplot(my_club_goals,
                             aes(x = Season,
                                 y = Goals_proportion)) +
    geom_point(aes(col=as.factor(tier)), size=4) + ### 
    geom_line() +
    scale_color_manual(name='', 
                       labels=labs.legend,
                       values=cols.legend) +
    # geom_ma(n = 10, # We can perform moving averages
    #        colour = "red",
    #        size = 2,
    #        lty = 1,
    #       ma_fun = SMA) +
    geom_smooth(se = FALSE,
                colour = 	club_colour, 
                span = 0.5,
                size = 3) +
    scale_x_continuous(limits = season_limits,
                       breaks = season_breaks,
                       minor_breaks = NULL) + # seq(from = 1920, to = 2020, by = 10)
    scale_y_continuous(limits = c(1.5,4.5),
                       breaks = 2:4,
                       minor_breaks = NULL) +
    labs(x = "Year in which season starts (specific dates)",
         y = "Average number of goals",
         title = club_of_interest) + ### 
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 20,
                                     angle = 90,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 30),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(size=20, 
                                  face='bold', 
                                  colour =club_colour)) + ### 
    theme(legend.text=element_text(size=20)) + ### 
    theme(legend.position = c(0.25,0.08)) +
    guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    NULL ### 
  
  #
  gg_my_club_goals
  #
  # Save as a pdf
  #
  if(save_plots)
    ggsave(paste0("Average_number_of_goals_", club_of_interest, ".pdf"), 
           gg_my_club_goals,
           width = 24,
           height = 16)
  #
  # -------------------------------------------
  #
  # Plot the proportion of draws
  #
  
  my_club_proportion_draws <- my_club_summary_long %>% 
    filter(Type_f == "Draws")
  #
  my_club_proportion_draws$tier_colour<- col.tier[my_club_proportion_draws$tier] ### 
  labs.legend<- paste('Tier ', unique.tier, sep='')### 
  cols.legend<- col.tier[unique.tier] ### 
  
  gg_my_club_draws <- ggplot(my_club_proportion_draws,
                             aes(x = Season,
                                 y = Goals_proportion)) +
    geom_point(aes(col=as.factor(tier)), size=4) + ### 
    geom_line() +
    scale_color_manual(name='', 
                       labels=labs.legend,
                       values=cols.legend) +
    # geom_ma(n = 10, # We can perform moving averages
    #        colour = "red",
    #        size = 2,
    #        lty = 1,
    #        ma_fun = SMA) +
    geom_smooth(se = FALSE,
                span = 0.5,
                size = 3,
                colour = club_colour) +
    scale_x_continuous(limits = season_limits,
                       breaks = season_breaks,
                       minor_breaks = NULL) + # seq(from = 1920, to = 2020, by = 10)
    scale_y_continuous(limits = c(0, 0.5),
                       breaks = seq(from = 0, to = 0.5, by = 0.1),
                       minor_breaks = NULL, 
                       labels = scales::percent) + ### 
    # scale_color_manual(values = c("1 or 0 goals" = "orange",
    #                             "2, 1 or 0 goals" = "red")) +  
    ### I'm not sure how this fits here 
    labs(x = "Year in which season starts (specific dates)",
         y = "Proportion of Draws", title=club_of_interest) +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 20,
                                     angle = 90,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 30),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(size=20, 
                                  face='bold', 
                                  colour =club_colour)) + ### 
    theme(legend.text=element_text(size=20)) + ### 
    theme(legend.position = c(0.25,0.08)) +
    guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    NULL ### 
  
  #
  gg_my_club_draws
  #
  if(save_plots)
    ggsave(paste0("Proportion_draws_", club_of_interest, ".pdf"),
           gg_my_club_draws,
           width = 24,
           height = 16)
  #
  #
  # -----------------------------------------------------
  #
  # Two plots together
  #
  
  #ggarrange(gg_my_club_goals,
  #          gg_my_club_draws,
  #          ncol = 2)
  #
  #
  #
  # -------------------------------------------
  # -------------------------------------------
  #
  # Plot the low/high goal proportions
  #
  my_club_proportion_0_1_2 <- my_club_summary_long %>% 
    filter(Type_f == "1 or 0 goals" |
             Type_f == "2, 1 or 0 goals")
  #
  
  my_club_proportion_0_1_2$tier_colour<- 
    col.tier[my_club_proportion_0_1_2$tier] ### 
  
  
  gg_my_club_low <- ggplot(my_club_proportion_0_1_2,
                           aes(x = Season,
                               y = Goals_proportion,
                               group = Type_f,
                               colour = Type_f)) +
    # geom_point() +
    # geom_line() +
    # geom_ma(n = 10, # We can perform moving averages
    #        colour = "red",
    #        size = 2,
    #        lty = 1,
    #        ma_fun = SMA) +
    geom_smooth(se = FALSE,
                span = 0.5,
                size = 2) +
    scale_x_continuous(limits = season_limits,
                       breaks = season_breaks,
                       minor_breaks = NULL) + # seq(from = 1920, to = 2020, by = 10) 
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = seq(from = 0, to = 0.6, by = 0.1),
                       minor_breaks = NULL, 
                       labels = scales::percent) + ###  +
    scale_color_manual(values = c("1 or 0 goals" = "orange",
                                  "2, 1 or 0 goals" = "red")) + 
    labs(x = "Season",
         y = "Proportion of low-scoring matches",title=club_of_interest,### 
         colour = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 20,
                                     angle = 90,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 30),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(size=20, 
                                  face='bold', 
                                  colour =club_colour)) + ### 
    theme(legend.text=element_text(size=20)) + ### 
    theme(legend.position = c(0.25,0.08)) +
    guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    NULL ### 
  #
  gg_my_club_low
  #
  if(save_plots)
    ggsave(paste0("Proportion_2_1_0_goals_", club_of_interest, ".pdf"),
           plot = gg_my_club_low,
           width = 16,
           height = 20)
  #
  # ----------------------------------------
  #
  my_club_proportion_4_5 <- my_club_summary_long %>% 
    filter(Type_f == "4 or more goals" |
             Type_f == "5 or more goals")
  
  my_club_proportion_4_5$tier_colour<- 
    col.tier[my_club_proportion_4_5$tier] ### 
  #
  gg_my_club_high <- ggplot(my_club_proportion_4_5,
                            aes(x = Season,
                                y = Goals_proportion,
                                group = Type_f,
                                colour = Type_f)) +
    # geom_point() +
    # geom_line() +
    # geom_ma(n = 10, # We can perform moving averages
    #        colour = "red",
    #        size = 2,
    #        lty = 1,
    #        ma_fun = SMA) +
    geom_smooth(se = FALSE,
                span = 0.5,
                size = 2) +
    scale_x_continuous(limits = season_limits,,
                       breaks = season_breaks,
                       minor_breaks = NULL) + # seq(from = 1920, to = 2020, by = 10) 
    scale_y_continuous(limits = c(0, 0.6),
                       breaks = seq(from = 0, to = 0.6, by = 0.1),
                       minor_breaks = NULL, 
                       labels = scales::percent) + ### 
    scale_color_manual(values = c("4 or more goals" = "blue",
                                  "5 or more goals" = "violet")) + 
    labs(x = "Season",
         y = "Proportion of high-scoring matches", title=club_of_interest,
         colour = "") +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 20,
                                     angle = 90,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 20),
          strip.text = element_text(size = 30),
          panel.grid.major.y = element_blank(),
          plot.title=element_text(size=20, 
                                  face='bold', 
                                  colour =club_colour)) + ### 
    theme(legend.text=element_text(size=20)) + ### 
    theme(legend.position = c(0.3,0.08)) +
    guides(col = guide_legend(nrow = 1, byrow = TRUE)) +
    NULL ###
    
    ### 
  #
  gg_my_club_high
  #
  if(save_plots)
    ggsave(paste0("Proportion_5_4_goals_", club_of_interest, ".pdf"),
           plot = gg_my_club_high,
           width = 16,
           height = 20)
  #
  # -----------------------------------------------------
  #
  # Two plots together
  #
  #ggarrange(gg_my_club_low,
  #          gg_my_club_high,
  #          ncol = 2)
  #
  if(save_plots)
    ggsave(paste0("Proportion_2_1_0_5_4_goals_", club_of_interest, ".pdf"), 
           width = 24,
           height = 20)
  #
  
  return(invisible(list(plot_goals=gg_my_club_goals, plot_draws=gg_my_club_draws,
                        plot_low=gg_my_club_low, plot_high=gg_my_club_high,
                        data=my_club, goals=my_club_goals, 
                        summary=my_club_summary )))
}


testWW<-football_plots("Wolverhampton Wanderers", 'orange', 
                       season_limits=c(1888,2019), save_plots = TRUE)

testPAFC<-football_plots("Plymouth Argyle", '#005045', save_plots = TRUE)

fig6<-   ggarrange(testPAFC$plot_goals, testPAFC$plot_draws, ncol = 2)
ggsave(paste0("Goals_Draws_", club_of_interest, ".pdf"), 
       plot = fig6,   width = 24, height = 20)

ggsave(paste0("Goals_Draws_", club_of_interest, ".jpg"), 
       plot = fig6,   width = 24, height = 20, dpi=600)

fig7<- ggarrange(testPAFC$plot_low, testPAFC$plot_high, ncol = 2)
ggsave(paste0("High_Low_Scoring_", club_of_interest, ".pdf"), 
       plot = fig7,   width = 24, height = 20)

ggsave(paste0("High_Low_Scoring_", club_of_interest, ".jpg"), 
       plot = fig7,   width = 24, height = 20, dpi=600)
###########################################################
