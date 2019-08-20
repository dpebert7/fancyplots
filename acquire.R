# David Ebert
# 20 August 2019
# Acquire board game question:
#   What's the probability of being able to start a new company, 
#   given the number of existing tiles on the board and how many 
#   spots are avilable to start a company?

find_p_starting_company = function(n_tiles_played, n_starting_spaces){
  # Too many possible starting spaces
  if(n_tiles_played * 4 < n_starting_spaces){
    return(NA)
  } else if (n_starting_spaces <= (n_tiles_played)){
  # Not enough starting spaces
    return(NA)
  # Some of these combos are also invalid, but we'll show them anyway.
  } else {
    return(1-
             (108 - n_tiles_played - n_starting_spaces) / (108 - n_tiles_played) * 
             (107 - n_tiles_played - n_starting_spaces) / (107 - n_tiles_played) * 
             (106 - n_tiles_played - n_starting_spaces) / (106 - n_tiles_played) * 
             (105 - n_tiles_played - n_starting_spaces) / (105 - n_tiles_played) * 
             (104 - n_tiles_played - n_starting_spaces) / (104 - n_tiles_played) * 
             (103 - n_tiles_played - n_starting_spaces) / (103 - n_tiles_played)  
    )
  }
}

# Common 3 player game
find_p_starting_company(3,12)

# Common 4 player game:
find_p_starting_company(4,14)

# Invalid:
find_p_starting_company(4,1)

for(tiles_played in seq(1,4)){
  for(starting_spaces in seq(4,12)){
   cat("\n\nTiles Played: ", tiles_played,
                "\nStarting Spaces: ", starting_spaces,
                "\nProb of starting a chain: ", find_p_starting_company(tiles_played, starting_spaces))
  }
}



### Make Heatmap

df <- expand.grid(tiles_played = seq(1,6),
                  starting_spaces = seq(2,24)
)

i = 1
for(starting_spaces in seq(2,24)){
  for(tiles_played in seq(1,6)){
      df[i,"prob_starting"] = find_p_starting_company(tiles_played, starting_spaces) * 100
    i = i + 1
  }
}


require(ggplot2)
theme_set(theme_bw())
ggplot(data = df, aes(y = starting_spaces, x = tiles_played)) +
  geom_tile(aes(fill = prob_starting)) +
  geom_text(aes(label = round(prob_starting, 1))) + 
  scale_fill_gradient(low = "white", high = "red", na.value = "white") + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  labs(#title="Can I Start a New Company?",
       title="",
       x="Number of tiles currently on the board",
       y="Number of spaces available to start a company") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_continuous(breaks=seq(2,24,2), position = "right") +
  annotate("text", x = c(2), y = c(24), 
           label = c("Can I start a new company?"), 
           color="black", size=5)

ggsave('acquire.png')
