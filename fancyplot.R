# David Ebert
# 27 Nov 2018

library('ggplot2')
library('ggthemes')

dat <- data.frame(
  cats = factor(c("No ECD","iThemba Mentoring"), levels=c("No ECD","iThemba Mentoring")),
  percent = c(47,80)
)

# Add title, narrower bars, fill color, and change axis labels
ggplot(data=dat, aes(x=cats, y=percent, fill=cats)) + 
  theme_solid() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12)) +
  geom_bar(fill="#32CD32", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  #ylab("Percent Proficient") +
  ylab("") +
  #geom_text(aes(label=paste(percent, '%', sep='')), vjust=-0.4, size=6) +
  xlab("") +
  coord_cartesian(ylim = c(0,90))

ggsave(filename = 'endofyearplot.png')
