
# Important points
#   - Ordering was important
#   - Bold text to make more legible
#   - Improved axis ticks
#   - legend, at minimum, to be inside plot to save space on page
#   - ideally just single line highlighting the color / rather than full legend

library(dplyr)
library(ggplot2)
library(scales)

regression_results = data.frame(
  y = rep(c('PCS', 'MCS'), each=9),
  x = rep(c('PF', 'RP', 'RE', 'VT', 'MH', 'SF' , 'BP', 'GH', 'Mass'), 2),
  beta = c(.25, .2, .11, .25, .01, -.02, .15, .11, .04, .14, .05, .56, .38, .66, .5, .2, .07, -.07),
  pval = c(.01, .01, .13, .00, .97, .83, .02, .15, .39, .24, .61, .01, .02, .02, .03, .11, .64, .41)
)


# Initial plot...
regression_results %>%
  mutate(Significant = ifelse(pval<.05, 'Yes', 'No'),
         y = factor(y, levels=c('PCS', 'MCS')),
         x = factor(x, levels=c('Mass', 'GH', 'BP', 'SF', 'MH', 'VT', 'RE', 'RP', 'PF'))) %>%
  ggplot( aes(x=x, y=beta, fill=Significant)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~y)

# FIRST TRY
regression_results %>%
  mutate(Significant = ifelse(pval<.05, 'Yes', 'No'),
         y = factor(y, levels=c('PCS', 'MCS')),
         x = factor(x, levels=c('Mass', 'GH', 'BP', 'SF', 'MH', 'VT', 'RE', 'RP', 'PF'))) %>%
  ggplot( aes(x=x, y=beta, fill=Significant)) +
  geom_col() +
  coord_flip() +
  labs(x = '', y='Effect Size') +
  scale_fill_manual('p-value < 0.05', values=c('grey65', 'grey40')) +
  scale_y_continuous(breaks=0:10/10)+
  theme(text = element_text(face='bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=15),
        strip.text = element_text(size = 15),
        legend.position = c(.922, .085),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.key.size = unit(1, 'cm')
  ) +
  facet_wrap(~y)

# SECOND TRY
regression_results %>%
  mutate(Significant = ifelse(pval<.05, 'p-value < 0.05', ''),
         y = factor(y, levels=c('PCS', 'MCS')),
         x = factor(x, levels=c('Mass', 'GH', 'BP', 'SF', 'MH', 'VT', 'RE', 'RP', 'PF'))) %>%
  ggplot( aes(x=x, y=beta, fill=Significant)) +
  geom_col() +
  coord_flip() +
  labs(x = '', y='Effect Size') +
  scale_fill_manual('', values=c('grey65', 'grey40')) +
  scale_y_continuous(breaks=0:10/10)+
  theme(text = element_text(face='bold'),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=15),
        strip.text = element_text(size = 15),
        legend.position = c(.91, .085),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.key.size = unit(1, 'cm'),
        legend.background = element_blank()) +
  guides(
    fill = guide_legend(override.aes = list(size=1, 
                                            fill=c('grey92', 'grey40'),
                                            color='grey92'))) +
  facet_wrap(~y)

