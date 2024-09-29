


library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(scales)
library(ggrepel)


########################################
# 1 - Iowa Liquor Sales / Market Share #
#      * multiple geoms                #
#      * introduce joining data        #
#      * lots of formatting            #
#      * custom theme?                 #
########################################
d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales.csv') %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label=T)
  )


# MONTHLY SALES
monthly = d %>%
  group_by(month) %>%
  summarise(total_sales = sum(sale_total))

# OUR SALES
prox = d %>%
  filter(vendor == 'PROXIMO') %>%
  group_by(year, month) %>%
  summarise(prox_sales = sum(sale_total)) %>%
  left_join(monthly, by='month') %>%
  mutate(
    market_share = prox_sales/total_sales,
    on_target = ifelse(market_share > .03, 'Yes', 'No')
  )

ggplot(prox, aes(x=reorder(month, year), y=market_share, fill=on_target)) + 
  geom_col(color='black') + 
  geom_hline(yintercept=.03, color='red3', linetype=2) + 
  geom_vline(xintercept = 5, linewidth=2) + 
  scale_fill_manual('On Target?', values=c('grey85', 'lawngreen')) +
  scale_y_continuous(labels=percent) +
  labs(title='Market Share Analysis') + 
  theme_wsj()


#############################
# 2 - Game Level Difficulty #
#      * multiple geoms     #
#      * coloring via aes() #
#      * formatting         #
#      * errorbars          #
#############################   
data = read.csv('https://dxl-datasets.s3.amazonaws.com/data/candy_crush.csv') %>%
  group_by(level) %>%
  summarise(
    wins = sum(num_success),
    attempts = sum(num_attempts),
    p = wins/attempts
  ) %>%
  mutate(
    se = sqrt((p*(1-p))/attempts),
    p_low = p - 2*se,
    p_high = p + 2*se,
    Difficulty = ifelse(p_high < .1, 'Too Hard', ifelse(p_low > .1, 'Too Easy', 'Perfect'))
  )

ggplot(data, aes(x=level, y=p, fill=Difficulty, ymin=p_low, ymax=p_high)) + 
  geom_col(color='black') +
  geom_point() +
  geom_hline(yintercept=.1, linetype=2, color='red2')+
  geom_errorbar() +
  scale_y_continuous(labels=percent, breaks=0:10/10) +
  scale_x_continuous(breaks = 1:15) +
  scale_fill_manual(values = c('gold', 'grey75', 'red2')) +
  labs(x = 'Game Level', y = 'Win Probability', title = 'Difficulty by Game Level')


##############################################
# 3 -  HDI vs CPI from Economist             #
#      * plot easy                           #
#      * theme hard                          #
#      * practice customizing theme elements #
##############################################

colors = c('#2b6276','#34abdd','#94dbf8','#3f9e95','#f36c59','#863721')
EconData = read_csv('https://dxl-datasets.s3.amazonaws.com/data/EconomistData.csv')

myCountries = subset(EconData, 
                     Country %in% c('Afghanistan', 'Congo', 'Sudan', 'Myanmar', 
                                    'Iraq', 'Venezuela', 'Russia', 'Argentina', 
                                    'Greece', 'Brazil', 'India', 'Italy', 'China', 
                                    'South Africa', 'Rwanda', 'Bhutan', 'Cape Verde', 
                                    'Botswana', 'Spain', 'France', 'United States', 
                                    'Germany', 'Britain', 'Barbados', 'Japan', 
                                    'Norway', 'Singapore', 'New Zealand'))

ggplot(EconData, aes(x=CPI, y=HDI, color=Region)) + 
  geom_hline(yintercept=2:10/10, color='grey75') + 
  geom_smooth(method='loess', color='red3', se=F, span=10) +
  geom_point(size=4, shape = 1, stroke=2) +
  scale_color_manual(values=colors) + 
  scale_x_continuous(breaks=1:10, limits=c(1, 10)) + 
  scale_y_continuous(breaks=1:10/10, limits=c(.2, 1)) + 
  labs(color = NULL,
       x = 'Corruption Perceptions Index, 2011 (10 = least corrupt)',
       y = 'Human Development Index, 2011 (1 = best)',
       title = 'Corruption and human development',
       caption = 'Sources: Transparency International; UN Human Development Report') +
  theme(
    axis.title.x = element_text(face='italic', size=12),
    axis.title.y = element_text(face='italic', size=12),
    axis.text = element_text(size=12),
    plot.title = element_text(face='bold', size=24),
    plot.caption = element_text(hjust=0),
    panel.background = element_blank(),
    legend.position = 'top',
    legend.key = element_rect(fill='white'),
    legend.text = element_text(size='12')
  ) + 
  guides(color = guide_legend(nrow = 1)) + 
  geom_text_repel(data=myCountries, aes(x=CPI, y=HDI, label=Country), color='black')
