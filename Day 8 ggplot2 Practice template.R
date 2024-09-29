


library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(scales)
library(ggrepel)
library(readr)

########################################
# 1 - Iowa Liquor Sales / Market Share #
#      * multiple geoms                #
#      * introduce joining data        #
#      * lots of formatting            #
#      * custom theme?                 #
########################################
d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales.csv')


str(d)


d2 = d %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )

d2 = d %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date, label = T)
  )


#monthly total

monthly = d2 %>%
  group_by(month) %>%
  summarise(total_sales = sum(sale_total))

#PROXIMO

prox = d2 %>%
  filter(vendor == 'PROXIMO') %>%
  group_by(year, month) %>% #year
  summarise(prox_sales = sum(sale_total)) %>% #????? object 'sale_total' not found
  left_join(monthly, by = 'month') %>%
  mutate(
    market_share = prox_sales/total_sales,
    on_target = ifelse(market_share > .03, 'yes', 'no')
  )

       
       
         
ggplot(prox, aes(x=reorder(month, year), y=market_share, fill = on_target)) + 
  geom_col(color = 'black') +
  geom_hline(yintercept = .03, color = 'red', linetype = 2) +
  geom_vline(xintercept = 5, linewidth = 2) +
  scale_fill_manual('On Target?', values = c('grey75', 'gold')) +
  scale_y_continuous(labels = percent) +
  labs(x = '', y = 'Market Share', title = 'Market Share Analysis') +
  theme_wsj()


#theme_minimum theme_excel bw .....



  


#############################
# 2 - Game Level Difficulty #
#      * multiple geoms     #
#      * coloring via aes() #
#      * formatting         #
#      * errorbars          #
#############################   
data = read_csv('https://dxl-datasets.s3.amazonaws.com/data/candy_crush.csv')










##############################################
# 3 -  HDI vs CPI from Economist             #
#      * plot easy                           #
#      * theme hard                          #
#      * practice customizing theme elements #
##############################################

colors = c('#2b6276','#34abdd','#94dbf8','#3f9e95','#f36c59','#863721')





EconData = read_csv('https://dxl-datasets.s3.amazonaws.com/data/EconomistData.csv')





ggplot(EconData, aes(x = CPI, y = HDI, color = Region)) +
  geom_hline(yintercept = 1:10/10, color = 'gray75') +
  geom_smooth(method = 'loess', color='red', se=F, span = 10) +
  geom_point(size=5, shape = 1, stroke = 2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = 1:10, limits = c(1,10)) +
  scale_y_continuous(breaks = 1:10/10, limits = c(.2, 1)) +
  labs(color = '',
       x = 'Corruption Perceptions Index, 2011 (10 = least corrupt)',
       y = 'Human Development Index, 2011 (1 = best)',
       title = 'Corruption and Human Development',
       caption = 'Sources: Transparency International; UN Human Development Report') +
  theme(
    plot.title = element_text(face="bold", size = 24),
    axis.title.x = element_text(face="italic"),
    axis.title.y = element_text(face="italic"),
    plot.caption = element_text(hjust = 0),
    panel.background = element_blank(),
    legend.position = 'top',
    legend.key = element_rect(fill = 'white')
  ) +
  guides(color = guide_legend(nrow = 1)) +
  geom_text_repel(data = myCountries, aes(x=CPI, y = HDI, label = Country), color = 'black')




myCountries = subset(EconData, 
                     Country %in% c('Afghanistan', 'Congo', 'Sudan', 'Myanmar', 
                                    'Iraq', 'Venezuela', 'Russia', 'Argentina', 
                                    'Greece', 'Brazil', 'India', 'Italy', 'China', 
                                    'South Africa', 'Rwanda', 'Bhutan', 'Cape Verde', 
                                    'Botswana', 'Spain', 'France', 'United States', 
                                    'Germany', 'Britain', 'Barbados', 'Japan', 
                                    'Norway', 'Singapore', 'New Zealand'))

  
  
  theme(
    legend.position = 
    
  )
  






