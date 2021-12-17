# Install Packages
install.packages(c('rym',
                   'telegram.bot',
                   'tidyverse'
))

# Get Packages
library(rym)
library(telegram.bot)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Options
options(rym.user = "..."
        rym.token_path = "...")

# Authorization
rym_auth()

# Get data from Yandex Metrika
ym_data <- rym_get_data(conters = , # counter id
                        date.from = Sys.Date() - 8,
                        date.to = Sys.Date() - 1,
                        dimensions = 'ym:s:date,
                                      ym:s:trafficSourceName',
                        metrics = 'ym:s:users,
                                   ym:s:visits,
                                   ym:s:sumGoalReachesAny,
                                   ym:s:bounces',
                        lang = 'en')
# Additional calculation
# Yesterday traffic source
tr_source_yesterday <- ym_data %>%
  filter(`Date of visit` == Sys.Date() - 1) %>%
  arrange(desc(Sessions)) %>%
  head(1) %>%
  mutate(bounce_rate = `Number of bounces` / Sessions,
         info = 
           str_glue('The most popular traffic;s source for yesterday:* \n',
                    '{`Last traffic source`}\n',
                    'Users: {`Users`}\n',
                    'Sessions: {`Sessions`}\n',
                    'Goals reached: {`All goals reached`}\n',
                    'Bounce rate: {`bounce_rate`}')) %>%
  .$info

# Traffic source for seven days
tr_source_week <- ym_data %>%
  filter(`Date of visit` < Sys.Date() - 1) %>%
  group_by(`Last traffic source`) %>%
  summarise(across(where(is.numeric), sum)) %>%
  arrange(desc(Sessions)) %>%
  head(1) %>%
  mutate(bounce_rate = `Number of bounces` / Sessions,
         info = 
           str_glue('The most popular traffic;s source for 7 days:* \n',
                    '{`Last traffic source`}\n',
                    'Users: {`Users`}\n',
                    'Sessions: {`Sessions`}\n',
                    'Goals reached: {`All goals reached`}\n',
                    'Bounce rate: {`bounce_rate`}')) %>%
  .$info

# Dynamics
dynamic_data <- ym_data %>%
  mutate(period = if_else(`Date of visit` < Sys.Date() - 1,
                          'last week',
                          'yesterday')) %>%
  group_by(`period`) %>%
  summarise(across(were(is.numeric), sum))

# Calculate dynamics function
compare <- function(data, ind) {
  
  prev <- data[data$period == 'last week', ind] / 7
  yest <- data[data$period == 'yesterday', ind]
  res <- (prev - yest) / prev * 100
  
  return(str_c(round(res,0),"%"))
}

user_dyn <- compare(dynamic_data, 'Users')
ses_dyn <- compare(dynamic_data, 'Sessions')
nub_dyn <- compare(dynamic_data, 'Number of bounces')

# Message text
dyn_msg <- str_glue('Dynamics, comparison of yesterday\'s indicators 
                    with the average daily indicators for the previous 
                    week',
                    'Users: {user_dyn}\n',
                    'Sessions: {ses_dyn}\n',
                    'Conversions: {nub_dyn}')

# Visualization
ym_data %>%
  mutate(`Date of visit` = as.Date(`Date of visit`)) %>%
  ggplot(aes(x = `Date of visit`,
             y = Sessions,
             colot = `Last traffic source`)) +
  geom_line() + geom_point() +
  ggsave('metrica,png', device = 'png')

# Send message to telegram
tr_source_yesterday
tr_source_week
dyn_msg

# Initialize bot
bot <- Bot('...')

# Getting chat_id

#updates <- bot$getUpdates()
#chat_id <- updates[[1]]$message$chat_id

chat_id <- ...

# Send message
bot$sendMessage(
  chat_id,
  tr_source_week,
  'Markdown')

bot$sendMessage(
  chat_id,
  tr_source_yesterday,
  'Markdown')

# Plot
bot$sendPhoto(
  chat_id,
  'metrica.png')
