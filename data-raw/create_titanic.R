data("titanic_train",package="titanic")
library(tidyverse)

d <- titanic_train %>% as_tibble %>%
  mutate(title=str_replace_all(string = Name, # extract title as general feature
                               pattern = "^[[:alpha:][:space:]'-]+,\\s+(the\\s)?(\\w+)\\..+",
                               replacement = "\\2")) %>%
  mutate(title=str_trim(title),
         title=case_when(title %in% c('Mlle','Ms')~'Miss', # normalize some titles
                         title=='Mme'~ 'Mrs',
                         title %in% c('Capt','Don','Major','Sir','Jonkheer', 'Col')~'Sir',
                         title %in% c('Dona', 'Lady', 'Countess')~'Lady',
                         TRUE~title)) %>%
  mutate(title=as_factor(title),
         Survived=factor(Survived,levels = c(0,1),labels=c("no","yes")),
         Sex=as_factor(Sex),
         Pclass=factor(Pclass,ordered = T)) %>%
  group_by(title) %>% # impute Age by median in current title
  mutate(Age=replace_na(Age,replace = median(Age,na.rm = T))) %>% ungroup
table(d$title,d$Sex) # look on title distribution        
caret::nearZeroVar(x = d,saveMetrics = T) # search and drop some unusefull features (PassengerId,Name,Ticket)
d <- d %>% select_at(vars(-c(PassengerId,Name,Ticket)))

titanic = d

usethis::use_data( titanic, overwrite = TRUE )
