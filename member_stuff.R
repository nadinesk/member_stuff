library(httr)
library(dplyr)
library(plyr)


x=c()


member_list <- GET("https://api.propublica.org/congress/v1/115/senate/members.json",
           add_headers(`X-API-Key` = ''))


ml_1 <- content(member_list, 'parsed')

ml_2 <- ml_1$results

ml_3 <- ml_2[[1]]

ml_4 <- ml_3$members

ml_5 <- data.frame(do.call(rbind, ml_4), stringsAsFactors=FALSE)

str(ml_5)



x3 <- lapply(ml_5, function(x) ifelse(x=="NULL", NA, x))
x4 <- lapply(x3, function(x) as.character((unlist(x))))

x5 <- as.data.frame(do.call(cbind, x4))

str(x5)


getMembers <- function(key,congress,branch) {

  make_url <- paste('https://api.propublica.org/congress/v1/',congress, '/',branch,'/members','.json', sep="" )

  get_members <-  GET(make_url,
                   add_headers(`X-API-Key` = key))
  
  ml_1 <- content(get_members, 'parsed')
  
  ml_2 <- ml_1$results
  
  ml_3 <- ml_2[[1]]
  
  ml_4 <- ml_3$members
  
  ml_5 <- data.frame(do.call(rbind, ml_4), stringsAsFactors=FALSE)
  
  ml_6 <- lapply(ml_5, function(x) ifelse(x=="NULL", NA, x))
  ml_7 <- lapply(ml_6, function(x) as.character((unlist(x))))
  
  ml_8 <- as.data.frame(do.call(cbind, ml_7))
  
  return(ml_8)
  
}

sml_115 <- getMembers(api_key, '115', 'senate') %>%
              mutate(congress_num = '115_S')

hml_115 <- getMembers(api_key, '115', 'house') %>%
              mutate(congress_num = '115_H')

sml_114 <- getMembers(api_key, '114', 'senate') %>%
              mutate(congress_num = '114_S')

hml_114 <- getMembers(api_key, '114', 'house') %>%
              mutate(congress_num = '114_H')

sml_113 <- getMembers(api_key, '113', 'senate') %>%
              mutate(congress_num = '113_S')

hml_113 <- getMembers(api_key, '113', 'house') %>%
              mutate(congress_num = '113_H')

sml_112 <- getMembers(api_key, '112', 'senate') %>%
              mutate(congress_num = '112_S')

hml_112 <- getMembers(api_key, '112', 'house') %>%
              mutate(congress_num = '112_H')

sml_111 <- getMembers(api_key, '111', 'senate') %>%
              mutate(congress_num = '111_S')
  
hml_111 <- getMembers(api_key, '111', 'house') %>%
              mutate(congress_num = '111_H')

comb_h_congress <- rbind(hml_115, hml_114, hml_113, hml_112, hml_111) %>% 
                        rename(c("district" = "district#senate_class")) %>%
                        rename(c("at_large" = "at_large#state_rank")) %>%
                        rename(c("geoid" = "geoid#lis_id")) 

comb_s_congress <- rbind(sml_115, sml_114, sml_113, sml_112, sml_111) %>% 
                        rename(c("senate_class" = "district#senate_class")) %>%
                        rename(c("state_rank" = "at_large#state_rank")) %>%
                        rename(c("lis_id" = "geoid#lis_id")) 

comb_sh_congress <- rbind(comb_h_congress, comb_s_congress)

table(comb_sh_congress$party, comb_sh_congress$congress_num)


comb_h_congress1 <- comb_h_congress %>%
                        rename(c("district" = "district/state_rank"))
str(comb_h_congress) 

names(sml_115)
names(hml_115)








