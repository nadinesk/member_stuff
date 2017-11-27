library(httr)
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)


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

sml_110 <- getMembers(api_key, '110', 'senate') %>%
              mutate(congress_num = '110_S') 

hml_110 <- getMembers(api_key, '110', 'house') %>%
              mutate(congress_num = '110_H')

sml_109 <- getMembers(api_key, '109', 'senate') %>%
            mutate(congress_num = '109_S')

hml_109 <- getMembers(api_key, '109', 'house') %>%
  mutate(congress_num = '109_H')

sml_108 <- getMembers(api_key, '108', 'senate') %>%
  mutate(congress_num = '108_S')

hml_108 <- getMembers(api_key, '108', 'house') %>%
  mutate(congress_num = '108_H')

sml_107 <- getMembers(api_key, '107', 'senate') %>%
  mutate(congress_num = '107_S')

hml_107 <- getMembers(api_key, '107', 'house') %>%
  mutate(congress_num = '107_H')

sml_106 <- getMembers(api_key, '106', 'senate') %>%
  mutate(congress_num = '106_S')

hml_106 <- getMembers(api_key, '106', 'house') %>%
  mutate(congress_num = '106_H')

sml_105 <- getMembers(api_key, '105', 'senate') %>%
  mutate(congress_num = '105_S')

hml_105 <- getMembers(api_key, '105', 'house') %>%
  mutate(congress_num = '105_H')

sml_104 <- getMembers(api_key, '104', 'senate') %>%
  mutate(congress_num = '104_S')

hml_104 <- getMembers(api_key, '104', 'house') %>%
  mutate(congress_num = '104_H')

sml_103 <- getMembers(api_key, '103', 'senate') %>%
  mutate(congress_num = '103_S')

hml_103 <- getMembers(api_key, '103', 'house') %>%
  mutate(congress_num = '103_H')

sml_102 <- getMembers(api_key, '102', 'senate') %>%
  mutate(congress_num = '102_S')

hml_102 <- getMembers(api_key, '102', 'house') %>%
  mutate(congress_num = '102_H')

str(hml_102)
names(hml_109)
names(hml_111)

hml_110_to_102 <- rbind(hml_110, hml_109, hml_108, hml_107, hml_106, hml_105, hml_104,
                        hml_103, hml_102) %>%
                        mutate(next_election = "na")

str(hml_110_to_102)

hml_110_to_102_1 <- hml_110_to_102[ ,c(1:27,42,28:41)]

names(hml_110_to_102_1)


comb_h_congress <- rbind(hml_115, hml_114, hml_113, hml_112, hml_111,hml_110_to_102_1 ) %>% 
                        rename(c("district" = "district#senate_class")) %>%
                        rename(c("at_large" = "at_large#state_rank")) %>%
                        rename(c("geoid" = "geoid#lis_id")) 

comb_s_congress <- rbind(sml_115, sml_114, sml_113, sml_112, sml_111, sml_110, sml_109, sml_108, sml_107, sml_106, sml_105, sml_104,
                         sml_103, sml_102) %>% 
                        rename(c("senate_class" = "district#senate_class")) %>%
                        rename(c("state_rank" = "at_large#state_rank")) %>%
                        rename(c("lis_id" = "geoid#lis_id")) 

comb_sh_congress <- rbind(comb_h_congress, comb_s_congress)


cm_party <- as.data.frame(table(comb_sh_congress$party, comb_sh_congress$congress_num))

str(cm_party)

cm_party_melt <- melt(cm_party, value.var=c("value"), id.var=c("Var1", "Var2")) %>%
                    select(-variable) %>%
                    rename(c("Var1" = "Party")) %>%
                    rename(c("Var2" = "Congress_Branch")) %>%
                    rename(c("value" = "Total"))
cm_party_melt

cm_graph <- ggplot(cm_party_melt, aes(x=Congress_Branch, y=Total, fill=Party)) +   
                    geom_bar(stat="identity", position="dodge", size=0.25, width=0.8) +
                    scale_fill_manual(values=c("blue","red", "purple", "brown"))


cm_graph




