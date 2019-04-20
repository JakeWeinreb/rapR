library(spotifyr)
library(tidyverse)
library(datapasta)

Sys.setenv(SPOTIFY_CLIENT_ID = 'your_id')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your_secret')
access_token <- get_spotify_access_token()

#load in synonyms of money (used datapasta for this)------------------------------
money <- c("Bacon", "• Bank", "• Beans", "Benji", "• Benjamins", "• Biscuits", "• Big Ones", "Billionaire", 
           "• Bills", "• Bisquick", "• Bits", "• Boffo", "• Bones", "• Boodle", "• Bread", "• Brisket", "• Broccoli", 
           "• Bucks", "• Bullets", "• Bundle", "• Cabbage", "• Cake", "• Capital", "• Cash", "• Cheddar", "• Cheese", 
           "• Chips", "• Chits", "• Chump Change", "• Clams", "• Cnotes", "• Coin", "• Collard Greens", "• Cream", 
           "• Currency", "• Dead Presidents", "• Dibs", "• Din", "Dollar", "• Doubloons", "• Dough", "• Drafts", 
           "• Ducats", "• Duckets", "• Dub", "• Fetti", "• Fetta", "• Finances", "• Fins", "• Fish Sticks", 
           "• Fold Ups", "• Fortune", "• Franklins", "Frog", "• Frogskins", "• Funds", "• Gs", "• Gelt", "• Gold", 
           "Grand", "Green", "Greenback", "• Guala", "• Guap", "Gwop", "• Gwap", "• Guineas", "• Hundies", "Hunnit", 
           "• Jack", "• Jacksons", "• Kings ransom", "• Knots", "• Lettuce", "• Lincolns", "• Long green", "• Loonies", 
           "• Loot", "• Lucci", "• Luchini", "• Lucre", "• Mazuma", "• Means", "Millie", "Millionaire", "• Mint", 
           "• Mite", "• Moolah", "• Moss", "• Mozzarella", "• Notes", "• Paper", "• Payola", "• Pelf", "peso", 
           "piece of eight", "• Pieces of eight", "• Pin", "• Pittance", "• Plaster", "• Ps", "• Quiche", "• Quid", 
           "• Racks", "• Reagans", "• Rich", "• Rivets", "• Sawbucks", "• Scratch", "• Scrilla", "• Scrip", "• Shekels", 
           "• Shrapnel", "• Silver", "• Simoleons", "• Singles", "• Smack", "• Sodium", "• Specie", "• Spinach", 
           "• Spondulicks", "• Stacks", "• Stake", "• Stash", "• Tender", "• Treasure", "• VBucks", "• Wad", 
           "• Wampum", "Yard", "• Zucchini") %>%
  str_replace_all("[^[:alnum:]]", "")

#---------------------------------------------------------

#get top 100 most popular rappers today, filter to ones that can be considered pure rap
top_50 <- get_genre_artists("rap", limit = 50) 
next_50 <- get_genre_artists("rap", offset = 50, limit = 50)
current_rappers <- bind_rows(top_50, next_50) %>%
  unnest(genres) %>%
  filter(genres == "rap") %>% 
  select(artist_name = name, artist_id = id, artist_genre = genres, artist_followers = followers.total)

#get popular artists from past
i_love_90s <- get_playlist_tracks("37i9dQZF1DX186v583rmzp") %>%
  select(track.artists) %>% 
  unlist() %>% 
  .[str_detect(names(.), pattern = "track.artists.id")] %>% 
  unname() %>% 
  unique()

i_love_80s <- get_playlist_tracks("37i9dQZF1DX2XmsXL2WBQd") %>%
  select(track.artists) %>% 
  unlist() %>% 
  .[str_detect(names(.), pattern = "track.artists.id | track.artists.name")] %>% 
  unname() %>% 
  unique()

old_rappers_info <- c(i_love_80s, i_love_90s) %>%
  unique()

#loop over old_rappers to get genre and popularity info on each one
old_rappers_raw <- map(old_rappers_info, get_artist)
output <- vector("list", length = length(old_rappers_info))
for(i in seq_along(old_rappers_info)) {
  output[[i]]$artist_name <- old_rappers_raw[[i]]$name
  output[[i]]$artist_id <- old_rappers_raw[[i]]$id
  output[[i]]$artist_genre <- old_rappers_raw[[i]]$genres
  output[[i]]$artist_followers <- old_rappers_raw[[i]]$followers$total
}
old_rappers <- data.table::rbindlist(output) %>% 
  filter(artist_genre == 'rap') %>% #eliminate old artists that aren't pure "rap"
  mutate(artist_genre = as.character(artist_genre))

#combine old school and new school rappers
all_artists <- bind_rows(old_rappers, current_rappers) %>%
  .$artist_name %>%
  unique()

#get the full discography of all artists
start_time <- Sys.time()
discog_raw <- map(all_artists, possibly(get_discography, otherwise = NULL))
end_time <- Sys.time()
elapsed <- end_time - start_time
discog <- bind_rows(discog_raw)


artists_clean <- discog %>%
  ungroup() %>%
  group_by(artist_name) %>%
  distinct(track_name, .keep_all = TRUE) %>% #make sure there are no duplicate track names within each artist
  filter(!map_lgl(lyrics, is.null)) %>%
  select(artist_name, artist_id, album_name, album_id, album_release_year, duration_ms, track_name, track_uri, 
         lyrics) %>%
  unnest(lyrics) %>%
  ungroup() %>%
  mutate(lyric = tolower(str_replace_all(lyric, "[:punct:]", ""))) %>% #clean capitalization, punctuation
  mutate(count = str_count(.$lyric, 
                           pattern = paste("\\b", tolower(money),"\\b", sep="", collapse = "|"))) %>% #add counts of money words
  filter(!str_detect(tolower(track_name), "- live")) #remove song duplicates from live albums

#plot use of money words over time
artists_clean %>% 
  group_by(artist_name, album_name, album_release_year, track_name) %>%
  summarize(tot_ref = sum(count, na.rm = TRUE)) %>%
  ggplot(aes(x = album_release_year, y = tot_ref)) + #could add size = popularity
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") +
  theme_minimal()

use_over_time <- artists_clean %>% 
  rowwise() %>% #slower because the mutate won't use vectorization
  mutate(n_words = str_split(lyric, pattern = " ") %>% unlist() %>% length()) %>% 
  group_by(artist_name, album_name, album_release_year, track_name) %>% 
  summarize(num_words = sum(n_words, na.rm = TRUE),
            count_ratio = sum(count, na.rm = TRUE) / num_words)
  
#gucci mane uses by far the most money words
use_over_time %>% 
  ungroup() %>% 
  arrange(-count_ratio) %>% 
  filter(count_ratio > .03) %>% 
  count(artist_name) %>% 
  arrange(-n)

#check which artists were cut out
artists_clean_unique <- artists_clean$artist_name %>% unique()
setdiff(all_artists, artists_clean_unique)

#create list of rappers to gather net worth data on
#this list comprises rappers with at least three albums since the year I started college
#consider changing to rappers with at least 1 album in last 5 years and no albums before 2000
net_worth_list <- artists_clean %>% 
  filter(album_release_year > "2011-01-01") %>%
  select(artist_name, album_release_year) %>% 
  group_by(artist_name) %>% 
  distinct(album_release_year, .keep_all = TRUE) %>% 
  summarize(num_albums = n()) %>% 
  filter(num_albums > 2)

#export artists to google sheet for net worth data collection
library(googlesheets)
gs_title("rapR") %>% gs_ws_new(ws_title = "Net Worth", input = net_worth_list)

#load in net worth data from google spreadsheet
net_worth_df <- gs_title("rapR") %>% gs_read(ws = "Net Worth")

#combine net worth data with frequency of artists talking about money
#plot frequency vs. net worth
#plot net worth vs. first album date
artists_clean %>% group_by(artist_name) %>% 
  count(artist_name, track_name) %>%
  summarize(tot_lyrics = sum(count, na.rm = TRUE)) %>% #why do track_name and track_uri spit out diff counts?
  
#sample with kendrick
kendrick %>% select(album_name, track_name, line, lyric) %>% tidytext::unnest_tokens(word, lyric) %>% 
  mutate(money_word = ifelse(word %in% tolower(money), 1, 0)) %>% group_by(track_name) %>% 
  summarize(sum = sum(money_word)) %>% arrange(desc(sum))
  
pct_money_words <- artists_clean %>% 
  select(artist_name, album_name, track_name, line, lyric) %>% 
  tidytext::unnest_tokens(word, lyric) %>% 
  mutate(money_word = str_count(word, pattern = paste("\\b", tolower(money),"\\b", sep="", collapse = "|"))) %>% 
  group_by(artist_name, album_name) %>% 
  summarize(sum = sum(money_word, na.rm = TRUE), n = n()) %>% 
  mutate(ratio = sum / n * 100)

pct_money_words2 <- artists_clean %>% 
  select(artist_name, album_name, track_name, line, lyric) %>% 
  tidytext::unnest_tokens(word, lyric) %>% 
  mutate(money_word = str_count(word, pattern = paste("\\b", tolower(money),"\\b", sep="", collapse = "|"))) %>% 
  group_by(artist_name) %>% 
  summarize(sum = sum(money_word, na.rm = TRUE), n = n()) %>% 
  mutate(ratio = sum / n * 100)

pct_money_words2 %>% 
  inner_join(net_worth_df, by = "artist_name") %>% 
  select(-num_albums, -notes) %>% 
  ggplot(aes(net_worth, ratio)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10()

