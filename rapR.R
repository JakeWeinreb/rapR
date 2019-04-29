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

#plot use of money words over time--need to figure out a better way to do this
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
            count_ratio = sum(count, na.rm = TRUE) / num_words) %>% 
  ungroup()
  
#gucci mane uses by far the most money words
use_over_time %>% 
  arrange(-count_ratio) %>% 
  filter(count_ratio > .03) %>% 
  count(artist_name) %>% 
  arrange(-n)

#check which artists were cut out
artists_clean_unique <- artists_clean$artist_name %>% unique()
setdiff(all_artists, artists_clean_unique)

#create list of rappers to gather net worth data on
#consider changing to rappers with at least 1 album in last 5 years and no albums before 2000
#maybe consider using song popularity as a proxy for sales?
net_worth_list <- artists_clean %>% 
  filter(album_release_year > "2011-01-01") %>%
  select(artist_name, album_release_year) %>% 
  group_by(artist_name) %>% 
  distinct(album_release_year, .keep_all = TRUE) %>% 
  summarize(num_albums = n()) %>% 
  filter(num_albums > 2)

#full list for net worth data...more data
net_worth_list <- artists_clean %>% 
  select(artist_name) %>% 
  unique()

#export artists to google sheet for net worth data collection
library(googlesheets)
gs_title("rapR") %>% gs_ws_new(ws_title = "Net Worth 2", input = net_worth_list) #have to log in if not already

#load in net worth data from google spreadsheet
net_worth_df <- gs_title("rapR") %>% 
  gs_read(ws = "Net Worth 2") %>% 
  mutate(net_worth = str_remove_all(net_worth, ",") %>% as.numeric())


#plot frequency of money words vs. net worth
use_over_time %>% 
  group_by(artist_name) %>% 
  summarize(count_ratio = mean(count_ratio)) %>% 
  full_join(net_worth_df) %>% 
  ggplot(aes(net_worth, count_ratio)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_log10()

#plot net worth vs. first album date

#can look into which money words are used by which rappers and overall
p <- artists_clean %>% tidytext::unnest_tokens(word, lyric)
p %>% mutate(money_lgl = ifelse(word %in% tolower(money), 1, 0)) %>% 
  filter(money_lgl == 1) %>% 
  group_by(album_release_year) %>% 
  count(word) %>% 
  top_n(1, n) %>% 
  arrange(album_release_year, n) %>% 
  print(n = 'inf')
  
