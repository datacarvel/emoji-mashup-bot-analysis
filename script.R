library(tidyverse)
library(bskyr)
library(emoji) # IMO, useful for the emoji database but not so for visualizing them
library(data.table)
library(ragg)
library(textshaping)
library(systemfonts)
library(emoGG) # Works ok if plot is square (otherwise emojis are stretched on the longer axis)
library(utf8)
library(camcorder)

#library(Rmoji) - not very useful in this case
#library(emojifont) - works, but is boring (emojis are boringly colorless)
#library(ggimage) - never worked, might be useful if emojis are image files

fontss <- system_fonts()

gg_record(dir = "camcorder-test", device = "png", width = 6, height = 8, units = "in", dpi = 320)

set_bluesky_user(user = "stevecarufel.bsky.social")
set_bluesky_pass(pass = "f53t-nc55-lgex-darg")

auth <- bs_auth(user = bs_get_user(), pass = bs_get_pass())

profile <- bs_get_profile('emojimashupbot.bsky.social')

feed <- bs_get_author_feed("emojimashupbot.bsky.social", limit = 1000)
feed10k <- bs_get_author_feed("emojimashupbot.bsky.social", limit = 10000)
# 1000 posts cover ~ 90 days

#glimpse(feed)

feed_df <- feed10k |>
  select(uri, reply_count, repost_count, like_count, quote_count, record)

post_records <- bind_rows(feed_df$record) |>
  select(created_at, text)

df <- bind_cols(feed_df, post_records) |> select(-record, -uri)

df_sort <- df |>
  filter(str_detect(text, "(?=.*\\+).*=$")) # making sure the post is a normal post (an auto emoji mashup) (has at least one plus sign + and ends with equal sign)

df_sort_w_emojis <- df_sort |>
  mutate(
   emojiLIST = emoji::emoji_extract_all(text)
  )

outdf_LIST <- df_sort_w_emojis$emojiLIST

unlisted_LIST <- rbindlist(lapply(outdf_LIST, as.list), fill = T, use.names = F)

colnames(unlisted_LIST) <- c("emoji1", "emoji2", "emoji3")

df_emojis <- bind_cols(df_sort_w_emojis, unlisted_LIST)

result1 <- df_emojis |>
  group_by(emoji1) |>
  reframe(
    emoji1,
    avg_likes_per_presence = mean(like_count),
    avg_reposts_per_presence = mean(repost_count),
    n_emoji = n()
  ) |>
  ungroup() |>
  unique() |>
  select(emoji = emoji1, avg_likes_per_presence, avg_reposts_per_presence, n_emoji) |>
  arrange(desc(avg_likes_per_presence))

result2 <- df_emojis |>
  group_by(emoji2) |>
  reframe(
    emoji = emoji2,
    avg_likes_per_presence = mean(like_count),
    avg_reposts_per_presence = mean(repost_count),
    n_emoji = n()
  ) |>
  ungroup() |>
  unique() |>
  select(emoji = emoji2, avg_likes_per_presence, avg_reposts_per_presence, n_emoji) |>
  arrange(desc(avg_likes_per_presence))

result3 <- df_emojis |>
  filter(!is.na(emoji3)) |>
  group_by(emoji3) |>
  reframe(
    emoji = emoji3,
    avg_likes_per_presence = mean(like_count),
    avg_reposts_per_presence = mean(repost_count),
    n_emoji = n()
  ) |>
  ungroup() |>
  unique() |>
  select(emoji = emoji3, avg_likes_per_presence, avg_reposts_per_presence, n_emoji) |>
  arrange(desc(avg_likes_per_presence))

df_3_results <- bind_rows(result1, result2, result3) |>
  group_by(emoji) |>
  reframe(
    EMOJI = emoji,
    avg_likes_final = weighted.mean(avg_likes_per_presence, w = n_emoji),
    avg_reposts_final = weighted.mean(avg_reposts_per_presence, w = n_emoji)
  ) |>
  unique() |>
  select(EMOJI, avg_likes_final, avg_reposts_final) |>
  arrange(desc(avg_likes_final))
  
###

# preparing emojis viz

list_emj <- emoji::emojis |>
  select(emoji, name, group, subgroup, runes) # join data for group, subgroup etc

pre_modf <- df_3_results |>
  rename(emoji = EMOJI)

EMO_DF <- left_join(
  pre_modf,
  list_emj,
  by = "emoji"
) |>
  mutate(
    runes = tolower(runes)
  ) |>
  mutate(
    runes = if_else(
      str_detect(runes, " fe0f"),
      str_remove(runes, " fe0f"),
      runes
    )
  ) |>
  arrange(desc(avg_likes_final))

###

# vizzing

emojicol <- "#1CA1F1"
emojicol2 <- "darkblue"
iconcol <- "#FFDD5E"
bigfamily = "Rammetto One"
smallfamily = "DynaPuff"
txtcol <- "white"

ggplot(EMO_DF, aes(x = avg_likes_final, y = avg_reposts_final)) +
  #geom_text(family = "OpenMoji", size = 10, aes(label = EMOJI)) +
  #geom_col() +   
  geom_point(color = "darkblue", size = 10) +
  emoGG::geom_emoji(aes(emoji = runes)) +
  #emojifont::geom_emoji() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(color = txtcol, hjust = .5, face = "bold", family = smallfamily, size = 21, margin = margin(t = 6, unit = "pt")),
    plot.subtitle = element_text(color = txtcol, hjust = .5, family = "Josefin Sans", face = "bold", size = 12, margin = margin(b = 10, unit = "pt")),
    plot.caption = element_text(size = 9, color = txtcol, hjust = .5, family = "Josefin Sans", face = "bold", margin = margin(t = 10, b = 7, unit = "pt")),
    axis.title = element_text(family = smallfamily, size = 14, color = "white"),
    axis.text = element_text(color = iconcol, family = smallfamily, size = 13.5),
    axis.line = element_blank(),
    plot.margin = margin(l = 10, r = 10, t = 8, b = 8, unit = "pt"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = emojicol),
    panel.background = element_rect(fill = emojicol, color = emojicol)
  ) +
  labs(
    title = "When doing emoji mashups,\nbetter give the finger",
    subtitle = "
    The Emoji Mashup Bot posts every 2 hours\n
    It combines 2 or 3 emojis : 
    faces and occasionnal accessories and hands\n
    Which emojis generate more likes
    and reposts than others?
    ",
    y = "Mean number of reposts per post",
    x = "Mean number of likes per post",
    caption = "Source : Posts by @emojimashupbot.bsky.social (~6000)\nPosts fetched thanks to {bskyr}\nMade by @stevecarufel.bsky.social for the #30DayChartChallenge"
  ) +
  coord_cartesian(clip = "off")

ggsave("emojimashupbot.png", width = 768, height = 1250, units = "px")

record_polaroid()

finger <- "On average, the middle-finger generates 8.78 likes and 2.22 reposts when it's part of a mashup"
woozy <- "Woozy Face is a distant second. On average, its presence brings in 6.55 likes and 1.63 repost per post"
popcorn <- "Popcorn seems to be the best accessory"
fist <- "Oncoming Fist is the second best use of hands"