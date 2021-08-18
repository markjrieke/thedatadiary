# libraries ----
library(tidyverse)

# import data ----

# distances
path_distance <- "http://pi-ratebay.com/files/all_keyboard_distances.txt"

f_distances <- 
  read_fwf(path_distance,
           fwf_empty(path_distance,
                     col_names = c("letter_combination", "equal", "distance")))

# words
path_words <- "https://github.com/dwyl/english-words/raw/master/words.txt"

f_words <- 
  read_fwf(path_words, fwf_empty(path_words))

# prep distance frame ----

# reformat distance frame
f_distances <- 
  f_distances %>%
  select(-equal) %>%
  mutate(letter_combination = str_remove_all(letter_combination, "\\|"),
         letter_combination = str_to_lower(letter_combination),
         distance = str_remove_all(distance, "m"),
         distance = as.numeric(distance))

# hands on a keyboar rest on ASDFJKL; keys, just want combinations that start with these
# ; is not listed in the distances frame, but we'll deal with that later
f_distances <- 
  f_distances %>%
  filter(str_sub(letter_combination, 1, 1) %in% c("a", "s", "d", "f", "j", "k", "l", ";"))

# create a list of which fingers are used for each letter
f_fingers <- 
  tribble(
    ~letter, ~finger,
    "a", "left pinky",
    "b", "left pointer",
    "c", "left middle",
    "d", "left middle",
    "e", "left middle",
    "f", "left pointer", 
    "g", "left pointer",
    "h", "right pointer",
    "i", "right middle",
    "j", "right pointer",
    "k", "right middle",
    "l", "right ring",
    "m", "right pointer",
    "n", "right pointer",
    "o", "right ring",
    "p", "right pinky",
    "q", "left pinky",
    "r", "left pointer",
    "s", "left ring",
    "t", "left pointer",
    "u", "right pointer",
    "v", "left pointer",
    "w", "left ring",
    "x", "left ring",
    "y", "left pointer",
    "z", "left pinky"
  )

# filter down to just distances that will be traveled 
# (aka, start finger & end finger are the same)
f_distances <- 
  f_distances %>%
  mutate(start = str_sub(letter_combination, 1, 1),
         end = str_sub(letter_combination, 2, 2)) %>%
  left_join(f_fingers, by = c("start" = "letter")) %>%
  left_join(f_fingers, by = c("end" = "letter")) %>%
  filter(finger.x == finger.y) %>%
  select(end, distance)

# manually add 0 distances for ASDFGHJKL 
f_distances <- 
  f_distances %>%
  rename(letter = end) %>%
  bind_rows(
    tibble(letter = c("a", "s", "d", "f", "j", "k", "l"),
           distance = rep(0, 7))
  )

# p is the only letter used by the right pinky, and travels from ; to p. 
# using the same distance for uj/ik/ol for p (19.64)
f_distances <- 
  f_distances %>%
  bind_rows(
    tibble(letter = "p",
           distance = 19.64)
  )

# clean up environment
rm(f_fingers)

# prep words frame ----
f_words <- 
  f_words %>%
  rename(word = X1) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(!str_detect(word, "[:punct:]"),
         !str_detect(word, "[:digit:]"))

# define distance function ----

# assuming your fingers return to the "resting key" between letters,
# each distance should be multiplied by 2 (to/fro)
f_distances <- 
  f_distances %>%
  mutate(distance = 2 * distance) %>%
  arrange(letter)

get_distance <- function(input_string) {
  
  distance <- sum(str_count(input_string, f_distances$letter) * f_distances$distance)
  return(distance)
  
}

# get the total distances for the words set! ----

# this is slow, becasue of row-wise operations, but just wanted to quickly throw this together
f_words <- 
  f_words %>%
  rowwise() %>%
  mutate(total_distance = get_distance(word)) %>%
  ungroup() %>%
  mutate(distance_per_letter = total_distance/str_length(word))

# 10 longest total distances
f_words %>%
  arrange(desc(total_distance)) %>%
  slice_head(n = 10)

# top word: pneumonoultramicroscopicsilicovolcanoconiosis @ 1465.58 mm
# top (non-chemical) word: counterexcommunication @ 871.44 mm
  
# 10 longest distances per letter
f_words %>%
  arrange(desc(distance_per_letter)) %>%
  slice_head(n = 10)

# top non-trivial: by @ 76.8 mm/letter

# 10 shortest words (greater than 0)
f_words %>%
  filter(total_distance > 0) %>%
  arrange(total_distance) %>%
  slice_head(n = 10)

# shortest non-trivial: ah @ 38.1 mm
# shortest non-proper english word I recognize: ash @ 38.1 mm

# 10 shortest distances per letter (greater than 0)
f_words %>%
  filter(total_distance > 0) %>%
  arrange(distance_per_letter) %>%
  slice_head(n = 10)

# shortest (top): valaskjalf @ 4.26 mm/letter (norse word)
# shortest english: deadfalls @ 4.36 mm/letter

# longest word w/0 distance
f_words %>%
  filter(total_distance == 0) %>%
  arrange(desc(str_length(word)))

# longest: alfalfas