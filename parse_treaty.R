
library(tidyverse)
library(pdftools)

## taken from here
## https://www.r-bloggers.com/2018/04/strsplit-but-keeping-the-delimiter/
strsplit2 <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

raw <- pdftools::pdf_text(pdf = "https://www.kabinetsformatie2021.nl/binaries/kabinetsformatie/documenten/publicaties/2021/12/15/coalitieakkoord-omzien-naar-elkaar-vooruitkijken-naar-de-toekomst/coalitieakkoord-2021-2025.pdf")

# page1and3 <- raw[c(1, 3)]
# raw <- raw[-c(1, 3)]


chapters <- c("1\\. Democratische rechtsorde", 
  "2\\. Duurzaam land", 
  "3\\. Veiligheid en sterke samenleving",
  "4\\. Bestaanszekerheid en kansengelijkheid",
  "5\\. Welvarend land",
  "6\\. Gezondheid",
  "7\\. Internationaal",
  "Bijlage: financiën") %>% 
  paste0(collapse = "|")

clean_pages <- function(page, page_number){
  
  if(page_number == 1){
    raw <- page %>% 
      str_remove_all("      [:digit:]\n|      [:digit:][:digit:]\n") %>% 
      str_split("\n\n")
  } else if(page_number == 3){
    raw <- page %>% 
      str_remove_all("      [:digit:]\n|      [:digit:][:digit:]\n") %>% 
      str_split("\n\n") %>% 
      unlist() %>% 
      str_split("\n")     
  } else {
    raw <- page %>% 
      paste0(collapse = "    ") %>% 
      str_remove_all("      [:digit:]\n|      [:digit:][:digit:]\n|\n\n\n") %>% 
      str_split(" |\n\n") %>% 
      unlist() %>% 
      strsplit2(chapters, type = "before") %>% 
      unlist() %>% 
      strsplit2(chapters, type = "after")   
  }
  
  
  raw %>% 
    unlist()  %>% 
    str_trim() %>%
    str_squish() %>% 
    discard(~str_count(.x)==0) 

}

coalitieakkoord_part1 <- raw %>% 
  .[1:3] %>% 
  set_names(1:3) %>% 
  imap(~clean_pages(.x, .y)) %>% 
  unlist()

coalitieakkoord_part2 <- raw[4:length(raw)] %>% 
  paste0(collapse = "    ") %>% 
  str_remove_all("      [:digit:]\n|      [:digit:][:digit:]\n|\n\n\n") %>% 
  str_split(" |\n\n") %>% 
  unlist() %>% 
  strsplit2(chapters, type = "before") %>% 
  unlist() %>% 
  strsplit2(chapters, type = "after") %>% 
  unlist() %>% 
  str_trim() %>%
  str_squish() %>% 
  discard(~str_count(.x)==0) 

coalitieakkoord <- c(coalitieakkoord_part1,
  coalitieakkoord_part2)

coalitieakkoord2021 <- tibble(txt = coalitieakkoord) %>% 
  mutate(paragraph = 1:n()) %>% 
  select(paragraph, txt) %>% 
  mutate(chapter = ifelse(str_detect(txt, chapters), txt, NA)) %>%
  tidyr::fill(chapter, .direction = "down") %>% 
  mutate(chapter = ifelse(is.na(chapter), "Intro", chapter)) %>% 
  mutate(type = ifelse(paragraph %in% c(465:472, 477:479, 481:486), "tables", "txt"))

write_csv(coalitieakkoord2021, file = "coalitieakkoord2021.csv")

