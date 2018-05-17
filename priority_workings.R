numbers_unique <- numbers %>%
  mutate(duplicated = duplicated(application_number)) %>% 
  filter(duplicated == "FALSE") %>% 
  select(priority_number, application_number, publication number) %>% 
  drop_na(priority_number)

target <- nrow(numbers_unique) # add with mutate? to make life easier?

numbers_unique <- numbers_unique %>% 
  separate_rows(priority_number, sep = ";") %>% 
  mutate(priority_number = str_trim(priority_number, side = "both")) %>%
  separate(priority_number, into = c("priority", "priority_date"), sep = " ", remove = FALSE) %>% 
  mutate(priority_date = lubridate::ymd(priority_date)) %>% 
  mutate(priority_number = str_trim(priority_number, side = "both")) %>% 
  mutate(provisional = str_detect(.$priority_number, "[[:digit:]]P ")) %>%
  group_by(application_number) %>%
  mutate(priority_count = seq_along(1)) %>%
  add_tally(wt = priority_count) %>% 
  ungroup() %>% 
  mutate(priority_country = str_sub(.$priority_number, 1,2)) %>% 
  mutate(application_country = str_sub(.$application_number, 1,2)) %>%
  mutate(first = str_match(.$priority_number, .$application_number)) %>%
  select(-priority_count, -priority) 

singletons <- numbers_unique %>%
  filter(n == 1)

# prepare multiple priorities

multi <- numbers_unique %>%
  filter(n >= 2) %>% 
  group_by(application_number) %>% 
  mutate(filing_order = rank(priority_date, ties.method = "first")) %>%
  ungroup() %>% 
  mutate(duplicated_first = .$application_number %in% .$first)

# create direct filings  
direct <- multi %>% 
  filter(duplicated_first == "TRUE") %>% 
  drop_na(first)

# create indirect

indirect <- multi %>% 
  filter(duplicated_first == "FALSE") %>% 
  filter(filing_order == 1)

# bind first filings

first_filings <- bind_rows(singletons, direct, indirect) %>% 
  select(-first, -filing_order, -duplicated_first, -provisional) %>%
  rename(priorities = n)

# check we have what we are expecting from the deduplicated input

#nrow equals target. Add as a column above with mutate?

# deduplicate the priority numbers
first_filings <- first_filings %>% 
  mutate(duplicate_priority = duplicated(.$priority_number)) %>% 
  filter(duplicated == "FALSE")
