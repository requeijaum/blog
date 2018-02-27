# deduplicate
vietnam <- vietnam %>% select(publication_number, application_numbers, priority_numbers_long, inpadoc_first_family_member_number, inpadoc_first_family_member_number)

vietnam$appln_duplicated <- duplicated(vietnam$application_numbers)

vietpr1 <- filter(vietnam, appln_duplicated == FALSE) %>% 
  separate_rows(priority_numbers_long, sep = ";")

vietpr1$priority_numbers_long <- trimws(vietpr1$priority_numbers_long, which = "both")  

vietpr1 <- vietpr1  %>% separate(., priority_numbers_long, into = c("priority_number", "priority_date"), sep = " ", remove = FALSE)

vietpr1$priority_date <- lubridate::ymd(vietpr1$priority_date)

vietpr1 <- vietpr1 %>% 
  dplyr::group_by(application_numbers) %>%
  dplyr::mutate(filing_order = rank(priority_date, ties.method = "first"))
