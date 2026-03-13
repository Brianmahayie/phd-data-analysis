# API Pull ####
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(sf)
library(openxlsx)

kobo_token <- Sys.getenv("KOBO_TOKEN")
form_id <- "aDnYgCWxBduEXrC6cFzCP2"
base_url <- "https://kf.kobotoolbox.org/api/v2/assets/"

all_results <- list()
offset <- 0
limit <- 1000
repeat {
  response <- GET(
    paste0(base_url, form_id, "/data.json?limit=", limit, "&start=", offset),
    add_headers(Authorization = paste("Token", kobo_token))
  )
  data_list <- content(response, as = "parsed", simplifyDataFrame = TRUE)
  batch <- as.data.frame(data_list$results)
  all_results <- append(all_results, list(batch))
  if (nrow(batch) < limit) break
  offset <- offset + limit
}
ws_data <- bind_rows(all_results)

# Column Name Cleaning ####
ws_uuid <- ws_data[["_uuid"]]

names(ws_data) <- names(ws_data) %>%
  sub(".*/", "", .) %>%
  sub("^_", "", .)

# Coalesce duplicate columns before deduplication
dup_names <- names(ws_data)[duplicated(names(ws_data))]
for (dn in unique(dup_names)) {
  cols <- which(names(ws_data) == dn)
  ws_data[[cols[1]]] <- coalesce(ws_data[[cols[1]]], ws_data[[cols[2]]])
}
ws_data <- ws_data[, !duplicated(names(ws_data))]
ws_data$uuid <- ws_uuid

# Datetime & Consent Filter ####
ws_data <- ws_data %>%
  filter(consent == "yes" | (functional == "1" & is.na(consent)))%>%
  mutate(
    start_time = ymd_hms(start_time, tz = "UTC"),
    start = ymd_hms(start, tz = "UTC"),
    end_time = ymd_hms(end_time, tz = "UTC"),
    survey_date = as.Date(coalesce(start_time, start)),
    display_month = factor(
      case_when(
        month(survey_date) %in% c(11, 12) ~ "Nov",
        month(survey_date) %in% c(1, 2)   ~ "Jan",
        month(survey_date) %in% c(3, 4)   ~ "Mar",
        month(survey_date) %in% c(5, 6)   ~ "May",
        month(survey_date) %in% c(7, 8)   ~ "Jul",
        month(survey_date) %in% c(9, 10)  ~ "Sep",
        TRUE ~ NA_character_
      ),
      levels = c("Nov", "Jan", "Mar", "May", "Jul", "Sep")
    )
  )

# Numeric Conversions ####
ws_data <- ws_data %>%
  mutate(
    percent_women_gatherers = as.integer(percent_women_gatherers),
    price_per_jerrycan = as.numeric(price_per_jerrycan),
    price_per_month = as.numeric(price_per_month)
  )

# November WSID Removal ####
ws_data$source_ID <- ifelse(ws_data$month == "november", NA, ws_data$source_ID)

# Factors ####
ws_data <- ws_data %>%
  mutate(surveyor_name = factor(surveyor_name,
                                levels = c("abdulaib", "abdulaik", "boboieh", "jamiatu", "musa", "joana", "moses", "joseph", "sinneh", "other"),
                                labels = c("Abdulai B", "Abdulai K", "Boboieh", "Jamiatu", "Musa", "Joana", "Moses", "Joseph", "Sinneh", "Other")))

ws_data <- ws_data %>%
  mutate(community = factor(community,
                            levels = c("cockle_bay", "dworzark", "portee_rokupa"),
                            labels = c("Cockle Bay", "Dworzark", "Portee Rokupa")))

ws_data <- ws_data %>%
  mutate(cocklezone = factor(cocklezone,
                             levels = c("inlet_view", "jay_matta", "kola_stick", "mafengbeh"),
                             labels = c("Inlet View", "Jay Matta", "Kola Stick", "Mafengbeh")))

ws_data <- ws_data %>%
  mutate(porteezone = factor(porteezone,
                             levels = c("benk", "mefleh", "portee_wharf", "rokupa_wharf"),
                             labels = c("Benk", "Mefleh", "Portee Wharf", "Rokupa Wharf")))

ws_data <- ws_data %>%
  mutate(dworzarkzone = factor(dworzarkzone,
                               levels = c("argentina", "brazil", "cameroon", "england", "france",
                                          "germany", "holland", "italy", "morocco", "nigeria",
                                          "spain", "usa"),
                               labels = c("Argentina", "Brazil", "Cameroon", "England", "France",
                                          "Germany", "Holland", "Italy", "Morocco", "Nigeria",
                                          "Spain", "USA")))

ws_data <- ws_data %>%
  mutate(consent = factor(consent,
                          levels = c("yes", "no"),
                          labels = c("Yes", "No")))

ws_data <- ws_data %>%
  mutate(water_source_type = factor(water_source_type,
                                    levels = c("open_well", "closed_well", "private_tap", "public_tap",
                                               "surface_water", "spring", "tank", "rainfall"),
                                    labels = c("Open Well", "Closed Well", "Private Tap", "Public Tap",
                                               "Surface Water", "Spring", "Tank", "Rainfall")))

ws_data <- ws_data %>%
  mutate(manager_gender = factor(manager_gender,
                                 levels = c("male", "female", "mixed"),
                                 labels = c("Male", "Female", "Mixed")))

ws_data <- ws_data %>%
  mutate(owner_type = factor(owner_type,
                             levels = c("individual", "household", "committee", "organization", "none"),
                             labels = c("Individual", "Household", "Committee", "Organization", "None")))

ws_data <- ws_data %>%
  mutate(funder = factor(funder,
                         levels = c("household_self", "community", "fcc", "national_gov",
                                    "freetown_ngo", "international_ngo"),
                         labels = c("Household/self", "Community", "FCC", "National Government",
                                    "Freetown-based NGO", "International NGO")))

ws_data <- ws_data %>%
  mutate(users_per_day = factor(users_per_day,
                                levels = c("0", "0_to_10", "10_to_20", "20_to_50", "50_to_100",
                                           "100_to_200", "200_to_300", "300_to_500",
                                           "500_to_1000", "1000+"),
                                labels = c("0", "1 to 10", "10 to 20", "20 to 50", "50 to 100",
                                           "100 to 200", "200 to 300", "300 to 500",
                                           "500 to 1000", "1000+")))

ws_data <- ws_data %>%
  mutate(access_rights = factor(access_rights,
                                levels = c("household_only", "friends_family", "community", "anybody"),
                                labels = c("Household", "Close friends and family", "Community", "Anybody")))

ws_data <- ws_data %>%
  mutate(days_per_week = factor(days_per_week,
                                levels = c("7_days", "4_6_days", "2_3_days", "1_day", "less_than_weekly"),
                                labels = c("7 days per week", "4-6 days per week", "2-3 days per week",
                                           "1 day per week", "Less than weekly")))

ws_data <- ws_data %>%
  mutate(daily_schedule = factor(daily_schedule,
                                 levels = c("all_day", "twice_day", "three_times", "random"),
                                 labels = c("All day", "Twice a day", "Three times a day",
                                            "Randomly throughout day")))

ws_data <- ws_data %>%
  mutate(opening_time = factor(opening_time,
                               levels = c("1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am",
                                          "9am", "10am", "11am", "afternoon", "evening"),
                               labels = c("1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am",
                                          "9am", "10am", "11am", "Afternoon", "Evening")))

ws_data <- ws_data %>%
  mutate(is_potable = factor(is_potable,
                             levels = c("1", "0", "unknown"),
                             labels = c("Yes", "No", "Unknown")))

ws_data <- ws_data %>%
  mutate(treatment_frequency = factor(treatment_frequency,
                                      levels = c("daily", "weekly", "monthly", "less_than_monthly", "never"),
                                      labels = c("Daily", "Weekly", "Monthly", "Less than Monthly", "Never")))

ws_data <- ws_data %>%
  mutate(price_change = factor(price_change,
                               levels = c("1", "0"),
                               labels = c("Yes", "No")))

ws_data <- ws_data %>%
  mutate(source_age = factor(source_age,
                             levels = c("6months", "6to12months", "1to5years", "5to10years",
                                        "10to20years", "20plusyears"),
                             labels = c("Less than 6 Months", "6 months to one year", "1 to 5 years",
                                        "5 to 10", "10 to 20", "20+ years")))

ws_data <- ws_data %>%
  mutate(month = factor(month,
                        levels = c("november", "december", "january", "february", "march", "april",
                                   "may", "june", "july", "august", "september", "october"),
                        labels = c("November", "December", "January", "February", "March", "April",
                                   "May", "June", "July", "August", "September", "October")))

ws_data <- ws_data %>%
  mutate(quoting = factor(quoting,
                          levels = c("no", "no_name", "with_name", "no_quotes"),
                          labels = c("No", "Yes, but don't use my name",
                                     "Yes, and you can use my name too.", "No useful quotes")))

# Zone Consolidation & Column Removal ####
ws_data <- ws_data %>%
  mutate(zone = coalesce(cocklezone, porteezone, dworzarkzone)) %>%
  select(-any_of(c("cocklezone", "porteezone", "dworzarkzone",
                   "consent", "picture_consent", "_version__", "audit", "instanceID", "instanceName",
                   "xform_id_string", "rootUuid", "status", "geolocation", "submission_time", "tags",
                   "notes", "submitted_by", "quoting", "surveyor_notes",
                   "start", "end", "validation_status", "guma", "respondent_name_pull",
                   "id", "end_time", "start_time"))) %>%
  select(-ends_with("_pull")) %>%
  mutate(source_photo = replace_na(source_photo, "No Photo")) %>%
  relocate(zone, .after = community) %>%
  relocate(gps_location, source_photo, display_month, .after = attachments) %>%
  relocate(uuid, month, .before = surveyor_name)


# WSID Handling ####
ws_data <- ws_data %>%
  mutate(wsid = source_ID) %>%
  select(-any_of("source_ID")) %>%
  relocate(wsid, .after = community)

# WSID Corrections ####
ws_data[ws_data$uuid == "507c314a-c321-4269-9af0-662cf605aefd", "wsid"] <- "D127"
ws_data[ws_data$uuid == "832d3a97-00d4-4d71-b800-5a504ffbe1e0", "wsid"] <- "D138"
ws_data[ws_data$uuid == "0dd30028-7743-4493-95b1-119b36130464", "wsid"] <- "D140"
ws_data[ws_data$uuid == "63bef5d0-cf98-44e2-b5b3-15262f51e41d", "wsid"] <- "D153"
ws_data[ws_data$uuid == "c9cb5eb2-05e9-4282-b840-012a5b80c400", "wsid"] <- "C001" 
ws_data[ws_data$uuid == "f16654a1-67da-408b-93d7-5b752bdb24e6", "wsid"] <- "C002" 
ws_data[ws_data$uuid == "0b7018d7-7d63-4de9-b73c-97125d4ebf7d", "wsid"] <- "C003" 
ws_data[ws_data$uuid == "ea74f24a-1932-422f-9a40-39f2b7df1d03", "wsid"] <- "C004" 
ws_data[ws_data$uuid == "0319f35d-33a4-402f-a9b1-d3f0c9d77fca", "wsid"] <- "C005" 
ws_data[ws_data$uuid == "3b295cf7-437b-4209-97ea-b9ead11a36ac", "wsid"] <- "C006" 
ws_data[ws_data$uuid == "a48ad3b9-0504-404c-9615-950e91eb7288", "wsid"] <- "C007" 
ws_data[ws_data$uuid == "3268ecc8-791b-49d5-b512-9d3fa43224b5", "wsid"] <- "C008" 
ws_data[ws_data$uuid == "30425489-8777-4cfe-afdc-4c2ba2b80169", "wsid"] <- "C009" 
ws_data[ws_data$uuid == "8c109526-7405-461b-9f60-c13561170a52", "wsid"] <- "C010" 
ws_data[ws_data$uuid == "8808b7f8-2e61-476a-b241-038f92981711", "wsid"] <- "C011" 
ws_data[ws_data$uuid == "1e54f8cd-18ef-4462-8d28-e03671499b57", "wsid"] <- "C012" 
ws_data[ws_data$uuid == "0a69f104-3b48-4812-ae00-281f756afaa2", "wsid"] <- "C013" 
ws_data[ws_data$uuid == "abf0f2bd-9c02-4ed0-ae1e-b9c983d8e617", "wsid"] <- "C014" 
ws_data[ws_data$uuid == "ddebaedf-b5c5-492c-9b6d-a3216b8093f5", "wsid"] <- "C015" 
ws_data[ws_data$uuid == "07ea899c-c321-4222-bddb-e7739bec8844", "wsid"] <- "C016" 
ws_data[ws_data$uuid == "7fe30a70-0491-4718-9160-53e6d527c094", "wsid"] <- "C017" 
ws_data[ws_data$uuid == "2e2ae0b7-0082-40e7-997b-e0d8822b8caa", "wsid"] <- "C018" 
ws_data[ws_data$uuid == "24a22646-9113-4438-ae4f-1ed984eddfc6", "wsid"] <- "C019" 
ws_data[ws_data$uuid == "a8d2a6ff-fbeb-4256-a797-16ddd2f615ec", "wsid"] <- "C020" 
ws_data[ws_data$uuid == "4a0180d0-2349-437f-9c89-7116fd11ce64", "wsid"] <- "C021" 
ws_data[ws_data$uuid == "c0aa6249-b661-4b5c-9dd4-3249fd480ffd", "wsid"] <- "C022" 
ws_data[ws_data$uuid == "a653578b-2561-4164-999c-41d1885bce8f", "wsid"] <- "C023" 
ws_data[ws_data$uuid == "1caa272e-0f07-425a-b19e-0266832eabc6", "wsid"] <- "C024" 
ws_data[ws_data$uuid == "93658744-e449-4848-b98b-6e20546d3e8f", "wsid"] <- "C025" 
ws_data[ws_data$uuid == "04bd0a3c-1028-405d-9b66-fbc5680e38a9", "wsid"] <- "C026" 
ws_data[ws_data$uuid == "a8348737-5458-47b5-b91b-43faa0ecb27f", "wsid"] <- "C027" 
ws_data[ws_data$uuid == "116f63c3-489d-43fd-a9c5-1009df54f88c", "wsid"] <- "C028" 
ws_data[ws_data$uuid == "4cb32ab5-0aa1-4b2b-8822-c1d4530e0180", "wsid"] <- "C029" 
ws_data[ws_data$uuid == "b711d55e-0094-42a0-acec-6c487e0a62fb", "wsid"] <- "C030" 
ws_data[ws_data$uuid == "442822ed-fc8a-42e1-84f3-61de0be97071", "wsid"] <- "C031" 
ws_data[ws_data$uuid == "167bffc1-7a6b-49e5-a801-a180f2dc0601", "wsid"] <- "C032" 
ws_data[ws_data$uuid == "0d6839a5-59c4-44d5-809c-79d6dca702e6", "wsid"] <- "C033" 
ws_data[ws_data$uuid == "34df99ba-c5df-4bd6-aece-92670b8de237", "wsid"] <- "C034" 
ws_data[ws_data$uuid == "0873c2a7-6ed8-43a1-8e2b-03c13e9a09d1", "wsid"] <- "C035" 
ws_data[ws_data$uuid == "6c84a9d6-8234-4f5d-a431-0ec8c2b8f6a1", "wsid"] <- "D001" 
ws_data[ws_data$uuid == "12d6a5bf-8071-4feb-b576-4894a21c7982", "wsid"] <- "D002" 
ws_data[ws_data$uuid == "a3460870-4c29-4d8e-9109-20a9764d00ef", "wsid"] <- "D003" 
ws_data[ws_data$uuid == "068bb4d9-55ee-4c3a-bc2f-bcb4e2181860", "wsid"] <- "D004" 
ws_data[ws_data$uuid == "e9b8fbf6-5adc-40f6-a470-8cbb304fa325", "wsid"] <- "D005" 
ws_data[ws_data$uuid == "173cde3e-739d-4179-aa5c-5504123818ff", "wsid"] <- "D006" 
ws_data[ws_data$uuid == "5fe28aa0-3825-4745-9761-e56e9c8693f1", "wsid"] <- "D007" 
ws_data[ws_data$uuid == "21466ef8-a716-4e35-b2e3-12623ed4a08c", "wsid"] <- "D008" 
ws_data[ws_data$uuid == "7f307137-a03d-412a-b1f5-9e3a9314d91a", "wsid"] <- "D009" 
ws_data[ws_data$uuid == "70f2539d-9b63-4583-bcaa-601d6561fce3", "wsid"] <- "D010" 
ws_data[ws_data$uuid == "a83c4b48-8dbb-4f6e-ab61-454df86b493e", "wsid"] <- "D011" 
ws_data[ws_data$uuid == "a5a5ece5-0756-4a3b-b224-feaf992b5355", "wsid"] <- "D012" 
ws_data[ws_data$uuid == "83a721af-f52b-4439-8128-bf0e995a8ccc", "wsid"] <- "D013" 
ws_data[ws_data$uuid == "7cbc2969-0085-4371-bf8b-52f6020712dd", "wsid"] <- "D014" 
ws_data[ws_data$uuid == "8b2c20eb-0dcc-4ff7-89b5-423e7b527a25", "wsid"] <- "D015" 
ws_data[ws_data$uuid == "a9665ac5-d477-42a8-9758-8a8b3a54615b", "wsid"] <- "D016" 
ws_data[ws_data$uuid == "c9083a42-1b04-4776-9606-e40d641172d7", "wsid"] <- "D017" 
ws_data[ws_data$uuid == "a6beb35d-0694-41cd-b4ec-e5675857f88f", "wsid"] <- "D018" 
ws_data[ws_data$uuid == "2158be3a-bad4-4dbf-a02c-7771ab1b2143", "wsid"] <- "D019" 
ws_data[ws_data$uuid == "80c912be-8baa-46ed-bcc9-ed5ed3be0d1d", "wsid"] <- "D020" 
ws_data[ws_data$uuid == "b67ed60c-67a9-4768-918e-9295c795276e", "wsid"] <- "D021" 
ws_data[ws_data$uuid == "8b123a97-808d-4c99-bb86-868d0f9a7579", "wsid"] <- "D022" 
ws_data[ws_data$uuid == "b4f9ef4c-baeb-4714-bc82-63f178948fc9", "wsid"] <- "D023" 
ws_data[ws_data$uuid == "6c8b0a81-d84c-4104-9bec-3d0a07482ff4", "wsid"] <- "D024" 
ws_data[ws_data$uuid == "df13444a-0ad7-4cf8-bf72-655df7f5dcfb", "wsid"] <- "D025" 
ws_data[ws_data$uuid == "3fc5bd2c-c63e-4d00-b38e-da92a4c132c3", "wsid"] <- "D026" 
ws_data[ws_data$uuid == "ad764171-d866-4c84-a232-79cc32103ae4", "wsid"] <- "D027" 
ws_data[ws_data$uuid == "1bbcd68b-8607-462e-86db-2d5bfd465422", "wsid"] <- "D028" 
ws_data[ws_data$uuid == "5dc59a74-8f63-4774-b059-f0014c5c0004", "wsid"] <- "D029" 
ws_data[ws_data$uuid == "5ffd3b31-fceb-44e7-954b-90b80b4117e2", "wsid"] <- "D030" 
ws_data[ws_data$uuid == "bea701f6-8b2b-4067-8773-27570c76094a", "wsid"] <- "D031" 
ws_data[ws_data$uuid == "8dc069d0-ddd9-498c-a8d0-7fa663e5af32", "wsid"] <- "D032" 
ws_data[ws_data$uuid == "422afaa9-6734-440e-8223-adc522b60b03", "wsid"] <- "D033" 
ws_data[ws_data$uuid == "4ade334b-5208-4ee5-a5cb-23af4adb5c5f", "wsid"] <- "D034" 
ws_data[ws_data$uuid == "fb824fa2-9a29-491b-b5b8-3f940824d1e3", "wsid"] <- "D035" 
ws_data[ws_data$uuid == "b18a3594-1f85-4134-bbd6-b6d0660697cc", "wsid"] <- "D036" 
ws_data[ws_data$uuid == "2f069d1d-be9b-4d12-b4f4-5c2ee1ef8e1c", "wsid"] <- "D037" 
ws_data[ws_data$uuid == "cf780d2d-116d-42ff-a2fb-4a07c095b840", "wsid"] <- "D038" 
ws_data[ws_data$uuid == "142949db-26ad-416d-8ddd-728f4e35c867", "wsid"] <- "D039" 
ws_data[ws_data$uuid == "cb08e319-1ec5-42a9-a0e7-a2e70356592f", "wsid"] <- "D040" 
ws_data[ws_data$uuid == "a11c553f-0579-4064-afd4-42c6952ec1eb", "wsid"] <- "D041" 
ws_data[ws_data$uuid == "66d14651-0dfd-41c7-be23-62bff653df3b", "wsid"] <- "D042" 
ws_data[ws_data$uuid == "82767895-ca4d-4e70-9147-cb747e6b0d58", "wsid"] <- "D043" 
ws_data[ws_data$uuid == "9ea48050-437b-4b62-ab22-d3808007bccb", "wsid"] <- "D044" 
ws_data[ws_data$uuid == "be6fb0cb-3b6b-4e60-be03-cd74ce9dc51e", "wsid"] <- "D045" 
ws_data[ws_data$uuid == "e9e41ab3-802c-4c38-8206-d87431c90dc8", "wsid"] <- "D046" 
ws_data[ws_data$uuid == "87324255-26b7-435b-b843-c5399f2162e5", "wsid"] <- "D047" 
ws_data[ws_data$uuid == "acab46de-2d8a-4acd-b767-b892b2ed11e5", "wsid"] <- "D048" 
ws_data[ws_data$uuid == "199107f1-fe56-485d-9149-fbae1c5799b1", "wsid"] <- "D049" 
ws_data[ws_data$uuid == "e7663e8b-9b25-469c-a83e-ba8d898423dd", "wsid"] <- "D050" 
ws_data[ws_data$uuid == "0523749b-0f33-4842-86a0-d21f50395403", "wsid"] <- "D051" 
ws_data[ws_data$uuid == "59985923-b935-4d98-8632-1e684e11e25c", "wsid"] <- "D052" 
ws_data[ws_data$uuid == "5a036057-a1bf-43c1-aadd-bbeef2d9afc4", "wsid"] <- "D053" 
ws_data[ws_data$uuid == "b6ca17cd-6a1f-4cba-859f-19f91fd21c3c", "wsid"] <- "D054" 
ws_data[ws_data$uuid == "4e9847cf-e23f-43e1-b863-e13e9251def7", "wsid"] <- "D055" 
ws_data[ws_data$uuid == "5a7f2210-086c-4a75-aa93-c15902b528b4", "wsid"] <- "D056" 
ws_data[ws_data$uuid == "b5ecd74c-3496-4c6e-afe8-e4fc5ee1e707", "wsid"] <- "D057" 
ws_data[ws_data$uuid == "833cee57-75e4-47db-af25-deaf59c14597", "wsid"] <- "D058" 
ws_data[ws_data$uuid == "f640e9db-d9a9-42fe-96d3-bb8caa94a8dc", "wsid"] <- "D059" 
ws_data[ws_data$uuid == "e022c950-2877-4ae3-af8a-6470752e1ecd", "wsid"] <- "D060" 
ws_data[ws_data$uuid == "d5005ed9-e864-4ef1-a0d2-32c095ce0e37", "wsid"] <- "D061" 
ws_data[ws_data$uuid == "3c8f2de1-cecd-4d9b-95a3-425e21982213", "wsid"] <- "D062" 
ws_data[ws_data$uuid == "623e76a1-9e10-4c64-a3e2-b47f9dee753b", "wsid"] <- "D063" 
ws_data[ws_data$uuid == "72236f96-7f89-4d13-8c4f-fb2e6b4349e0", "wsid"] <- "D064" 
ws_data[ws_data$uuid == "88ac6a32-da63-4a8c-8f95-55c04902ee3e", "wsid"] <- "D065" 
ws_data[ws_data$uuid == "3450f005-e6bd-4e1e-9be0-543769c4b475", "wsid"] <- "D066" 
ws_data[ws_data$uuid == "74a60d85-8a3a-4423-b29c-e893311d406e", "wsid"] <- "D067" 
ws_data[ws_data$uuid == "5353a3be-3843-456b-a066-91e496455bf9", "wsid"] <- "D068" 
ws_data[ws_data$uuid == "cc66eb7b-16a3-413b-8e28-6af0e958f558", "wsid"] <- "D069" 
ws_data[ws_data$uuid == "bb05eca5-3ee7-4adf-a766-a6236350840c", "wsid"] <- "D070" 
ws_data[ws_data$uuid == "9d160668-e238-4f3b-a2a5-b5b1be40f0e1", "wsid"] <- "D071" 
ws_data[ws_data$uuid == "2ba37bb7-893c-4a99-955c-209a2f03182e", "wsid"] <- "D072" 
ws_data[ws_data$uuid == "69b90dd7-1b1f-4dec-ba98-77487689f8ab", "wsid"] <- "D073" 
ws_data[ws_data$uuid == "271e61ba-ad27-4715-be4a-c84f2a1deab9", "wsid"] <- "D074" 
ws_data[ws_data$uuid == "830d8597-d9de-45ad-864c-ff79f9ff4a97", "wsid"] <- "D075" 
ws_data[ws_data$uuid == "3d350dcf-6eaa-4df0-8d13-b61b972f2554", "wsid"] <- "D076" 
ws_data[ws_data$uuid == "fb2fcf4b-744c-4b4b-ac5d-65c8b044ac4c", "wsid"] <- "D077" 
ws_data[ws_data$uuid == "71c9d7dd-bbd7-49af-b470-138b22a2af85", "wsid"] <- "D078" 
ws_data[ws_data$uuid == "1f43cf2a-b4c0-40dc-89a4-a31a0b913449", "wsid"] <- "D079" 
ws_data[ws_data$uuid == "0fecafc0-80af-412b-b324-a28a09135b55", "wsid"] <- "D080" 
ws_data[ws_data$uuid == "6d99d65c-728c-478e-b981-24d3ea726aaa", "wsid"] <- "D081" 
ws_data[ws_data$uuid == "296251a9-840d-4f2f-a1cb-48e4fb624bdc", "wsid"] <- "D082" 
ws_data[ws_data$uuid == "dc00fb12-0741-49b2-98c8-828bd4c9349c", "wsid"] <- "D083" 
ws_data[ws_data$uuid == "0e2c86d5-fd0a-4ef1-bc97-ae0b3753073b", "wsid"] <- "D084" 
ws_data[ws_data$uuid == "ebfb1913-26fe-4f98-b7e5-7dacb4d63aa6", "wsid"] <- "D085" 
ws_data[ws_data$uuid == "cf932071-6551-44d2-8751-6403968b5157", "wsid"] <- "D086" 
ws_data[ws_data$uuid == "66bba983-f4bc-4fd7-9440-2f1a31217366", "wsid"] <- "D087" 
ws_data[ws_data$uuid == "fe8edcfd-d6e2-4cb1-b4c5-4ecd7a9a7392", "wsid"] <- "D088" 
ws_data[ws_data$uuid == "8fc4fe6a-3683-4cff-be30-c465a0688222", "wsid"] <- "D089" 
ws_data[ws_data$uuid == "64bd4bc2-cc95-4bc0-acc0-cce708954969", "wsid"] <- "D090" 
ws_data[ws_data$uuid == "ed0acb58-9d67-4932-9462-3f0a3767ab83", "wsid"] <- "D091" 
ws_data[ws_data$uuid == "3ddc613b-c3b1-445a-ae68-d1349ef3f9d8", "wsid"] <- "D092" 
ws_data[ws_data$uuid == "e4b91629-29a9-4012-be25-c0a287a6ab9f", "wsid"] <- "D093" 
ws_data[ws_data$uuid == "ddebded1-64b7-42c4-b1af-461cc41ad0a4", "wsid"] <- "D094" 
ws_data[ws_data$uuid == "c9101077-45bc-4aab-b7b5-2303fd4b2832", "wsid"] <- "D095" 
ws_data[ws_data$uuid == "1b394e64-b43c-4bbb-aa3a-46c105607834", "wsid"] <- "D096" 
ws_data[ws_data$uuid == "d0bcda65-c5fb-4804-9591-219e45603153", "wsid"] <- "D097" 
ws_data[ws_data$uuid == "3dfc0064-a2dc-4a27-a153-8b92f682530d", "wsid"] <- "D098" 
ws_data[ws_data$uuid == "73ea109c-aaa1-4cbb-8725-15a3ad6a7083", "wsid"] <- "D099" 
ws_data[ws_data$uuid == "07f899e4-09ab-4976-851e-24649547a9a9", "wsid"] <- "D100" 
ws_data[ws_data$uuid == "951fe803-0507-4edd-abb8-1c834b677290", "wsid"] <- "D101" 
ws_data[ws_data$uuid == "7dd24a6c-85b0-4f89-89cb-fdb1e547b5a6", "wsid"] <- "D102" 
ws_data[ws_data$uuid == "b0b1bd00-9dd0-45af-af00-df08af5abc37", "wsid"] <- "D103" 
ws_data[ws_data$uuid == "d750e54d-59d0-4c9d-87ee-af97b64d7c7c", "wsid"] <- "D104" 
ws_data[ws_data$uuid == "558da352-a325-4c19-92b9-ab50b44ca9f5", "wsid"] <- "D105" 
ws_data[ws_data$uuid == "06ec8d8d-c2d2-49e6-87fe-baa4b6196d9d", "wsid"] <- "D106" 
ws_data[ws_data$uuid == "8eee7884-667b-407c-aaf8-af698b97e750", "wsid"] <- "D107" 
ws_data[ws_data$uuid == "11f7fb53-3dad-498d-af8f-531037fcee5f", "wsid"] <- "D108" 
ws_data[ws_data$uuid == "fce203c6-d2ee-40ff-80ae-b73e75b22a8b", "wsid"] <- "D109" 
ws_data[ws_data$uuid == "3695047b-01be-40b6-a58a-ea49cfc6a5b9", "wsid"] <- "D110" 
ws_data[ws_data$uuid == "0a3cada3-0e21-4590-84ae-8e6d0b492343", "wsid"] <- "D111" 
ws_data[ws_data$uuid == "18deab1a-a775-4946-aea8-332fb950ac05", "wsid"] <- "D112" 
ws_data[ws_data$uuid == "177bc232-9c75-48c3-b456-83e479d15612", "wsid"] <- "D113" 
ws_data[ws_data$uuid == "1e61465d-a14f-47f6-9f7d-9732c9e4354c", "wsid"] <- "D114" 
ws_data[ws_data$uuid == "d995c439-82a1-4df5-ba78-99cbcb2da9ff", "wsid"] <- "D115" 
ws_data[ws_data$uuid == "712af73d-747f-442d-89df-7eeb9e3168d4", "wsid"] <- "D116" 
ws_data[ws_data$uuid == "45d997bd-81b3-49a8-89ed-0e8a4da94b91", "wsid"] <- "D117" 
ws_data[ws_data$uuid == "0bc563b6-0f58-474c-8553-172f7420a7c8", "wsid"] <- "D118" 
ws_data[ws_data$uuid == "cdab37fb-4382-484d-b198-def335f37f3c", "wsid"] <- "D119" 
ws_data[ws_data$uuid == "d2dd4201-ab12-418f-bdea-256503d56b91", "wsid"] <- "D120" 
ws_data[ws_data$uuid == "a2db3467-f575-4262-af72-3d81372a10e0", "wsid"] <- "D121" 
ws_data[ws_data$uuid == "60260a13-47d2-486a-afc1-9a229a1da2ca", "wsid"] <- "D122" 
ws_data[ws_data$uuid == "8e66c397-1e3c-418b-a748-b1b2aa5471b1", "wsid"] <- "D123" 
ws_data[ws_data$uuid == "2ac7ef62-cccc-471f-89b8-614c53efd5fa", "wsid"] <- "D124" 
ws_data[ws_data$uuid == "6176979e-cb71-4be2-aaf2-bf949fb2caac", "wsid"] <- "D125" 
ws_data[ws_data$uuid == "3a4e80a9-01b6-423d-b0c9-a30cef32203f", "wsid"] <- "D126" 
ws_data[ws_data$uuid == "dd5b2354-b769-40bc-9f09-17f61a2c0f1a", "wsid"] <- "D128" 
ws_data[ws_data$uuid == "2d80f5a5-db0a-4d9e-85f3-496ee57e5a57", "wsid"] <- "D129" 
ws_data[ws_data$uuid == "d949566d-4238-4650-8d58-32fe58f088c2", "wsid"] <- "D130" 
ws_data[ws_data$uuid == "618fc5b4-8456-475b-99c0-d91a858e7cbd", "wsid"] <- "D131" 
ws_data[ws_data$uuid == "58563d66-0bfe-43c9-850c-62496374fb3b", "wsid"] <- "D132" 
ws_data[ws_data$uuid == "28f85aad-0761-4341-8571-2213cc673685", "wsid"] <- "D133" 
ws_data[ws_data$uuid == "6545d73b-9075-4ee2-86c4-6b6f8551b475", "wsid"] <- "D134" 
ws_data[ws_data$uuid == "b346733a-8471-48a8-9268-9f7b1be9b258", "wsid"] <- "D135" 
ws_data[ws_data$uuid == "fe26e396-6073-497f-b0ce-8c0791b3ae8a", "wsid"] <- "D136" 
ws_data[ws_data$uuid == "808616fd-d835-49d9-9cbe-0f7efbe01a41", "wsid"] <- "D137" 
ws_data[ws_data$uuid == "463d8ca3-1eb1-4e09-a604-58b9df6671f9", "wsid"] <- "D139" 
ws_data[ws_data$uuid == "79e0c889-1ad4-490f-853b-210168ab6d4c", "wsid"] <- "D141" 
ws_data[ws_data$uuid == "467b11dd-44fb-444d-9582-9a70a9a05961", "wsid"] <- "D142" 
ws_data[ws_data$uuid == "a503dcc6-9845-40a6-a2da-0030122ba979", "wsid"] <- "D143" 
ws_data[ws_data$uuid == "8dbf765f-4663-45bd-85b7-19ea25bf752e", "wsid"] <- "D144" 
ws_data[ws_data$uuid == "62887c8c-d02e-400d-8531-fab833d04f10", "wsid"] <- "D145" 
ws_data[ws_data$uuid == "11fa7640-bec2-40ce-b7e0-a7f52ccce737", "wsid"] <- "D146" 
ws_data[ws_data$uuid == "2e9113ef-68d4-48ce-846f-3a5085bc5dc0", "wsid"] <- "D147" 
ws_data[ws_data$uuid == "30fd3ad1-19f9-4c15-a983-06c096cb2877", "wsid"] <- "D148" 
ws_data[ws_data$uuid == "6bdeb09e-2cdc-4b35-8c03-e2b1d7b0a279", "wsid"] <- "D149" 
ws_data[ws_data$uuid == "900d79e1-92f0-433b-a6f8-52639ab24f2a", "wsid"] <- "D150" 
ws_data[ws_data$uuid == "401afcb9-6bb5-4644-9d48-d485d991a26d", "wsid"] <- "D151" 
ws_data[ws_data$uuid == "b3a461aa-c074-4844-abe7-bdcc6a5b74d2", "wsid"] <- "D152" 
ws_data[ws_data$uuid == "67a1fcda-7f3e-4db4-b172-1a38483b275c", "wsid"] <- "D154" 
ws_data[ws_data$uuid == "74f2d77e-681e-468b-9e7c-65ca8b9541a5", "wsid"] <- "D155" 
ws_data[ws_data$uuid == "b39e4524-44a0-400c-8ca6-c5b5655cb164", "wsid"] <- "D156" 
ws_data[ws_data$uuid == "b5bc8a3b-50e9-4e52-aae2-999f5c328d25", "wsid"] <- "D157" 
ws_data[ws_data$uuid == "7584d12b-14e3-44ff-9cb4-62f9f5d30fe6", "wsid"] <- "D158" 
ws_data[ws_data$uuid == "5cec4a80-1abc-410b-91e1-dc159dca89e2", "wsid"] <- "D159" 
ws_data[ws_data$uuid == "49bd4e74-9af5-4785-b639-fb24457ef9d3", "wsid"] <- "D160" 
ws_data[ws_data$uuid == "cffb9e82-390b-4ea9-8652-4aa48f081be1", "wsid"] <- "D161" 
ws_data[ws_data$uuid == "92d40413-5ff4-4351-bbec-469c876bad99", "wsid"] <- "D162" 
ws_data[ws_data$uuid == "8c82358a-c85b-4fd9-8770-de0b0c43c5ea", "wsid"] <- "D163" 
ws_data[ws_data$uuid == "1c7f980a-a428-4c58-8b8d-411cbb18bae9", "wsid"] <- "D164" 
ws_data[ws_data$uuid == "a729b65d-9329-48fb-8869-e77b984d1664", "wsid"] <- "D165" 
ws_data[ws_data$uuid == "2d431b58-27be-4577-a189-d2e4ae8d8c1d", "wsid"] <- "D166" 
ws_data[ws_data$uuid == "dc9cd7e3-6821-4054-af49-de5c64d50739", "wsid"] <- "D167" 
ws_data[ws_data$uuid == "6d6e949d-829d-4845-9a80-1e3dd9ca8b71", "wsid"] <- "D168" 
ws_data[ws_data$uuid == "797c386b-4147-4084-a5ba-82cc01a0102e", "wsid"] <- "D169" 
ws_data[ws_data$uuid == "9754d87d-3365-4d70-88ca-b960607575a0", "wsid"] <- "D171" 
ws_data[ws_data$uuid == "12fc60da-3136-4ebf-9cc4-90bdecb1ca2e", "wsid"] <- "P001" 
ws_data[ws_data$uuid == "66af76d3-686f-4097-96cb-e0285c86ecb0", "wsid"] <- "P002" 
ws_data[ws_data$uuid == "cad31398-fd1d-429b-8493-4ba98568240c", "wsid"] <- "P003" 
ws_data[ws_data$uuid == "7abddd24-621e-4528-ad00-0f4983f32daa", "wsid"] <- "P004" 
ws_data[ws_data$uuid == "e1461ccb-f514-4fc9-b676-64317e21f720", "wsid"] <- "P005" 
ws_data[ws_data$uuid == "76042478-f480-46a3-bc86-d21cd827c57c", "wsid"] <- "P006" 
ws_data[ws_data$uuid == "b5a3e664-6497-42c5-9724-846c8c66f66d", "wsid"] <- "P007" 
ws_data[ws_data$uuid == "5b0b68e9-e2c1-4ca0-81db-e9af63f4ca81", "wsid"] <- "P008" 
ws_data[ws_data$uuid == "894d6df8-059e-4c8c-aca7-aa164a380009", "wsid"] <- "P009" 
ws_data[ws_data$uuid == "89aaf60b-dc5b-497c-912d-028b27a8fc6f", "wsid"] <- "P010" 
ws_data[ws_data$uuid == "26d8c43f-0faa-4667-afcc-76537280f1bb", "wsid"] <- "P011" 
ws_data[ws_data$uuid == "1510890b-1fce-4161-90a1-326cbcd0e74f", "wsid"] <- "P012" 
ws_data[ws_data$uuid == "e126e332-523d-496f-bc8f-44174838422b", "wsid"] <- "P013" 
ws_data[ws_data$uuid == "4c1cdb50-4229-4d61-8a70-b475304584b0", "wsid"] <- "P014" 
ws_data[ws_data$uuid == "07709631-d181-4683-86b9-b323ae274792", "wsid"] <- "P015" 
ws_data[ws_data$uuid == "b061ef8f-623e-44f9-ae07-8f89387d3412", "wsid"] <- "P016" 
ws_data[ws_data$uuid == "21aea4e0-e903-49dc-962c-336bf2df132b", "wsid"] <- "P017" 
ws_data[ws_data$uuid == "f0a0a197-83fb-4dc1-a05b-222fdd7078c3", "wsid"] <- "P018" 
ws_data[ws_data$uuid == "7ffbd008-b492-4385-82fa-b7ee6081a810", "wsid"] <- "P019" 
ws_data[ws_data$uuid == "5c0c07bf-5f67-44ba-a4ba-07890d585edb", "wsid"] <- "P020" 
ws_data[ws_data$uuid == "fa6f4e2e-1b40-441f-9d8d-44cfa4eac2ab", "wsid"] <- "P021" 
ws_data[ws_data$uuid == "ff50c58c-b0e6-482f-9e13-ecac7748ea9d", "wsid"] <- "P022" 
ws_data[ws_data$uuid == "fe8122e8-a18d-4d4c-9ecf-de24b623a688", "wsid"] <- "P023" 
ws_data[ws_data$uuid == "91f28f70-c789-45b5-b6c2-c8c66f3a02cc", "wsid"] <- "P024" 
ws_data[ws_data$uuid == "a1c8271b-e1fa-402b-ae43-78d418ae042e", "wsid"] <- "P025" 
ws_data[ws_data$uuid == "79923822-7b76-4fdc-b83f-ff2e21c13a55", "wsid"] <- "P026" 
ws_data[ws_data$uuid == "3c116067-d6f1-494e-9f7a-52744f0c868a", "wsid"] <- "P027" 
ws_data[ws_data$uuid == "fd0478a7-4a9e-4677-aeff-43c975f26470", "wsid"] <- "P028" 
ws_data[ws_data$uuid == "88b2f5d7-80f7-49fc-9701-6d3fa3c97569", "wsid"] <- "P029" 
ws_data[ws_data$uuid == "33851bd0-388d-488a-bf20-47ef6aae3657", "wsid"] <- "P030" 
ws_data[ws_data$uuid == "97c979e3-d848-4540-bdb6-6385db9192ba", "wsid"] <- "P031" 
ws_data[ws_data$uuid == "d7553a2a-9f39-4b40-b202-9ccf5352353c", "wsid"] <- "P032" 
ws_data[ws_data$uuid == "7c37ceed-42c0-491b-91ce-99121e050795", "wsid"] <- "P033" 
ws_data[ws_data$uuid == "5197f158-c999-47eb-a97a-71da48f70f80", "wsid"] <- "P034" 
ws_data[ws_data$uuid == "39304540-f685-43ad-8679-f5ddc8fe6461", "wsid"] <- "P035" 
ws_data[ws_data$uuid == "232a11cc-fc49-4f4f-853e-caddef632ed7", "wsid"] <- "P036" 
ws_data[ws_data$uuid == "0ad7b9e0-556d-42d9-ab78-1dac3b949e54", "wsid"] <- "P037" 
ws_data[ws_data$uuid == "639f8e4f-bb6e-42ff-88ad-0810478b410d", "wsid"] <- "P038" 
ws_data[ws_data$uuid == "bb7d3b48-5edb-4758-a1b8-9952dc60b46e", "wsid"] <- "P039" 
ws_data[ws_data$uuid == "3052b388-a9f5-4f38-821c-3a1b7427f06f", "wsid"] <- "P040" 
ws_data[ws_data$uuid == "9c6180f6-56d8-4aac-895d-e63e5b67f6a9", "wsid"] <- "P041" 
ws_data[ws_data$uuid == "9e3f7058-ba32-4896-a159-c1db787e83d5", "wsid"] <- "P042" 
ws_data[ws_data$uuid == "cfe17f38-5b48-45e6-bdcc-b59fe8f8fe95", "wsid"] <- "P043" 
ws_data[ws_data$uuid == "f9ec8247-8032-41f1-9ce2-2162e71629a9", "wsid"] <- "P044" 
ws_data[ws_data$uuid == "5cee4c98-05ec-46c1-a2e3-02a2ad3372ce", "wsid"] <- "P045" 
ws_data[ws_data$uuid == "0613f1ab-8ddd-4c28-a0ee-8790a5cb2ebe", "wsid"] <- "P046" 
ws_data[ws_data$uuid == "deab5d19-2ffd-4f78-9b00-cf6c3a540ca0", "wsid"] <- "P047" 
ws_data[ws_data$uuid == "768bc552-00a3-4484-9e3a-2cb9e359547e", "wsid"] <- "P048" 
ws_data[ws_data$uuid == "48f11b8b-1b6c-4b24-a9f7-070771738abb", "wsid"] <- "P049" 
ws_data[ws_data$uuid == "605846f5-15b2-4121-a22d-cc49a55ee116", "wsid"] <- "P050" 
ws_data[ws_data$uuid == "da7095b9-c8f2-4530-a5f7-adeeaef5152c", "wsid"] <- "P051" 
ws_data[ws_data$uuid == "607b184f-9fc8-47d5-9d8b-466323ca7c0a", "wsid"] <- "P052" 
ws_data[ws_data$uuid == "5012e738-bbe4-45f9-a484-b5d8648c5647", "wsid"] <- "P053" 
ws_data[ws_data$uuid == "146095dd-880f-4c2c-84a7-e3716ee16012", "wsid"] <- "P054" 
ws_data[ws_data$uuid == "8ef10b4e-9482-4cfd-b303-11056a550809", "wsid"] <- "P055" 
ws_data[ws_data$uuid == "65730b72-501c-4ee1-b927-4f3c9a81533f", "wsid"] <- "P056" 
ws_data[ws_data$uuid == "a610b1d2-96c9-4c59-a8aa-5c30760608d4", "wsid"] <- "P057" 
ws_data[ws_data$uuid == "bcad97c2-dc3e-4022-b93d-5923a479b449", "wsid"] <- "P058" 
ws_data[ws_data$uuid == "1735d46d-a233-483e-86ba-77c13a933393", "wsid"] <- "P059" 
ws_data[ws_data$uuid == "bd9b7070-dc71-476e-b786-7904d73f75ce", "wsid"] <- "P060" 
ws_data[ws_data$uuid == "d6275c9d-9da2-4df5-b5b1-2686e8495bd0", "wsid"] <- "P061" 
ws_data[ws_data$uuid == "dd43e0f5-1157-4ab3-8811-ed92733c49a5", "wsid"] <- "P062" 
ws_data[ws_data$uuid == "2e7acaef-588e-46be-9f93-ecb49b92ba26", "wsid"] <- "P063" 
ws_data[ws_data$uuid == "8272dc33-580a-4f55-baf9-8ca5f9e8a204", "wsid"] <- "P064" 
ws_data[ws_data$uuid == "5c0f91fe-4abd-4610-a47c-0ac34059d711", "wsid"] <- "P065" 
ws_data[ws_data$uuid == "2eca450a-98cc-4067-a990-c32dd909fac0", "wsid"] <- "P066" 
ws_data[ws_data$uuid == "748f39bd-cf41-42bc-b7dd-cbdb87a2101f", "wsid"] <- "P067" 
ws_data[ws_data$uuid == "c26aa159-2ba2-4db1-9094-63191460b13d", "wsid"] <- "P068" 
ws_data[ws_data$uuid == "deb5b27c-3eab-450a-8d5f-fb6be3b93ee3", "wsid"] <- "P069" 
ws_data[ws_data$uuid == "661d62ab-4b59-415d-a630-1e6cacd593bf", "wsid"] <- "P070" 

# Vital Corrections ####
ws_data <- ws_data[ws_data$uuid != "6260d134-2d8b-4521-bb70-9df99dc3acab", ]
ws_data <- ws_data[ws_data$uuid != "ea2472f6-adfb-41b7-90da-50fb074cdbf3", ]
ws_data <- ws_data[ws_data$uuid != "1e7398b9-3642-4772-8158-f6e9cb6f7fcc", ]

# Individual Field Corrections ####
# community
ws_data[ws_data$source_photo == "1762854513791.jpg", "community"] <- "Dworzark"

# zone
ws_data[ws_data$source_photo == "1762854513791.jpg", "zone"] <- "Nigeria"
ws_data[ws_data$source_photo == "1763030485469.jpg", "zone"] <- "Portee Wharf"

# respondent_name
ws_data[ws_data$source_photo == "1762854513791.jpg", "respondent_name"] <- "Ibrahim Jalloh"
ws_data[ws_data$source_photo == "1762856784217.jpg", "respondent_name"] <- "Amanda Jones"

# manager_gender
ws_data[ws_data$source_photo == "1762865226663.jpg", "manager_gender"] <- "Female"
ws_data[ws_data$source_photo == "1763029369673.jpg", "manager_gender"] <- "Male"
ws_data[ws_data$source_photo == "1762853482960.jpg", "manager_gender"] <- "Female"

# owner_type
ws_data[ws_data$source_photo == "1762865226663.jpg", "owner_type"] <- "Household"
ws_data[ws_data$source_photo == "1763029369673.jpg", "owner_type"] <- "Committee"
ws_data[ws_data$source_photo == "1763205423836.jpg", "owner_type"] <- "None"

# source_age
ws_data[ws_data$source_photo == "1762865226663.jpg", "source_age"] <- "20+ years"
ws_data[ws_data$source_photo == "1763029369673.jpg", "source_age"] <- "20+ years"

# users_per_day
ws_data[ws_data$source_photo == "1763029369673.jpg", "users_per_day"] <- "0"
ws_data[ws_data$source_photo == "1763295731653.jpg", "users_per_day"] <- "50 to 100"

# access_rights
ws_data[ws_data$source_photo == "1762856519831.jpg", "access_rights"] <- "Anybody"
ws_data[ws_data$source_photo == "1762956342305.jpg", "access_rights"] <- "Anybody"
ws_data[ws_data$source_photo == "1762865592920.jpg", "access_rights"] <- "Community"
ws_data[ws_data$source_photo == "1762867216335.jpg", "access_rights"] <- "Community"

# price_per_jerrycan
ws_data[ws_data$source_photo == "1762856519831.jpg", "price_per_jerrycan"] <- 1
ws_data[ws_data$source_photo == "1762956342305.jpg", "price_per_jerrycan"] <- 1
ws_data[ws_data$source_photo == "1762865592920.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1762867216335.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763200512880.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763202790648.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763203566533.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763204742486.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763208229214.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763209899556.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763211388128.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763212023654.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763215463057.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763289940199.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763203995442.jpg", "price_per_jerrycan"] <- 0
ws_data[ws_data$source_photo == "1763210997150.jpg", "price_per_jerrycan"] <- 0

# price_per_month
ws_data[ws_data$source_photo == "1762856519831.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1762956342305.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1762865592920.jpg", "price_per_month"] <- 10
ws_data[ws_data$source_photo == "1762867216335.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763200512880.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763202790648.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763203566533.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763204742486.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763208229214.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763209899556.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763211388128.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763212023654.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763215463057.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763289940199.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763203995442.jpg", "price_per_month"] <- 0
ws_data[ws_data$source_photo == "1763210997150.jpg", "price_per_month"] <- 0

# price_change
ws_data[ws_data$source_photo == "1762856519831.jpg", "price_change"] <- "Yes"
ws_data[ws_data$source_photo == "1762956342305.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1762855960522.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1762865592920.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1762867216335.jpg", "price_change"] <- "Yes"
ws_data[ws_data$source_photo == "1763200512880.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763202790648.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763203566533.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763204742486.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763208229214.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763209899556.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763211388128.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763212023654.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763215463057.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763289940199.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763203995442.jpg", "price_change"] <- "No"
ws_data[ws_data$source_photo == "1763210997150.jpg", "price_change"] <- "No"

# days_per_week
ws_data[ws_data$source_photo == "1762870443737.jpg", "days_per_week"] <- "7 days per week"

# daily_schedule
ws_data[ws_data$source_photo == "1762870443737.jpg", "daily_schedule"] <- "All day"

# opening_time
ws_data[ws_data$source_photo == "1762870443737.jpg", "opening_time"] <- "5am"
ws_data[ws_data$source_photo == "1763118663989.jpg", "opening_time"] <- "7am"

# percent_women_gatherers
ws_data[ws_data$source_photo == "1763208229214.jpg", "percent_women_gatherers"] <- 60
ws_data[ws_data$source_photo == "1763211388128.jpg", "percent_women_gatherers"] <- 70
ws_data[ws_data$source_photo == "1763200512880.jpg", "percent_women_gatherers"] <- 70
ws_data[ws_data$source_photo == "1763207174198.jpg", "percent_women_gatherers"] <- 70
ws_data[ws_data$source_photo == "1763212023654.jpg", "percent_women_gatherers"] <- 75
ws_data[ws_data$source_photo == "1763215463057.jpg", "percent_women_gatherers"] <- 80
ws_data[ws_data$source_photo == "1763203048324.jpg", "percent_women_gatherers"] <- 80
ws_data[ws_data$source_photo == "1763210997150.jpg", "percent_women_gatherers"] <- 80
ws_data[ws_data$source_photo == "1763215236980.jpg", "percent_women_gatherers"] <- 80
ws_data[ws_data$source_photo == "1763289940199.jpg", "percent_women_gatherers"] <- 50

# GPS Location Filtering ####
buffer_distance <- 200
cockle_shapefile <- st_read("shapefiles/CockleBay.shp", quiet = TRUE)
portee_shapefile <- st_read("shapefiles/PorteeRokupa.shp", quiet = TRUE)
dworzark_shapefile <- st_read("shapefiles/Dworzark.shp", quiet = TRUE)
cockle_shapefile_utm <- st_transform(cockle_shapefile, crs = 32628)
portee_shapefile_utm <- st_transform(portee_shapefile, crs = 32628)
dworzark_shapefile_utm <- st_transform(dworzark_shapefile, crs = 32628)
cockle_buffer <- st_buffer(cockle_shapefile_utm, dist = buffer_distance)
portee_buffer <- st_buffer(portee_shapefile_utm, dist = buffer_distance)
dworzark_buffer <- st_buffer(dworzark_shapefile_utm, dist = buffer_distance)

kobo_spatial <- ws_data %>%
  filter(!is.na(gps_location)) %>%
  separate(gps_location, into = c("latitude", "longitude", "altitude", "accuracy"),
           sep = " ", remove = FALSE, convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

kobo_sf <- kobo_spatial %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32628)

cockle_inside <- kobo_sf %>%
  filter(community == "Cockle Bay") %>%
  mutate(inside = lengths(st_intersects(., cockle_buffer)) > 0)
portee_inside <- kobo_sf %>%
  filter(community == "Portee Rokupa") %>%
  mutate(inside = lengths(st_intersects(., portee_buffer)) > 0)
dworzark_inside <- kobo_sf %>%
  filter(community == "Dworzark") %>%
  mutate(inside = lengths(st_intersects(., dworzark_buffer)) > 0)

all_spatial <- bind_rows(cockle_inside, portee_inside, dworzark_inside)

# Remove outside GPS coordinates
outside_uuids <- all_spatial %>%
  filter(!inside) %>%
  pull(uuid)

ws_data <- ws_data %>%
  mutate(gps_location = if_else(uuid %in% outside_uuids, NA_character_, gps_location))

# Average GPS by wsid and apply back
mean_gps <- ws_data %>%
  filter(!is.na(gps_location), !is.na(wsid)) %>%
  separate(gps_location, into = c("latitude", "longitude", "altitude", "accuracy"),
           sep = " ", remove = FALSE, convert = TRUE) %>%
  group_by(wsid) %>%
  summarise(
    mean_lat = mean(latitude, na.rm = TRUE),
    mean_lon = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mean_gps = paste(mean_lat, mean_lon, "0 0"))

ws_data <- ws_data %>%
  left_join(mean_gps %>% select(wsid, mean_gps), by = "wsid") %>%
  mutate(gps_location = if_else(!is.na(mean_gps), mean_gps, gps_location)) %>%
  select(-mean_gps)

# Water Source Colors ####
ws_colors <- c(
  "Open Well" = "tan4",
  "Closed Well" = "tan2",
  "Private Tap" = "coral3",
  "Public Tap" = "coral1",
  "Surface Water" = "snow3",
  "Spring" = "snow1",
  "Tank" = "cornsilk1",
  "Sachet" = "steelblue1",
  "My own source" = "grey50",
  "Rainwater" = "navy"
)