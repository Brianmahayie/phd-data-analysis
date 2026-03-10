# HH Data Cleanup Script
# Pulls from KoboToolbox API, cleans, corrects, and outputs hh_data
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(sf)
library(readxl)
library(openxlsx)

# API Pull ####
kobo_token <- Sys.getenv("KOBO_TOKEN")
form_id <- "a7WdSwwL7Zug9hC98x5PRd"
base_url <- "https://kf.kobotoolbox.org/api/v2/assets/"

# Paginated API Pull
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
hh_data <- bind_rows(all_results)

# Column Name Cleaning ####
uuid_submission <- hh_data$`_uuid`

names(hh_data) <- names(hh_data) %>%
  sub(".*/", "", .) %>%
  sub("^_", "", .)

hh_data <- hh_data[, !duplicated(names(hh_data))]
hh_data$uuid <- uuid_submission

# Datetime & Date ####
hh_data <- hh_data %>%
  mutate(
    start_time = ymd_hms(start_time, tz = "UTC"),
    end_time = ymd_hms(end_time, tz = "UTC"),
    survey_date = as.Date(start_time),
    date_label = format(survey_date, "%b %d")
  )

# Numeric Conversions ####

# Factors ####
numeric_vars <- c("adult_men", "adult_women", "boys", "girls", "school_age_boys",
                  "school_age_girls", "boys_attend_school", "girls_attend_school",
                  "drinking_water_daily_spend", "domestic_water_daily_spend",
                  "drinking_walk_time", "drinking_wait_time", "drinking_trips",
                  "domestic_walk_time", "domestic_wait_time", "domestic_trips",
                  "sources_access", "sources_used", "daily_helpers", "jerrycans_men",
                  "jerrycans_women", "jerrycans_boys", "jerrycans_girls", "jerrycans_hired",
                  "boys_late_school", "girls_late_school", "stored_amount", "sick_water",
                  "injuries_water")

hh_data <- hh_data %>%
  mutate(across(any_of(numeric_vars), as.numeric))
hh_data$community <- factor(hh_data$community,
                            levels = c("cockle", "dworzark", "portee"),
                            labels = c("Cockle Bay", "Dworzark", "Portee Rokupa"))
zone_levels <- c("inletview", "jaymatta", "kolastick", "mafengbeh", "outside",
                 "benk", "mefleh", "porteewharf", "rokupawharf", "outside_p",
                 "argentina", "brazil", "cameroon", "england", "france",
                 "germany", "holland", "italy", "morocco", "nigeria", "spain", "usa")
zone_labels <- c("Inlet View", "Jay Matta", "Kola Stick", "Mafengbeh", "Outside of the Community",
                 "Benk", "Mefleh", "Portee Wharf", "Rokupa Wharf", "Outside of the Community",
                 "Argentina", "Brazil", "Cameroon", "England", "France",
                 "Germany", "Holland", "Italy", "Morocco", "Nigeria", "Spain", "USA")

hh_data$zone <- factor(hh_data$survey_zone, levels = zone_levels, labels = zone_labels)

if ("drinking_source_zone" %in% names(hh_data)) {
  hh_data$drinking_source_zone <- factor(hh_data$drinking_source_zone, levels = zone_levels, labels = zone_labels)
  hh_data <- hh_data %>% relocate(drinking_source_selection, .after = drinking_source_zone)
}

if ("domestic_source_zone" %in% names(hh_data)) {
  hh_data$domestic_source_zone <- factor(hh_data$domestic_source_zone, levels = zone_levels, labels = zone_labels)
}
hh_data$surveyor_name <- factor(hh_data$surveyor_name,
                                levels = c("abdulaib", "abdulaik", "boboieh", "jamiatu", "musa", "joana", "moses", "joseph", "sinneh", "other"),
                                labels = c("Abdulai B", "Abdulai K", "Boboieh", "Jamiatu", "Musa", "Joana", "Moses", "Joseph", "Sinneh", "Other"))
hh_data$survey_month <- factor(hh_data$survey_month,
                               levels = c("november", "december", "january", "february", "march", "april",
                                          "may", "june", "july", "august", "september", "october"),
                               labels = c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))
frequency_vars <- c("hwise_worry", "hwise_interrupted", "hwise_clothes", "hwise_schedule",
                    "hwise_food", "hwise_hand_washing", "hwise_body_washing", "hwise_drinking",
                    "hwise_angry", "hwise_thirsty", "hwise_no_water", "hwise_stigma",
                    "sachet_frequency", "women_danger_environment", "men_danger_environment",
                    "women_bullying_owner", "women_bullying_others", "women_danger_people",
                    "men_danger_people", "treatment_frequency")

for (var in frequency_vars) {
  if (var %in% names(hh_data)) {
    hh_data[[var]] <- factor(hh_data[[var]],
                             levels = c("never", "rarely", "sometimes", "often", "always", "dont_know", "not_applicable"),
                             labels = c("Never (0 times)", "Rarely (1-2 times)", "Sometimes (3-10 times)",
                                        "Often (11-20 times)", "Always (More than 20 times)", "Don't know", "Not Applicable"))
  }
}

# HWISE Scoring ####
hwise_vars <- c("hwise_worry", "hwise_interrupted", "hwise_clothes", "hwise_schedule",
                "hwise_food", "hwise_hand_washing", "hwise_body_washing", "hwise_drinking",
                "hwise_angry", "hwise_thirsty", "hwise_no_water", "hwise_stigma")
hwise_vars <- hwise_vars[hwise_vars %in% names(hh_data)]

hh_data <- hh_data %>%
  mutate(across(all_of(hwise_vars),
                ~ case_when(
                  grepl("Never", .) ~ 0L,
                  grepl("Rarely", .) ~ 1L,
                  grepl("Sometimes", .) ~ 2L,
                  grepl("Often|Always", .) ~ 3L,
                  TRUE ~ NA_integer_
                ),
                .names = "{.col}_score")) %>%
  mutate(hwise_total_score = rowSums(across(ends_with("_score") & starts_with("hwise_")), na.rm = TRUE)) %>%
  relocate(ends_with("_score") & starts_with("hwise_"), hwise_total_score, .after = any_of("hwise_stigma"))

yes_no_vars <- c("own_water_source", "source_meets_needs", "training_received",
                 "drinking_potable", "domestic_potable", "standing_water")

hh_data <- hh_data %>%
  mutate(across(any_of(yes_no_vars),
                ~factor(., levels = c("yes", "no"), labels = c("Yes", "No"))))
if ("respondent_gender" %in% names(hh_data)) {
  hh_data$respondent_gender <- factor(hh_data$respondent_gender,
                                      levels = c("male", "female"), labels = c("Male", "Female"))
}
if ("hh_gender" %in% names(hh_data)) {
  hh_data$hh_gender <- factor(hh_data$hh_gender,
                              levels = c("male", "female"), labels = c("Male", "Female"))
}
if ("share_source" %in% names(hh_data)) {
  hh_data$share_source <- factor(hh_data$share_source,
                                 levels = c("household_friends", "close_friends", "anybody"),
                                 labels = c("Household and close friends", "Close friends", "Anybody"))
}
source_type_levels <- c("openwell", "closedwell", "privatetap", "publictap",
                        "surfacewater", "spring", "tank", "sachet", "personal_source", "rainwater")
source_type_labels <- c("Open Well", "Closed Well", "Private Tap", "Public Tap",
                        "Surface Water", "Spring", "Tank", "Sachet", "My own source", "Rainwater")

if ("drinking_source_type" %in% names(hh_data)) {
  hh_data$drinking_source_type <- factor(hh_data$drinking_source_type,
                                         levels = source_type_levels, labels = source_type_labels)
}
if ("domestic_source_type" %in% names(hh_data)) {
  hh_data$domestic_source_type <- factor(hh_data$domestic_source_type,
                                         levels = source_type_levels, labels = source_type_labels)
}
if ("collection_time" %in% names(hh_data)) {
  hh_data$collection_time <- factor(hh_data$collection_time,
                                    levels = c("morning", "afternoon", "evening", "night", "varies"),
                                    labels = c("Morning", "Afternoon", "Evening", "Night (9pm-3am)", "Varies"))
}
if ("morning_specific_time" %in% names(hh_data)) {
  hh_data$morning_specific_time <- factor(hh_data$morning_specific_time,
                                          levels = c("1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12pm"),
                                          labels = c("1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12pm"))
}
if ("collection_freq" %in% names(hh_data)) {
  hh_data$collection_freq <- factor(hh_data$collection_freq,
                                    levels = c("three_per_day", "two_per_day", "daily", "every_2_days", "weekly", "biweekly", "tobiweekly"),
                                    labels = c("3 times per day", "2 times per day", "Daily", "Every 2 days", "Weekly", "Biweekly", "Up to biweekly"))
}

# Splitting Multi-Select Variables ####

if ("treatment_type" %in% names(hh_data)) {
  hh_data <- hh_data %>%
    mutate(
      treatment_chlorine = factor(ifelse(grepl("cholorine", treatment_type), "Yes", "No"), levels = c("Yes", "No")),
      treatment_simplefilter = factor(ifelse(grepl("simplefilter", treatment_type), "Yes", "No"), levels = c("Yes", "No")),
      treatment_advancefilter = factor(ifelse(grepl("advancefilter", treatment_type), "Yes", "No"), levels = c("Yes", "No")),
      treatment_boil = factor(ifelse(grepl("boil", treatment_type), "Yes", "No"), levels = c("Yes", "No")),
      treatment_settle = factor(ifelse(grepl("settle", treatment_type), "Yes", "No"), levels = c("Yes", "No")),
      treatment_other = factor(ifelse(grepl("other", treatment_type), "Yes", "No"), levels = c("Yes", "No"))
    ) %>%
    relocate(treatment_chlorine, treatment_simplefilter, treatment_advancefilter,
             treatment_boil, treatment_settle, treatment_other, .after = treatment_type) %>%
    select(-treatment_type)
}
if ("water_disease" %in% names(hh_data)) {
  hh_data <- hh_data %>%
    mutate(
      water_disease = ifelse(is.na(water_disease), "none", water_disease),
      disease_malaria = factor(ifelse(grepl("malaria", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_dengue = factor(ifelse(grepl("dengue", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_diarrhea = factor(ifelse(grepl("diarrhea", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_typhoid = factor(ifelse(grepl("typhoid", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_skinrashes = factor(ifelse(grepl("skinrashes", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_eyeinfections = factor(ifelse(grepl("eyeinfections", water_disease), "Yes", "No"), levels = c("Yes", "No")),
      disease_none = factor(ifelse(grepl("none", water_disease), "Yes", "No"), levels = c("Yes", "No"))
    ) %>%
    relocate(disease_malaria, disease_dengue, disease_diarrhea, disease_typhoid,
             disease_skinrashes, disease_eyeinfections, disease_none, .after = water_disease) %>%
    select(-water_disease)
}

# Remove Unnecessary Columns ####
hh_data <- hh_data %>%
  select(-any_of(c("id", "end_time", "start_time", "consent", "_version__",
                   "xform_id_string", "rootUuid", "status", "geolocation",
                   "submission_time", "tags", "notes", "submitted_by", "audit", "instanceID","attachments")),
         -starts_with("pm_"))

# Vital Corrections ####

remove_uuids <- c(
  "9cb5a1b7-58b1-40e2-aad3-6aae6de2c423",  # Test
  "f9124034-0da7-4802-a0f9-c28d89a1bf4a",  # Test survey
  "8293d887-55f6-4c5e-a970-7111ab1e7e59",  # Test survey
  "79421f00-cccd-483b-a62d-572360c54321",  # Jamiatu and Boboieh Did this in January. Delete Boboieh's
  "be6b9996-7493-40af-9617-8b6dee5572c2",  # Jamiatu and Boboieh Did this in January. Delete Boboieh's
  "1f1f9459-a9c4-4535-b880-7b943a0cebc6",  # joseph surveyed this household poorly and needed to be redone
  "857ae078-f2d9-42ef-9e4c-fbecd6f1a3fa",  # joseph surveyed this household poorly and needed to be redone
  "f53d102c-46ca-4d29-a36f-77e4c373fa58",  # Boboieh did 2 surveys in December at this household
  "b84fde49-2079-42bc-95c2-674179ba8b37",  # Boboieh did 2 surveys in December at this household
  "81336b7f-bdb0-4861-8029-844ea668f9ac",  # No idea what this survey is. It repeats D027
  "2d953e1c-4eb7-4c04-88ea-c96715c6b11d",  # Repeat of Jamiatu's survey
  "d3649035-3027-4922-87e4-647ce10cd0e6",  # Repeat of Jamiatu's survey
  "3f379a69-ea8d-4069-a29c-f37fa04eb8ae",  # Repeat of Jamiatu's survey
  "188222a6-1b93-477d-bb71-e811126cce47",  # Was only surveyed in Dec, not since, Deleted from list
  "a33a612a-67b7-4865-b47d-c089b1c28f0e",  # Was only surveyed in Nov, not since, Wrong number
  "704d0f54-7aaf-4729-b766-0c9fc9424096",  # Was surveyed twice in December
  "8380f0e2-1daa-424e-9fac-2fb9b2929950",  # surveyed twice in Jan
  "1aad244e-517e-4cee-bdd4-f7274cc72eac",  # surveyed twice in Jan
  "d584e583-3be8-48b3-bf30-1de7fc2cd9bf",  # Second Jan survey, hh numbers very different
  "8b0e4790-8cf3-49e3-94db-cc281b7117a4",  # Abdulai interviewed this HH twice in December
  "e7a2b6dd-9f1f-40b4-ac50-20d56596a9b0",  # Abdulai interviewed this HH twice in January
  "08ec615f-03c4-4472-8735-2b7eb660d185",  # Abdulai B interviewed this HH after Abdulai K in January
  "82b83237-eefb-49ee-b335-0951bc69dc59",  # Abdulai K interviewed this HH after Abdulai B in January
  "3b08e931-d0d1-4ae4-b2fe-3cf3c175899f",  # Abdulai B interviewed this HH twice in January
  "5e60c74-0723-4d5c-a74d-097e44a86175",   # Abdulai B interviewed this HH twice in January
  "e5e60c74-0723-4d5c-a74d-097e44a86175",  # Abdulai B interviewed this HH twice in January
  "5bb68cd5-d5a8-49e5-a01c-dccde5c029c3",  # Jamiatu interviewed twice in Dec
  "e884c4f7-50e1-44f1-88dd-e594b4eab01e",  # Jamiatu different hhsize, same respondent as D237
  "8866ba3b-2298-4c51-bfef-803a8e4e334b",  # Jamiatu Interviewed twice in Dec
  "6d628f6c-3748-4382-9dc5-1be716ad26be",  # Jamiatu and Boboieh Interviewed in Dec. Delete Jamiatu's
  "206104b8-ae14-4238-a959-59c234ffcf9d",  # Jamiatu and Boboieh Interviewed in Dec. Delete Jamiatu's
  "d6d1b94c-543a-4ed5-baf2-ad97ebe2b304",  # Jamiatu and Boboieh Interviewed in Dec. Delete Jamiatu's
  "0bc1c164-3122-4934-8e58-e5ce87e29b63",  # Jamiatu and Boboieh Interviewed in Dec. Delete Jamiatu's
  "ad35581c-52ec-4216-ac1d-022f5130a240",  # Jamiatu and Moses Interviewed in Dec. Delete Moses'
  "14c28d74-7926-476c-ab12-3dc7a1fc5971",  # Jamiatu and Moses Interviewed in Dec. Delete Moses'
  "3d807481-03de-4d34-bdec-46ec6a384d38",  # Boboieh and Moses Interviewed in Dec. Delete Moses'
  "cf2b4fb7-831e-4192-9b43-5d249e2fcbb5",  # Boboieh and Moses Interviewed in Dec. Delete Moses'
  "c6785d83-64d4-4470-a527-6e4e48997248",  # Jamiatu Interviewed twice in Dec
  "cdb39bba-23db-4402-a76d-7107336ade4c",  # boboieh interviewed twice in Feb
  "52e59b66-5076-4690-92b3-d1e1666580b4",  # Jamiatu interviewed twice in Feb. Gave this one the wrong HHID
  "4e56573d-ac77-4efc-a415-812560d0fd05",  # Jamiatu interviewed twice in Feb
  "1861228c-ad30-4cd6-91a2-3d4e4fe49951",  # Test Survey
  "4bcff6b8-14a9-42b9-a6e2-e70c5059649c",  # Test Survey
  "ed9e5816-c960-4aef-aca3-fd210efd0002",  # Jamiatu interviewed twice in Feb. Gave this one the wrong HHID
  "cd7628b1-737c-490b-80cc-225be59231c0", # Upload problems, Jamiatu redid this survey
  "231ec26a-096e-4b78-b696-88efac27b658" # Upload problems, Jamiatu redid this survey
  )

hh_data <- hh_data %>% filter(!uuid %in% remove_uuids)

# Survey Month Corrections ####
hh_data[hh_data$uuid == "ca8f999a-213c-45f1-a1ee-547e76b3bc22", "survey_month"] <- "Nov"
hh_data[hh_data$uuid == "a86d1670-797d-455e-ba72-d79b7821dd05", "survey_month"] <- "Nov"
hh_data[hh_data$uuid == "dd53125d-3902-4dc4-b631-f88fc3a9b43d", "survey_month"] <- "Nov"
hh_data[hh_data$uuid == "35c4a5b2-74c4-4e7b-aed4-d3145fe52725", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "ac7c9cf4-8cf7-4450-aff5-b342ef0c7b8f", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "828f0034-59f3-4f68-8c38-8b0f06304552", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "ae3febae-017c-4b36-b78b-69fede473b89", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "99967ee2-714e-476e-bc50-0a02bb0c4807", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "70cd449c-4fc2-49fd-97df-29a4e52def8d", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "cc65151b-9144-4c90-b897-469eb2375743", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "2742ca6f-3083-4ea0-92d0-df45ef535d71", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "817429cf-4380-416c-ba6c-58b37a9e804e", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "eb0db8b7-b56b-4612-83a5-924a3e72f969", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "4598cb35-033e-442b-9697-1a9a627671c5", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "280789e3-210d-42d6-b96c-00394ec9d987", "survey_month"] <- "Dec"
hh_data[hh_data$uuid == "f119125e-3810-4ea7-9c0b-798be1e57a80", "survey_month"] <- "Jan"
hh_data[hh_data$uuid == "6a2d54ba-8e66-41dd-8941-b4a6e5e6b33d", "survey_month"] <- "Jan"
hh_data[hh_data$uuid == "b9db3d04-e36b-4ca8-8910-061bfcee5eb4", "survey_month"] <- "Jan"
hh_data[hh_data$uuid == "73dce3fb-1d80-4e9f-bb17-60a2bc2e381f", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "4056f266-dd1b-40c4-afc3-ac2843204cb5", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "a28cc866-5e29-41ba-b0de-4666b2a3377f", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "89d675b7-bd74-453d-af4b-c551a613650e", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "4b62d2c8-bb7a-4807-a837-bf3b2d012b93", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "2d12fbad-4dc6-46c9-af30-44b004af0583", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "9dbf3fb1-7a2e-47ef-8499-418e4cf45cde", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "bd4b9a30-d1c5-49bb-a772-acddd85fa80d", "survey_month"] <- "Feb"
hh_data[hh_data$uuid == "711475da-56a6-4d34-a4a4-2364485094db", "survey_month"] <- "Mar"

# Community Corrections ####
hh_data[hh_data$uuid == "fc735327-7e52-4fbb-8075-396dba0bf9a5", "community"] <- "Dworzark"

# Surveyor Name Corrections ####
hh_data[hh_data$uuid == "e6c9f82c-7257-41e1-90f8-0f1df5df1741", "surveyor_name"] <- "Musa"
hh_data[hh_data$uuid == "20bbff41-5fa0-474d-874a-0d67251e3005", "surveyor_name"] <- "Musa"
hh_data[hh_data$uuid == "1ead9361-25b6-482d-ab9a-33e7ee080f41", "surveyor_name"] <- "Boboieh"

# HHID Corrections ####

hh_data[hh_data$uuid == "35c4a5b2-74c4-4e7b-aed4-d3145fe52725", "hhid"] <- "D002"
hh_data[hh_data$uuid == "90e50774-3b30-4351-ae28-8ec54ceea84e", "hhid"] <- "D002"
hh_data[hh_data$uuid == "828f0034-59f3-4f68-8c38-8b0f06304552", "hhid"] <- "D003"
hh_data[hh_data$uuid == "2dd9bf5f-7ac2-4773-812c-10f965cc222b", "hhid"] <- "D003"
hh_data[hh_data$uuid == "cc65151b-9144-4c90-b897-469eb2375743", "hhid"] <- "D004"
hh_data[hh_data$uuid == "de21be53-fe92-4718-aa19-f430f3a941f7", "hhid"] <- "D004"
hh_data[hh_data$uuid == "ac7c9cf4-8cf7-4450-aff5-b342ef0c7b8f", "hhid"] <- "D005"
hh_data[hh_data$uuid == "99e1201f-45a2-4183-9d2c-c6127829b520", "hhid"] <- "D005"
hh_data[hh_data$uuid == "ae3febae-017c-4b36-b78b-69fede473b89", "hhid"] <- "D026"
hh_data[hh_data$uuid == "10966ba2-34eb-47fe-b89e-e74ffdf94cdd", "hhid"] <- "D026"
hh_data[hh_data$uuid == "5a204c0f-03a4-4455-873c-65c54f8dbb21", "hhid"] <- "D031"
hh_data[hh_data$uuid == "eaf01da1-962c-4438-96fa-b71f19c3d1ab", "hhid"] <- "D033"
hh_data[hh_data$uuid == "84789a0a-5c02-43c7-a3a0-16ef4799775d", "hhid"] <- "D035"
hh_data[hh_data$uuid == "09817020-418b-4d93-9cc0-9337513de2f4", "hhid"] <- "D036"
hh_data[hh_data$uuid == "b5478bbf-fdf4-4ee1-8f01-df761bb7d168", "hhid"] <- "D039"
hh_data[hh_data$uuid == "90e29a9b-ad2b-4aca-867a-0783d8033780", "hhid"] <- "D152"
hh_data[hh_data$uuid == "8ecada15-60fb-44c8-b249-be8c7ae1cb23", "hhid"] <- "D153"
hh_data[hh_data$uuid == "ad4f05be-4b68-4168-80cf-9311aa133912", "hhid"] <- "D155"
hh_data[hh_data$uuid == "7679e66d-3364-4e36-8329-54da13525977", "hhid"] <- "D156"
hh_data[hh_data$uuid == "5a401289-212b-4d89-b79c-cb69bcdeab9f", "hhid"] <- "D157"
hh_data[hh_data$uuid == "68518eb8-b46c-422b-94df-4ba5fe14b7d8", "hhid"] <- "D159"
hh_data[hh_data$uuid == "dfdc7764-ca76-40cd-b72f-0e80a086f84e", "hhid"] <- "D199"
hh_data[hh_data$uuid == "9ba88bbf-6991-4651-90d8-d0fb269cd994", "hhid"] <- "D223"
hh_data[hh_data$uuid == "21e89059-002d-4505-9062-2addcc84aaf1", "hhid"] <- "P055"
hh_data[hh_data$uuid == "dae8d6c7-6cea-4c93-a0a1-e22df8cdf589", "hhid"] <- "C001"
hh_data[hh_data$uuid == "b1fb67cb-f639-4e2f-9c70-2151201eda40", "hhid"] <- "C002"
hh_data[hh_data$uuid == "83f6813a-062a-476d-b3e1-a6d3a51d95f3", "hhid"] <- "C003"
hh_data[hh_data$uuid == "2dae5673-b82f-4158-bba4-95353017ba5e", "hhid"] <- "C004"
hh_data[hh_data$uuid == "70185e9d-2fbb-40de-93f6-02f997fc3132", "hhid"] <- "C005"
hh_data[hh_data$uuid == "029226f2-4892-448b-9be7-5ae500d7181e", "hhid"] <- "C006"
hh_data[hh_data$uuid == "15fc6fec-8b81-4e95-ba66-7eff70658a5f", "hhid"] <- "C007"
hh_data[hh_data$uuid == "f4c26c98-6816-45b9-a821-c3c1d407823c", "hhid"] <- "C008"
hh_data[hh_data$uuid == "fdaba63a-8707-4af5-abb0-7ee71d155b06", "hhid"] <- "C009"
hh_data[hh_data$uuid == "9f0af9e6-33ae-4d97-a9db-ba8afb5431a6", "hhid"] <- "C010"
hh_data[hh_data$uuid == "53b345c2-e91c-41dd-9e9e-45df19d1fead", "hhid"] <- "C011"
hh_data[hh_data$uuid == "302bd056-24f3-479a-89b1-b20e52870aa7", "hhid"] <- "C012"
hh_data[hh_data$uuid == "420197ad-9ff9-4db9-8986-299a4ca126b9", "hhid"] <- "C013"
hh_data[hh_data$uuid == "1f217bc7-c61b-4f9c-a6a0-d656e6d52cfb", "hhid"] <- "C014"
hh_data[hh_data$uuid == "a054919b-43a5-48a0-adaf-685ff5562eda", "hhid"] <- "C015"
hh_data[hh_data$uuid == "63e8ccab-b345-4a3a-b449-0c44559da0da", "hhid"] <- "C016"
hh_data[hh_data$uuid == "211fce4a-10d5-455b-9f87-f1b352f68a88", "hhid"] <- "C017"
hh_data[hh_data$uuid == "b40850a1-6681-49ed-bdf6-e556f10d1899", "hhid"] <- "C018"
hh_data[hh_data$uuid == "fdb3ff3f-ee3d-48c6-8a5e-9c2d03218b3e", "hhid"] <- "C019"
hh_data[hh_data$uuid == "340ab198-3f37-4f64-9a2a-04cd9ccf2144", "hhid"] <- "C020"
hh_data[hh_data$uuid == "5817e825-5933-41b1-8e4d-f986a53e125c", "hhid"] <- "C021"
hh_data[hh_data$uuid == "9d9acf40-2f6d-46f5-9efa-49ad90909b29", "hhid"] <- "C022"
hh_data[hh_data$uuid == "62bc35c3-73f7-4d90-9a82-36458799704f", "hhid"] <- "C023"
hh_data[hh_data$uuid == "46cb51ef-3d08-4dac-a31e-6c200be1a501", "hhid"] <- "P001"
hh_data[hh_data$uuid == "ac93cb2d-3adc-4a49-81b4-8b6e13dbb2ac", "hhid"] <- "C024"
hh_data[hh_data$uuid == "973aaf5f-643d-4e08-ad6b-fc75f100f162", "hhid"] <- "C025"
hh_data[hh_data$uuid == "7efa996a-1654-4cf8-a437-da34d548421a", "hhid"] <- "C026"
hh_data[hh_data$uuid == "6b442e23-ec4b-43d9-9857-76e02bbdc10c", "hhid"] <- "C027"
hh_data[hh_data$uuid == "a1b02dfd-2afb-4952-9060-877b619685d5", "hhid"] <- "C028"
hh_data[hh_data$uuid == "81849087-e050-457a-a383-f3dc1f2969ed", "hhid"] <- "C029"
hh_data[hh_data$uuid == "1454c6c0-0eaf-406c-b2d8-694ba3fe4298", "hhid"] <- "C030"
hh_data[hh_data$uuid == "a361308a-5303-4648-a98e-00c38adac20b", "hhid"] <- "C031"
hh_data[hh_data$uuid == "b2c130da-7b9d-4ba7-8a27-2819f2777d15", "hhid"] <- "P002"
hh_data[hh_data$uuid == "17e7d2f5-0eaf-4609-8a2c-9447535ead45", "hhid"] <- "P003"
hh_data[hh_data$uuid == "c937a95c-eb2f-4e10-8908-95ace1332dc3", "hhid"] <- "P004"
hh_data[hh_data$uuid == "eafb95f1-2247-4fff-a587-6b17ea928e0f", "hhid"] <- "P005"
hh_data[hh_data$uuid == "de3e0c16-4fa6-4881-a0df-56a12e6fe813", "hhid"] <- "P006"
hh_data[hh_data$uuid == "22b96e81-04f3-4f2f-a47b-c91665d13980", "hhid"] <- "P007"
hh_data[hh_data$uuid == "60649f35-8f17-40ca-8169-cec34960f1aa", "hhid"] <- "C032"
hh_data[hh_data$uuid == "1c5fe7f7-509f-41b5-b462-dcacdc4150fc", "hhid"] <- "C033"
hh_data[hh_data$uuid == "9212824a-c0b0-45f5-a9ad-e24e6e6d2d88", "hhid"] <- "C034"
hh_data[hh_data$uuid == "7ba70705-1823-432e-9445-fd00b5f524fb", "hhid"] <- "C035"
hh_data[hh_data$uuid == "e21ee736-26ee-4ae7-b7c8-7d3975fc921d", "hhid"] <- "C036"
hh_data[hh_data$uuid == "e04986b8-6f44-4862-940d-390d918ff5ee", "hhid"] <- "C037"
hh_data[hh_data$uuid == "3bf5b4bb-3b87-4421-92a6-f890bc7c4294", "hhid"] <- "P008"
hh_data[hh_data$uuid == "20122b58-9272-4989-869a-1d8aac6bd338", "hhid"] <- "C038"
hh_data[hh_data$uuid == "ab16ae1a-63c8-4e99-b199-901c329aa864", "hhid"] <- "P009"
hh_data[hh_data$uuid == "70b731f7-8054-4807-84ec-43e6037298eb", "hhid"] <- "P010"
hh_data[hh_data$uuid == "be602df6-32bb-4e2d-969c-765ef2d379e3", "hhid"] <- "P011"
hh_data[hh_data$uuid == "b659a818-e184-4241-8c70-ff5d88ab9b8d", "hhid"] <- "P012"
hh_data[hh_data$uuid == "c81b9199-0f44-4e99-b0ef-890ca51fb11e", "hhid"] <- "P013"
hh_data[hh_data$uuid == "f142d150-0b0e-4a01-af08-b4c03526da14", "hhid"] <- "P014"
hh_data[hh_data$uuid == "8f9ad52d-7bf4-4087-a0b9-8dc6320a3d01", "hhid"] <- "C039"
hh_data[hh_data$uuid == "d0b32575-f6ce-46fb-9c36-b7b04dd821f5", "hhid"] <- "P015"
hh_data[hh_data$uuid == "f6bce9a7-41b9-44ba-823d-c07ba4e34019", "hhid"] <- "C040"
hh_data[hh_data$uuid == "5d331b43-4243-4133-8ce7-67078d59ae9e", "hhid"] <- "C041"
hh_data[hh_data$uuid == "c268a3d4-ab13-4701-9dd0-1d6fb8bf19c1", "hhid"] <- "C042"
hh_data[hh_data$uuid == "29a944aa-c237-4954-a202-9bedeb7ddeae", "hhid"] <- "C043"
hh_data[hh_data$uuid == "a3664fd1-b118-49ce-b975-eca6f7b72495", "hhid"] <- "C044"
hh_data[hh_data$uuid == "b27d25a0-6c0a-4147-817b-e6ca2af8c56b", "hhid"] <- "C045"
hh_data[hh_data$uuid == "487fac52-084b-43a0-992c-ccc85ba351d5", "hhid"] <- "C046"
hh_data[hh_data$uuid == "8ee8c361-0951-493c-a07c-82a31d3f5182", "hhid"] <- "C047"
hh_data[hh_data$uuid == "f4b20748-0983-402b-a6a5-d3fdde67a190", "hhid"] <- "P016"
hh_data[hh_data$uuid == "5b094599-3767-4364-a0a9-bee112f4860c", "hhid"] <- "P017"
hh_data[hh_data$uuid == "51d9792a-42fc-4562-aa60-77bd7c0c3dd6", "hhid"] <- "P018"
hh_data[hh_data$uuid == "058c260c-a65d-474e-b23f-fa7b2a8e5e0f", "hhid"] <- "P019"
hh_data[hh_data$uuid == "f5009898-322f-4889-bfc9-f22a851cd88a", "hhid"] <- "P020"
hh_data[hh_data$uuid == "cc454141-f76a-4da2-86de-cac21f3a6ee7", "hhid"] <- "P021"
hh_data[hh_data$uuid == "62fbfc0b-6ec9-4e50-a282-46b9d0d7ab2f", "hhid"] <- "P022"
hh_data[hh_data$uuid == "8bd0777d-46d4-48fe-b5ad-2f57563d5716", "hhid"] <- "P023"
hh_data[hh_data$uuid == "9aa9a9bd-b195-4541-8a17-5fa47e978e27", "hhid"] <- "P024"
hh_data[hh_data$uuid == "05bf12dc-4858-4e63-ab5b-1c146e8d97d2", "hhid"] <- "C048"
hh_data[hh_data$uuid == "d487c621-14d1-4acd-9677-bd97d6e77db8", "hhid"] <- "C049"
hh_data[hh_data$uuid == "db45f088-bf8c-470d-a9c4-8c1fb2cf2ea4", "hhid"] <- "C050"
hh_data[hh_data$uuid == "c6d08fe4-4630-4281-8e24-af879906694d", "hhid"] <- "C051"
hh_data[hh_data$uuid == "d23b2954-7441-4e1f-9ba8-87ea5319d728", "hhid"] <- "C052"
hh_data[hh_data$uuid == "c0637b64-80fc-4b3f-b0a4-282c66f0f0e9", "hhid"] <- "C053"
hh_data[hh_data$uuid == "2efb1217-6723-4135-bbb4-173c84b9c91d", "hhid"] <- "C054"
hh_data[hh_data$uuid == "fa9065a0-966d-4940-9679-bf9a2db4f92e", "hhid"] <- "C055"
hh_data[hh_data$uuid == "acf954d2-f77e-4380-b9f9-ddf9c06ae6ad", "hhid"] <- "C056"
hh_data[hh_data$uuid == "c09c2780-f5a0-47eb-8c8d-d2b8845d164e", "hhid"] <- "C057"
hh_data[hh_data$uuid == "b56dbb55-7de0-49ba-acba-4b9465377bee", "hhid"] <- "C058"
hh_data[hh_data$uuid == "5106cec6-d3ff-4f5f-ad71-650306f8659c", "hhid"] <- "C059"
hh_data[hh_data$uuid == "85f22e9e-d43a-4c19-bcef-d6a1926a251f", "hhid"] <- "P025"
hh_data[hh_data$uuid == "f0081271-6af6-4e08-a03b-3347a7d3bc1b", "hhid"] <- "P026"
hh_data[hh_data$uuid == "ac89beec-0996-47e7-a9a2-319f42c98674", "hhid"] <- "P027"
hh_data[hh_data$uuid == "1e262ba4-158b-4c19-a379-8684c1c9cd22", "hhid"] <- "P028"
hh_data[hh_data$uuid == "41742a73-ffc4-4198-a5a8-fa10e8bdb50f", "hhid"] <- "P029"
hh_data[hh_data$uuid == "15404502-e6d3-47e3-b936-f89fddb54353", "hhid"] <- "P030"
hh_data[hh_data$uuid == "e9a5f862-97f1-44d7-895d-04775c7d4d63", "hhid"] <- "P031"
hh_data[hh_data$uuid == "68005fd3-7d8e-4539-b65c-27bd6c039b43", "hhid"] <- "C060"
hh_data[hh_data$uuid == "dea9d303-d452-4c97-b430-10400ec7c939", "hhid"] <- "C061"
hh_data[hh_data$uuid == "547b865a-31b4-4184-a8aa-0ae9ea1db6d2", "hhid"] <- "C062"
hh_data[hh_data$uuid == "4f1e4f04-a243-4bbb-a209-c66ed9e72feb", "hhid"] <- "C063"
hh_data[hh_data$uuid == "2811bc35-2eb8-42e9-84c9-c1785c0606dc", "hhid"] <- "C064"
hh_data[hh_data$uuid == "32941e9b-dc62-4137-9159-fabf1c58f76d", "hhid"] <- "C065"
hh_data[hh_data$uuid == "e65e13c7-e22d-4279-85e5-14ec3c1b5e00", "hhid"] <- "C066"
hh_data[hh_data$uuid == "62133311-ceac-47bd-b7f1-0baded6103f1", "hhid"] <- "C067"
hh_data[hh_data$uuid == "58cd587d-de28-4845-97e3-2339ab48c2c0", "hhid"] <- "C068"
hh_data[hh_data$uuid == "791e96a6-46b0-476c-aca4-02872aec4fab", "hhid"] <- "C069"
hh_data[hh_data$uuid == "d9e02f42-8c78-49fa-b5e0-9486d5cdaf6b", "hhid"] <- "C070"
hh_data[hh_data$uuid == "f4eb3120-6c72-4687-81dc-a42794299365", "hhid"] <- "C071"
hh_data[hh_data$uuid == "77c08b76-153c-408e-a56d-64d66d8d5f4e", "hhid"] <- "D002"
hh_data[hh_data$uuid == "830cd5cc-f951-40f0-9086-64ea8275b1c4", "hhid"] <- "D003"
hh_data[hh_data$uuid == "344dc685-8463-4f16-9a16-991132c68202", "hhid"] <- "D004"
hh_data[hh_data$uuid == "53edf992-28fd-4acd-8d60-c99389c1da19", "hhid"] <- "D005"
hh_data[hh_data$uuid == "a5516e5b-16a4-4b53-bf88-86e5a48859da", "hhid"] <- "D006"
hh_data[hh_data$uuid == "8af210bb-e2a3-4f0b-8a18-353a1702c891", "hhid"] <- "D007"
hh_data[hh_data$uuid == "061bd737-c256-4fe3-a6b3-acd1dc0b1ce2", "hhid"] <- "D008"
hh_data[hh_data$uuid == "5fbfea3b-e19b-442c-bc8b-9204b213d06a", "hhid"] <- "D009"
hh_data[hh_data$uuid == "159a98d2-76fa-468b-b49d-5ae471c5c57f", "hhid"] <- "D010"
hh_data[hh_data$uuid == "d1046787-5541-4e53-9753-a7996654b3d4", "hhid"] <- "D011"
hh_data[hh_data$uuid == "4c03b1c9-1feb-4ec4-a5c2-39553a526d10", "hhid"] <- "D012"
hh_data[hh_data$uuid == "ddabb3c7-7743-4de2-a6c4-be98540a18b0", "hhid"] <- "D013"
hh_data[hh_data$uuid == "1ce8d86f-47fd-4858-97ea-16baeb2b87f1", "hhid"] <- "P032"
hh_data[hh_data$uuid == "0e8dd522-6a51-4416-aa07-8dd09cf3e1f5", "hhid"] <- "P033"
hh_data[hh_data$uuid == "7aa1789d-d251-4644-a4fc-1c8b5906e5ab", "hhid"] <- "P034"
hh_data[hh_data$uuid == "eb7eee01-53c5-4446-8b51-38034955e7ee", "hhid"] <- "P035"
hh_data[hh_data$uuid == "3ed09e11-f32c-460f-8c73-a99291c0e044", "hhid"] <- "P036"
hh_data[hh_data$uuid == "8b6017e7-9d3c-482a-b4ce-198150853045", "hhid"] <- "P037"
hh_data[hh_data$uuid == "1a64af76-4375-4e9b-93f6-6589a7d4affc", "hhid"] <- "P038"
hh_data[hh_data$uuid == "423d27dd-e147-4652-9429-b48a17ff5b73", "hhid"] <- "P039"
hh_data[hh_data$uuid == "4a18da1e-69bb-4fa7-a8d8-5f1c298adb4c", "hhid"] <- "P040"
hh_data[hh_data$uuid == "b5486290-cc63-434e-b469-326e5257c406", "hhid"] <- "P041"
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "hhid"] <- "D014"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "hhid"] <- "D015"
hh_data[hh_data$uuid == "fc735327-7e52-4fbb-8075-396dba0bf9a5", "hhid"] <- "D016"
hh_data[hh_data$uuid == "2cfa5225-a766-4e56-b651-9d6f9eda52dc", "hhid"] <- "D017"
hh_data[hh_data$uuid == "9e309e95-7a3d-4d90-9ee3-90f7e5f60357", "hhid"] <- "P042"
hh_data[hh_data$uuid == "30b0a87e-b5d5-491d-a3aa-263e5d9bee98", "hhid"] <- "D018"
hh_data[hh_data$uuid == "745735e5-90b1-48b3-83e2-c258eb465fdb", "hhid"] <- "D019"
hh_data[hh_data$uuid == "736eee8c-1a26-4125-8ae4-366c1c302a97", "hhid"] <- "D020"
hh_data[hh_data$uuid == "0fed3414-2a13-4f46-bdde-2d4bba1b5e6d", "hhid"] <- "D021"
hh_data[hh_data$uuid == "8ad80985-70e8-4550-8989-5c180dad7641", "hhid"] <- "D022"
hh_data[hh_data$uuid == "630de0e7-b9d8-4a2c-a00f-d7bb9366603d", "hhid"] <- "D023"
hh_data[hh_data$uuid == "20f4f405-ac2f-4213-ba8e-49570ccf6519", "hhid"] <- "D024"
hh_data[hh_data$uuid == "f417187f-be37-4f05-89c1-b05bd1c227d8", "hhid"] <- "D025"
hh_data[hh_data$uuid == "588339eb-111d-4d02-a5f2-d6d916e6faf0", "hhid"] <- "C072"
hh_data[hh_data$uuid == "8a09a629-fdec-4206-9109-ea42320423fa", "hhid"] <- "C073"
hh_data[hh_data$uuid == "e319d3cf-ea67-4b89-a720-ac30b7b14f2d", "hhid"] <- "C074"
hh_data[hh_data$uuid == "24dd6d18-bac7-45ab-9be5-839f15a641ec", "hhid"] <- "C075"
hh_data[hh_data$uuid == "f7f1482b-631c-49a4-94bc-947550c0fc81", "hhid"] <- "C076"
hh_data[hh_data$uuid == "db66d2a4-36ed-4ddb-9327-9c81ee7fdb6d", "hhid"] <- "C077"
hh_data[hh_data$uuid == "66a5c709-81b1-4b33-99f0-ab26dab8780b", "hhid"] <- "C078"
hh_data[hh_data$uuid == "b605a411-1ff8-4673-9eac-303c207a0198", "hhid"] <- "C079"
hh_data[hh_data$uuid == "9917f358-248e-43bf-9cd0-bdbdb7be5b3b", "hhid"] <- "C080"
hh_data[hh_data$uuid == "4c926fbd-88b7-4317-885b-e2c9fff4fa59", "hhid"] <- "C081"
hh_data[hh_data$uuid == "5e01b6a3-3292-4c3f-9ac6-65bec14a86a0", "hhid"] <- "C082"
hh_data[hh_data$uuid == "5352c58e-ba07-45de-9b95-2205dae58980", "hhid"] <- "C083"
hh_data[hh_data$uuid == "72e186ad-4c6c-4032-b6cf-f839d2ecc996", "hhid"] <- "C084"
hh_data[hh_data$uuid == "5a9e3b22-09e3-453f-9511-48f22264519d", "hhid"] <- "C085"
hh_data[hh_data$uuid == "2c95bb2e-a298-43c4-b509-88ac3119321e", "hhid"] <- "C086"
hh_data[hh_data$uuid == "25445ce7-e1a6-4213-a29f-c90aacd0392c", "hhid"] <- "C087"
hh_data[hh_data$uuid == "de689fc2-2be8-4970-b02d-4c9410017fe8", "hhid"] <- "C088"
hh_data[hh_data$uuid == "367dbdf6-70ef-416b-821f-71793e4a50d4", "hhid"] <- "C089"
hh_data[hh_data$uuid == "7ded2afa-3659-4dcf-ab98-684f04c2a18f", "hhid"] <- "C090"
hh_data[hh_data$uuid == "ad661fa6-7660-4a66-8fcb-ce4392cb26bf", "hhid"] <- "C091"
hh_data[hh_data$uuid == "8bcb78c1-0cd5-4935-b0ff-393a1924e41e", "hhid"] <- "C092"
hh_data[hh_data$uuid == "f430af01-59f6-4a30-936d-b835747325a8", "hhid"] <- "D026"
hh_data[hh_data$uuid == "d844a308-56e9-44b9-bf87-1e99eaefa052", "hhid"] <- "D027"
hh_data[hh_data$uuid == "87466dd3-57bd-41d6-9547-cc5a8de0723e", "hhid"] <- "D028"
hh_data[hh_data$uuid == "32c671c4-000c-499a-b3fe-a277229d30a1", "hhid"] <- "D029"
hh_data[hh_data$uuid == "579ac23f-4b14-4374-9421-d82ab033ba44", "hhid"] <- "D031"
hh_data[hh_data$uuid == "522943f8-35d8-4c5a-ac59-6bacadf5f639", "hhid"] <- "D032"
hh_data[hh_data$uuid == "372163d4-796f-4d08-9755-f640b6dc9646", "hhid"] <- "D033"
hh_data[hh_data$uuid == "1698f65d-ab42-4cbc-b525-0d39882e9e6f", "hhid"] <- "D034"
hh_data[hh_data$uuid == "41e0c2d8-754e-4482-b2b6-957cb5a2e70c", "hhid"] <- "D035"
hh_data[hh_data$uuid == "a3135d7e-88ff-44dc-a4e5-08d64faa8611", "hhid"] <- "D036"
hh_data[hh_data$uuid == "b49c22a0-0a02-4608-a31a-ed7ee63c0d0f", "hhid"] <- "P043"
hh_data[hh_data$uuid == "a0b0d039-5a72-496a-90d1-693302e1be24", "hhid"] <- "P044"
hh_data[hh_data$uuid == "cab721d9-16b2-49e0-9bdc-991dd8370014", "hhid"] <- "P045"
hh_data[hh_data$uuid == "94b891cb-f0b5-4b29-b241-59f83b3b8335", "hhid"] <- "P046"
hh_data[hh_data$uuid == "9928c46d-37b9-4f95-b96b-c2288ec28b20", "hhid"] <- "P047"
hh_data[hh_data$uuid == "204d1ece-9fb8-47ea-853c-1d8da9996224", "hhid"] <- "D037"
hh_data[hh_data$uuid == "3eb4df93-80a4-46da-98fe-c2139f602f12", "hhid"] <- "D038"
hh_data[hh_data$uuid == "a38e58cf-6b97-44ae-bf2d-6682463d2345", "hhid"] <- "D039"
hh_data[hh_data$uuid == "0646a356-b502-4569-8e8b-5d6f4b55cf06", "hhid"] <- "D040"
hh_data[hh_data$uuid == "5df59f00-af3a-4881-a0ac-e2ff0a8f90b5", "hhid"] <- "D041"
hh_data[hh_data$uuid == "ecd142fc-9758-45a1-8592-b535dec8340a", "hhid"] <- "D042"
hh_data[hh_data$uuid == "df83da6a-fa01-450f-864c-ae5325446aef", "hhid"] <- "D043"
hh_data[hh_data$uuid == "42efae1a-cd77-49ca-85e6-6583fb8896b4", "hhid"] <- "D044"
hh_data[hh_data$uuid == "48be598f-b6e9-4e35-85ba-d672c3e45331", "hhid"] <- "D045"
hh_data[hh_data$uuid == "74bf1689-bbd5-4312-b14e-ecb835f1c018", "hhid"] <- "D046"
hh_data[hh_data$uuid == "1fa4316f-d559-4ba1-a4a3-30332db5cbfe", "hhid"] <- "D047"
hh_data[hh_data$uuid == "82730320-0b13-4d95-99d8-a8b1cab20615", "hhid"] <- "D048"
hh_data[hh_data$uuid == "81fca416-a986-4fe3-998c-84927148b979", "hhid"] <- "D049"
hh_data[hh_data$uuid == "8ee7c171-4538-4510-99b2-6de849b3ffd9", "hhid"] <- "D050"
hh_data[hh_data$uuid == "ce8d3269-42c3-4625-a023-ed95dda75df5", "hhid"] <- "D051"
hh_data[hh_data$uuid == "48bccb7d-6e07-4c4e-aa67-b77e9769ec97", "hhid"] <- "D052"
hh_data[hh_data$uuid == "73482313-d8a2-444b-8569-974c05842a71", "hhid"] <- "D053"
hh_data[hh_data$uuid == "fa4e052b-2b9d-4fe8-9221-effaca5cf1c5", "hhid"] <- "D054"
hh_data[hh_data$uuid == "d0e39125-abbb-489a-92a7-4da68c5b551e", "hhid"] <- "D055"
hh_data[hh_data$uuid == "90b2e60f-b059-4001-bdee-4723dd5748ee", "hhid"] <- "D030"
hh_data[hh_data$uuid == "f13d84d4-dba2-49f4-a5d7-8b7dafdea0cd", "hhid"] <- "D056"
hh_data[hh_data$uuid == "889b5f1a-dd22-4463-ab87-22147019bd47", "hhid"] <- "D057"
hh_data[hh_data$uuid == "b7ffe1df-4841-4a3d-82c6-d5bfa85a3f3b", "hhid"] <- "D058"
hh_data[hh_data$uuid == "333f5172-1763-47a7-9124-910ee1d7f136", "hhid"] <- "D059"
hh_data[hh_data$uuid == "3345a590-f702-404f-b2aa-d882b1669a79", "hhid"] <- "D060"
hh_data[hh_data$uuid == "0af995cd-4cf0-47e9-92fa-c20af29c75d5", "hhid"] <- "D061"
hh_data[hh_data$uuid == "84cf36ba-7748-4866-8c68-e8254908ff05", "hhid"] <- "D062"
hh_data[hh_data$uuid == "de3243ee-2d9d-419e-8b7c-57b3344b416c", "hhid"] <- "P048"
hh_data[hh_data$uuid == "6e12c352-d5ec-4e87-b1a3-088110e5d09d", "hhid"] <- "P049"
hh_data[hh_data$uuid == "00d72451-6d28-4d82-8a2d-8a9fcc7dae5a", "hhid"] <- "P050"
hh_data[hh_data$uuid == "6cf7bf89-19ac-4568-8457-e11c905833f1", "hhid"] <- "P051"
hh_data[hh_data$uuid == "2448823d-a51d-4189-939b-86ca918ce237", "hhid"] <- "P052"
hh_data[hh_data$uuid == "f879d736-ef16-4203-a7ab-915fb02458ae", "hhid"] <- "P053"
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "hhid"] <- "P054"
hh_data[hh_data$uuid == "b74edd88-ccfc-4cbc-8462-ceefc9b5256b", "hhid"] <- "P055"
hh_data[hh_data$uuid == "268b7e04-0d28-4ce0-adc0-4340c696f379", "hhid"] <- "P056"
hh_data[hh_data$uuid == "5e6a8026-65c2-4f39-b625-b9fd1eedc772", "hhid"] <- "P057"
hh_data[hh_data$uuid == "1667f99e-acaf-435a-bb6d-3d7f212a0e14", "hhid"] <- "P058"
hh_data[hh_data$uuid == "6c0bc6e5-8d5d-4e7d-85c1-77f8bc778fc7", "hhid"] <- "D063"
hh_data[hh_data$uuid == "2a394fab-0ce3-495b-b0ca-21f8f1cbc528", "hhid"] <- "D064"
hh_data[hh_data$uuid == "bbfe55bf-23ef-424f-883f-2c9e97cf8793", "hhid"] <- "D065"
hh_data[hh_data$uuid == "46301d9e-eaeb-452d-a989-334d307e9d7b", "hhid"] <- "D066"
hh_data[hh_data$uuid == "83d4d552-d91e-47b9-8a54-5cbd818ccbd5", "hhid"] <- "C093"
hh_data[hh_data$uuid == "df755ff4-b54e-4e72-ba93-4f984b3b3919", "hhid"] <- "C094"
hh_data[hh_data$uuid == "27bb835f-a247-45bf-a912-90a0bb9bf076", "hhid"] <- "C095"
hh_data[hh_data$uuid == "37856d33-d39c-480a-8c39-9e906726a389", "hhid"] <- "C096"
hh_data[hh_data$uuid == "0086a58b-a395-4ee9-a87d-284f219979dc", "hhid"] <- "C097"
hh_data[hh_data$uuid == "c72c0463-24cb-4938-babd-fc24a413e201", "hhid"] <- "C098"
hh_data[hh_data$uuid == "6af72574-bdd1-4cfc-8242-947c8359556e", "hhid"] <- "C099"
hh_data[hh_data$uuid == "17826929-773c-4b15-bd5d-ae40575b3066", "hhid"] <- "C100"
hh_data[hh_data$uuid == "d5807733-f05c-4f95-bec4-83c9b786baa3", "hhid"] <- "P059"
hh_data[hh_data$uuid == "f22e3760-824b-4192-8afe-e2aa257c2e6f", "hhid"] <- "P060"
hh_data[hh_data$uuid == "fe7ae80c-4064-4885-9bc2-740b615e944b", "hhid"] <- "P061"
hh_data[hh_data$uuid == "e8fed316-34ca-4e61-818f-e561a3ecb1a4", "hhid"] <- "P062"
hh_data[hh_data$uuid == "74916c19-83d2-43d9-b31b-85b5bc33d29e", "hhid"] <- "P063"
hh_data[hh_data$uuid == "1528dbdf-5ec4-4674-82c3-7aa17ec2533b", "hhid"] <- "D071"
hh_data[hh_data$uuid == "b20deacf-b776-47bd-8807-74e831bf2bbe", "hhid"] <- "D072"
hh_data[hh_data$uuid == "2778b1b0-d2d7-4be8-96bc-ef5324f9b3ee", "hhid"] <- "D073"
hh_data[hh_data$uuid == "e85fae41-1945-4371-8935-1155c8f76aef", "hhid"] <- "D074"
hh_data[hh_data$uuid == "5f734998-14b3-455b-aac8-1c6a2cf446df", "hhid"] <- "D075"
hh_data[hh_data$uuid == "61e1a657-f9ac-47b9-b379-f706eacde3d1", "hhid"] <- "D076"
hh_data[hh_data$uuid == "1a4fb681-def0-4d1a-ac03-be92d561d367", "hhid"] <- "D077"
hh_data[hh_data$uuid == "324531f9-fbee-4045-87d2-a6cd545fd66f", "hhid"] <- "D078"
hh_data[hh_data$uuid == "971fd243-4e42-43a1-b1ab-548fa6e33b99", "hhid"] <- "D079"
hh_data[hh_data$uuid == "f5e90db0-d6fe-4446-bd54-dea1ae625b7e", "hhid"] <- "D080"
hh_data[hh_data$uuid == "57d42d04-0ab7-4c6d-bf5e-777750b80d8d", "hhid"] <- "D067"
hh_data[hh_data$uuid == "8aea6b9a-19b9-4f49-aa82-e632613933d0", "hhid"] <- "D081"
hh_data[hh_data$uuid == "e528fb9b-4b58-4db4-b2fd-3e52774bc302", "hhid"] <- "D082"
hh_data[hh_data$uuid == "3853dcd7-b567-4441-8139-9047a37a730b", "hhid"] <- "D083"
hh_data[hh_data$uuid == "f466d8c8-1d80-441f-9bc8-7b207906e52c", "hhid"] <- "D084"
hh_data[hh_data$uuid == "adbc8e28-8cb3-4843-a200-0f1d1842cf8a", "hhid"] <- "D085"
hh_data[hh_data$uuid == "aa49fd4b-7c3a-45fe-b450-b9fb46771515", "hhid"] <- "D086"
hh_data[hh_data$uuid == "5e22e6fe-f3a0-4902-ad27-192d532f614f", "hhid"] <- "D087"
hh_data[hh_data$uuid == "70fe3ab0-ed5e-410e-bbfe-9572267bd6ab", "hhid"] <- "D088"
hh_data[hh_data$uuid == "558cb8d8-d09a-4976-822e-df399aea0283", "hhid"] <- "D089"
hh_data[hh_data$uuid == "a76e3017-875f-49fe-b5b2-818ffd50e896", "hhid"] <- "D090"
hh_data[hh_data$uuid == "39d0963f-46e3-46b0-b018-836367dbace4", "hhid"] <- "D091"
hh_data[hh_data$uuid == "fcbac837-810f-4458-afe5-856ad221ba60", "hhid"] <- "D092"
hh_data[hh_data$uuid == "633be5bd-e17b-4fde-9f4f-7303f718f485", "hhid"] <- "D093"
hh_data[hh_data$uuid == "a45dd22f-7dcd-4f50-be04-fe72f6f115ed", "hhid"] <- "D094"
hh_data[hh_data$uuid == "9bd63f89-8b0b-4dda-b568-812b9b485c20", "hhid"] <- "D095"
hh_data[hh_data$uuid == "2b3b26e2-55c8-4d04-942b-2ce81b19af95", "hhid"] <- "D096"
hh_data[hh_data$uuid == "a86d1670-797d-455e-ba72-d79b7821dd05", "hhid"] <- "D097"
hh_data[hh_data$uuid == "9341ca34-703a-4ffd-bfff-8386d25518c3", "hhid"] <- "D098"
hh_data[hh_data$uuid == "f246bd3d-f00b-4a4d-b081-f271e76a5940", "hhid"] <- "D099"
hh_data[hh_data$uuid == "f520e081-e2ef-4be8-b981-7e24ac2cb0ad", "hhid"] <- "D100"
hh_data[hh_data$uuid == "1ddaed4a-6b59-41e1-ad9c-f06b0cee3ca7", "hhid"] <- "D101"
hh_data[hh_data$uuid == "ef6fb9ad-1fea-4462-bea7-95b0cff9a706", "hhid"] <- "D068"
hh_data[hh_data$uuid == "9cb780f8-09cc-4479-ab58-793c914fb617", "hhid"] <- "P064"
hh_data[hh_data$uuid == "5b7920df-b0d0-440a-b962-261b9021bc02", "hhid"] <- "P065"
hh_data[hh_data$uuid == "e52bc04f-3ed1-49ad-8bf2-fd32f9762ce2", "hhid"] <- "D102"
hh_data[hh_data$uuid == "1f8e6f81-30cb-4e6d-9c68-d5b6ed1fd48a", "hhid"] <- "D103"
hh_data[hh_data$uuid == "28b81264-e07d-4286-a691-930e7d605ebc", "hhid"] <- "D104"
hh_data[hh_data$uuid == "4ec89962-e43d-4cd6-9f73-0fb873212ad0", "hhid"] <- "D105"
hh_data[hh_data$uuid == "480cf908-8857-45fb-8e8b-c2ae8e9d83e1", "hhid"] <- "P066"
hh_data[hh_data$uuid == "3b4fa4df-47b2-4b1e-a0f6-f789be53cd45", "hhid"] <- "P067"
hh_data[hh_data$uuid == "99408902-577c-49e6-8269-bdadf4167963", "hhid"] <- "P068"
hh_data[hh_data$uuid == "4a21018b-da1c-4bc2-9b57-191c49c95a87", "hhid"] <- "P069"
hh_data[hh_data$uuid == "11758ffd-ff83-40a6-bd08-66156576b601", "hhid"] <- "P070"
hh_data[hh_data$uuid == "963924d8-5893-4676-9c53-656192451937", "hhid"] <- "P071"
hh_data[hh_data$uuid == "a8f9e6f1-7d38-4846-a344-b0d054c1c345", "hhid"] <- "P072"
hh_data[hh_data$uuid == "f19f8219-7d5b-4eb4-b7ed-6f5678f33c15", "hhid"] <- "P073"
hh_data[hh_data$uuid == "a596f1b3-157c-4e36-a0f6-325da5f63734", "hhid"] <- "P074"
hh_data[hh_data$uuid == "aeb5ac42-2fe9-4889-83cd-7d22f42f6e91", "hhid"] <- "P075"
hh_data[hh_data$uuid == "45069156-0e13-492d-8e44-787e54a62f56", "hhid"] <- "P076"
hh_data[hh_data$uuid == "90ae1aeb-8c83-49bc-ac72-60f7f2da9081", "hhid"] <- "C101"
hh_data[hh_data$uuid == "53badab5-2a04-4f97-95a1-6e156e947d7a", "hhid"] <- "C102"
hh_data[hh_data$uuid == "94be19a9-7a4f-4578-ab04-4cf1c811695a", "hhid"] <- "C103"
hh_data[hh_data$uuid == "e64c5a89-d1db-47a6-80f7-e7446e8ab544", "hhid"] <- "C104"
hh_data[hh_data$uuid == "d823678e-a9cf-48b5-b658-e0f10a8da4bf", "hhid"] <- "C105"
hh_data[hh_data$uuid == "9066b65c-f20f-4371-a02c-dfe1157c8d66", "hhid"] <- "C106"
hh_data[hh_data$uuid == "66d778c3-facc-4d69-b403-a541cc54c29a", "hhid"] <- "C107"
hh_data[hh_data$uuid == "4327f4a6-0e24-4954-87d2-56b6b8535c6b", "hhid"] <- "C108"
hh_data[hh_data$uuid == "7f512acc-c37b-464e-96f4-d6af0823cc90", "hhid"] <- "D109"
hh_data[hh_data$uuid == "7df0391d-7b84-41f0-9fa7-42c12378d143", "hhid"] <- "D110"
hh_data[hh_data$uuid == "37b4180f-be32-4921-926f-67d1a5c9d98d", "hhid"] <- "D111"
hh_data[hh_data$uuid == "7aad59bc-6473-4a37-af9a-e692ab20bfbd", "hhid"] <- "D112"
hh_data[hh_data$uuid == "954dd61e-f3a6-4956-9b54-862b4a97421a", "hhid"] <- "D113"
hh_data[hh_data$uuid == "2a18652c-359c-44d7-9e34-b0e199d0e3e3", "hhid"] <- "D114"
hh_data[hh_data$uuid == "ad9e8012-5a24-49e4-89d0-549dbd59a8b6", "hhid"] <- "D115"
hh_data[hh_data$uuid == "77804397-6aaf-4e64-968d-d9fa07b63c5d", "hhid"] <- "D116"
hh_data[hh_data$uuid == "421055f1-8692-4b39-986c-edee2cbf8cfb", "hhid"] <- "D117"
hh_data[hh_data$uuid == "54e2f7a1-68bb-429c-a537-d3f9df2edd7a", "hhid"] <- "D118"
hh_data[hh_data$uuid == "6319e309-b9cb-4f6d-bde1-354c58504202", "hhid"] <- "D119"
hh_data[hh_data$uuid == "375df493-2150-440f-956d-7cb2d9ee5940", "hhid"] <- "D120"
hh_data[hh_data$uuid == "ef70da4c-4dcb-4563-a224-40ada0e1beb1", "hhid"] <- "D121"
hh_data[hh_data$uuid == "335979df-b6c4-4fa5-8565-5ef1931a5c5c", "hhid"] <- "D122"
hh_data[hh_data$uuid == "399fb96b-af0b-4fb5-993e-8891c47d08e4", "hhid"] <- "D123"
hh_data[hh_data$uuid == "115ee76f-0233-45d6-8e56-135501bb1bb1", "hhid"] <- "P077"
hh_data[hh_data$uuid == "457c1a45-cada-4b30-8ece-7bd27811c7f2", "hhid"] <- "P078"
hh_data[hh_data$uuid == "696c8457-e0f1-4859-8ee5-efe929e43b9e", "hhid"] <- "P079"
hh_data[hh_data$uuid == "58396db6-a3c1-47bb-b99d-524f55f3bbf6", "hhid"] <- "P080"
hh_data[hh_data$uuid == "1cd63eab-9445-4bdd-94fa-814c06b2d2ea", "hhid"] <- "P081"
hh_data[hh_data$uuid == "852c85f4-bca6-47aa-9879-b49ec8723e71", "hhid"] <- "P082"
hh_data[hh_data$uuid == "ac59957f-cd00-4e30-b771-5cffb9f8bfb7", "hhid"] <- "P083"
hh_data[hh_data$uuid == "3eebb03f-d80b-42e4-8839-3b0b08d7e18f", "hhid"] <- "P084"
hh_data[hh_data$uuid == "2ffed453-f986-4514-a419-9b9bef0cef95", "hhid"] <- "P085"
hh_data[hh_data$uuid == "381f113e-b9f0-41b1-9713-a1da562efd28", "hhid"] <- "P086"
hh_data[hh_data$uuid == "7888ffd8-e7d4-4563-91a3-78672b934008", "hhid"] <- "P087"
hh_data[hh_data$uuid == "42db454d-d187-415c-a750-921cc5c9ecc9", "hhid"] <- "P088"
hh_data[hh_data$uuid == "d89b86ac-5f29-4ec9-9dc7-658bcf040aaf", "hhid"] <- "P089"
hh_data[hh_data$uuid == "9d3a168a-3de5-4279-83a2-dbd5221a4eab", "hhid"] <- "P090"
hh_data[hh_data$uuid == "0350caba-20e1-4e86-bb30-c760486668ee", "hhid"] <- "P091"
hh_data[hh_data$uuid == "a4f943f8-702a-41b3-8f53-e8893ffe43f4", "hhid"] <- "P092"
hh_data[hh_data$uuid == "36a6ce64-ae1c-46c4-b058-614a957f4cc9", "hhid"] <- "P093"
hh_data[hh_data$uuid == "5c1afed1-353c-4c50-8ffb-b5f5fc5b2d69", "hhid"] <- "P094"
hh_data[hh_data$uuid == "db7117ff-5d01-4ea8-b631-2fa9e93d1ba6", "hhid"] <- "P095"
hh_data[hh_data$uuid == "fafd77f0-cd47-4b5d-800d-4822c419f3ae", "hhid"] <- "C109"
hh_data[hh_data$uuid == "814b4a53-f9f6-4311-87f3-f8c67eb48721", "hhid"] <- "C110"
hh_data[hh_data$uuid == "aa782258-d691-4707-befa-6a0b7940c438", "hhid"] <- "C111"
hh_data[hh_data$uuid == "6a4fab26-c6b7-48ba-8e77-2b6ed2bc3fa3", "hhid"] <- "C112"
hh_data[hh_data$uuid == "2568ce15-e455-40a4-ad14-13d13c1cbcfc", "hhid"] <- "C113"
hh_data[hh_data$uuid == "b06c3174-8c03-4a43-86e0-046a6dd18f9f", "hhid"] <- "C114"
hh_data[hh_data$uuid == "fc2ee0ad-21cb-40a6-b882-e7e9416c6be2", "hhid"] <- "C115"
hh_data[hh_data$uuid == "7a7f59a6-a05c-4126-a130-dff847fff8ee", "hhid"] <- "C116"
hh_data[hh_data$uuid == "6759f219-fd89-4141-9237-b42faaa5fa67", "hhid"] <- "D106"
hh_data[hh_data$uuid == "91ca0f86-b597-4f84-929e-5613bd26c764", "hhid"] <- "D069"
hh_data[hh_data$uuid == "9addc381-17c8-46c3-99cb-0fa92e2a41f8", "hhid"] <- "D124"
hh_data[hh_data$uuid == "bd8de69d-7a7e-42d9-979d-a652186c6a18", "hhid"] <- "D125"
hh_data[hh_data$uuid == "17d7b4f9-f254-47b7-b0b4-affc303a5361", "hhid"] <- "D070"
hh_data[hh_data$uuid == "01fe9ec0-31b3-42f0-985f-9e92a44ebed5", "hhid"] <- "D107"
hh_data[hh_data$uuid == "17530cce-66ab-42b3-9f41-21ef87daf6bb", "hhid"] <- "D108"
hh_data[hh_data$uuid == "eebea2c9-c357-4d70-be71-59d6b05f681f", "hhid"] <- "D126"
hh_data[hh_data$uuid == "b0ddb029-eb6e-4dde-9c31-9b143878ed47", "hhid"] <- "P096"
hh_data[hh_data$uuid == "8fe70934-109c-43ff-9506-9503f2372a8e", "hhid"] <- "D127"
hh_data[hh_data$uuid == "ef622a00-fe84-4fc9-8203-a061ec05f4d8", "hhid"] <- "D128"
hh_data[hh_data$uuid == "d18fd4a5-f59d-4634-ae6c-0422ec0c1757", "hhid"] <- "D129"
hh_data[hh_data$uuid == "64feaaba-c348-4150-9a3e-f29353538c44", "hhid"] <- "D130"
hh_data[hh_data$uuid == "3307ea5a-c12e-4782-8553-67b6dc11791d", "hhid"] <- "D131"
hh_data[hh_data$uuid == "1e325fad-f473-4b32-a814-ca479fc229ea", "hhid"] <- "D132"
hh_data[hh_data$uuid == "3f689148-4465-48f4-818c-a8087c68db82", "hhid"] <- "D133"
hh_data[hh_data$uuid == "636a5b2a-927d-4dd7-b06f-4f5642a35efa", "hhid"] <- "D140"
hh_data[hh_data$uuid == "57013807-322e-44c4-9b65-05b623c9ff5a", "hhid"] <- "D141"
hh_data[hh_data$uuid == "70a149ae-1528-4c41-94f2-20dae09d3b48", "hhid"] <- "D142"
hh_data[hh_data$uuid == "a9761821-694e-4cbb-ac19-2f4e063e0639", "hhid"] <- "D143"
hh_data[hh_data$uuid == "5ce8fb64-27b2-4eb7-ab1a-2af1dd2374de", "hhid"] <- "D144"
hh_data[hh_data$uuid == "71b8a84e-e768-434b-aace-c4753478eafd", "hhid"] <- "D145"
hh_data[hh_data$uuid == "58206da9-5f19-4ee8-8e4e-b850d7699000", "hhid"] <- "D146"
hh_data[hh_data$uuid == "5362a896-8c3e-4ea1-ab31-ad9492bd32c5", "hhid"] <- "D147"
hh_data[hh_data$uuid == "53113f49-15ad-4f58-b9a6-bb2d9af71b85", "hhid"] <- "D148"
hh_data[hh_data$uuid == "861df033-5e14-470c-a318-91086819dbf2", "hhid"] <- "D149"
hh_data[hh_data$uuid == "d0188224-a0b5-4ddd-91f1-84f0aa011cdd", "hhid"] <- "D150"
hh_data[hh_data$uuid == "e54684d0-34e6-4918-b914-98c543488f85", "hhid"] <- "D151"
hh_data[hh_data$uuid == "7daffbfa-be98-405d-b59e-da17ab96e1b4", "hhid"] <- "D152"
hh_data[hh_data$uuid == "cf6003e8-a6a0-41e3-a82e-1fffd87e27cf", "hhid"] <- "D153"
hh_data[hh_data$uuid == "e8036b1b-80fb-4137-a108-64e6ae85b7ac", "hhid"] <- "D154"
hh_data[hh_data$uuid == "6ddd7a94-7f35-4159-a503-ea0459e64a2d", "hhid"] <- "D155"
hh_data[hh_data$uuid == "9bb815e8-4b9f-4964-b824-e42cb3fbce51", "hhid"] <- "D156"
hh_data[hh_data$uuid == "f091cbef-04e4-48f2-b0b7-c3c5aa283b90", "hhid"] <- "D157"
hh_data[hh_data$uuid == "fd911b4c-a105-4028-843c-959c39f0bf19", "hhid"] <- "D158"
hh_data[hh_data$uuid == "7ee64854-7d0d-49eb-b4c7-56b981c3de6a", "hhid"] <- "D159"
hh_data[hh_data$uuid == "caefd02d-508a-4aa7-988a-5b4c2b7a1ab4", "hhid"] <- "D160"
hh_data[hh_data$uuid == "097a675a-0e80-4762-8dcb-5d15fc00bc8f", "hhid"] <- "P097"
hh_data[hh_data$uuid == "b76c627c-2f88-4186-b35a-947e7d8ed758", "hhid"] <- "P098"
hh_data[hh_data$uuid == "255804a7-de80-49a1-955f-16e0a4903e8e", "hhid"] <- "P099"
hh_data[hh_data$uuid == "86787e20-3eb7-4414-b1a5-b7b7ad2a7866", "hhid"] <- "P100"
hh_data[hh_data$uuid == "7ce517cf-ebb9-48a1-8ba2-ef90ecb53113", "hhid"] <- "P101"
hh_data[hh_data$uuid == "d109691d-5788-4ebc-9a20-232922d65977", "hhid"] <- "P102"
hh_data[hh_data$uuid == "8562657a-fb94-40ab-bf79-7d9bafbdf4ca", "hhid"] <- "P103"
hh_data[hh_data$uuid == "8cac6429-9a7b-4f51-b64d-f75f57b9ff41", "hhid"] <- "P104"
hh_data[hh_data$uuid == "4f46ca4d-2cad-423c-9b36-a365f8fc79bd", "hhid"] <- "P105"
hh_data[hh_data$uuid == "406c7e4f-643e-4274-8902-9b9a10a0ed82", "hhid"] <- "P106"
hh_data[hh_data$uuid == "34621e5f-dbb4-4ad7-9d48-24e266b78f04", "hhid"] <- "P107"
hh_data[hh_data$uuid == "76946c38-0709-4455-b3a6-df0618748f39", "hhid"] <- "P108"
hh_data[hh_data$uuid == "63b9af14-cd8b-4d0a-a555-abf32bd758ec", "hhid"] <- "P109"
hh_data[hh_data$uuid == "f568e5d0-3ddd-4083-b32b-aede19bcdaba", "hhid"] <- "P110"
hh_data[hh_data$uuid == "d71cdd6d-12a7-488d-bb07-2777a93da059", "hhid"] <- "P111"
hh_data[hh_data$uuid == "3c164ffd-2ba2-4a67-a40b-873f2158dd33", "hhid"] <- "P112"
hh_data[hh_data$uuid == "186f7567-8fba-4771-82b3-a34716f97ac0", "hhid"] <- "C117"
hh_data[hh_data$uuid == "212c51b1-b242-492e-a7d6-66b662ff4f7f", "hhid"] <- "C118"
hh_data[hh_data$uuid == "283d1126-ae2b-4370-802f-7cdcb989ae8f", "hhid"] <- "C119"
hh_data[hh_data$uuid == "b7eba4e5-8e5c-464d-876d-23f50eb0cb79", "hhid"] <- "C120"
hh_data[hh_data$uuid == "809620ad-bb12-4901-849b-5f203e7b4577", "hhid"] <- "C121"
hh_data[hh_data$uuid == "1f580253-539d-4e76-bfde-a6f6551612f3", "hhid"] <- "C122"
hh_data[hh_data$uuid == "7b1da360-5aff-4fb8-8a67-35e0c65f7e55", "hhid"] <- "C123"
hh_data[hh_data$uuid == "fc07c76b-1223-4bff-8404-5abe82bc0d8d", "hhid"] <- "C124"
hh_data[hh_data$uuid == "31f2d458-ed7e-457b-b527-c94068f88e68", "hhid"] <- "C125"
hh_data[hh_data$uuid == "b564485d-61d2-4d6f-a620-4031568302e5", "hhid"] <- "C126"
hh_data[hh_data$uuid == "cf521c3e-4457-4b0f-9940-1afb1bd638d2", "hhid"] <- "C127"
hh_data[hh_data$uuid == "720087f3-7f90-4c5a-93b2-7f6cd59aaa7e", "hhid"] <- "C128"
hh_data[hh_data$uuid == "a8ae4854-c274-4b1e-931a-1b00913f37e8", "hhid"] <- "C129"
hh_data[hh_data$uuid == "990f39f2-ca0a-4510-9beb-0fefea5951b1", "hhid"] <- "D134"
hh_data[hh_data$uuid == "55d80978-bb49-48b5-9464-ea9d45f1c6c0", "hhid"] <- "D135"
hh_data[hh_data$uuid == "55ae2adf-1809-4553-a54c-b9d2ab5d853a", "hhid"] <- "D161"
hh_data[hh_data$uuid == "47f99d48-f26a-4270-9ee8-c8b219986fdc", "hhid"] <- "D162"
hh_data[hh_data$uuid == "21925d17-7dd2-40c8-ac42-28dda80be86e", "hhid"] <- "D136"
hh_data[hh_data$uuid == "05961c13-7d90-4040-a020-0e5a4333422d", "hhid"] <- "D137"
hh_data[hh_data$uuid == "f34c1bbb-ba23-4bcd-85ed-ce722773a5bc", "hhid"] <- "D138"
hh_data[hh_data$uuid == "e621ea3b-e436-4a9d-8f87-3717a9871494", "hhid"] <- "D163"
hh_data[hh_data$uuid == "ca00e9e0-b85f-4f86-9e3c-e9a5e2154a3b", "hhid"] <- "D171"
hh_data[hh_data$uuid == "ab305164-d7c5-40bc-a00c-07ec54294959", "hhid"] <- "D172"
hh_data[hh_data$uuid == "2f4ba304-57a2-494e-b77b-059f96c91fd5", "hhid"] <- "D173"
hh_data[hh_data$uuid == "55ee7ee8-a4d3-486a-b79c-d6d9d6a7b45d", "hhid"] <- "D164"
hh_data[hh_data$uuid == "09a3066f-dda7-4484-8711-627f645036f7", "hhid"] <- "D174"
hh_data[hh_data$uuid == "6d178dfa-ed2c-4d2f-b9c9-1c101388bbc0", "hhid"] <- "D175"
hh_data[hh_data$uuid == "73bbb766-759e-4770-99a4-ac3f5e122f09", "hhid"] <- "D176"
hh_data[hh_data$uuid == "dd53125d-3902-4dc4-b631-f88fc3a9b43d", "hhid"] <- "D177"
hh_data[hh_data$uuid == "6d842ee4-c06d-4e26-ad33-ba09d8d346cf", "hhid"] <- "D178"
hh_data[hh_data$uuid == "64f29c59-d2fd-4443-aadc-6ef0856eb64e", "hhid"] <- "D179"
hh_data[hh_data$uuid == "ee0311c8-dee0-4000-b153-f5072c55810b", "hhid"] <- "D180"
hh_data[hh_data$uuid == "d0057282-9fc6-4f00-ae54-2d00ba6800b1", "hhid"] <- "D181"
hh_data[hh_data$uuid == "282eb09e-d43e-44f6-803c-3a205046618e", "hhid"] <- "D182"
hh_data[hh_data$uuid == "ad360919-5323-4cfa-aa6d-7aed099c7984", "hhid"] <- "D183"
hh_data[hh_data$uuid == "e72ac9cc-a126-4a8e-a069-d3615f2a6ec6", "hhid"] <- "D184"
hh_data[hh_data$uuid == "356a3a89-5e92-4e80-bb00-7b588f97e54b", "hhid"] <- "C130"
hh_data[hh_data$uuid == "90a034eb-dda6-47bc-8af1-cda1cf31ffd0", "hhid"] <- "C131"
hh_data[hh_data$uuid == "bd0b6393-124c-4b87-a128-60480beccf34", "hhid"] <- "C132"
hh_data[hh_data$uuid == "c3243c02-6e64-4340-818a-e5feff51c09b", "hhid"] <- "C133"
hh_data[hh_data$uuid == "701a8670-edcc-418f-b0b1-def1c9bcebe3", "hhid"] <- "C134"
hh_data[hh_data$uuid == "d819cd50-556e-474c-8d7c-14d04c839cdd", "hhid"] <- "C135"
hh_data[hh_data$uuid == "46955719-93c5-4a3c-93c0-3660ade3f0c5", "hhid"] <- "C136"
hh_data[hh_data$uuid == "8c03178f-6be9-45e3-845a-03c55ab952f3", "hhid"] <- "C137"
hh_data[hh_data$uuid == "d916419c-4b24-4d93-8a99-45e674ee1b1c", "hhid"] <- "C138"
hh_data[hh_data$uuid == "a6ea3220-3735-422e-8a7a-edbb73629726", "hhid"] <- "C139"
hh_data[hh_data$uuid == "9b3aac41-4b7b-4ba5-b54a-2c878d8d120e", "hhid"] <- "D185"
hh_data[hh_data$uuid == "1b0e2a5a-ef16-4c89-8f61-14bb63f46210", "hhid"] <- "D139"
hh_data[hh_data$uuid == "04699513-2a3e-4226-a350-405aa20615e0", "hhid"] <- "D186"
hh_data[hh_data$uuid == "34f5a053-9784-4974-bce8-12d919c941ca", "hhid"] <- "D187"
hh_data[hh_data$uuid == "dc0ed15e-13cf-4bc2-990a-532b30906a04", "hhid"] <- "D188"
hh_data[hh_data$uuid == "290a4745-a941-45e6-96cf-86bf45f62900", "hhid"] <- "D165"
hh_data[hh_data$uuid == "a8eacc35-5b9b-4c8b-95c5-8fdb7427e588", "hhid"] <- "D166"
hh_data[hh_data$uuid == "131ae7f4-ca31-4cea-ac11-c8c4d238d754", "hhid"] <- "D167"
hh_data[hh_data$uuid == "5f09314b-9a3e-49c3-a8e5-6d9a88540f38", "hhid"] <- "D168"
hh_data[hh_data$uuid == "63e489dc-f561-44aa-99c4-6fd4a7198c64", "hhid"] <- "C140"
hh_data[hh_data$uuid == "647d4e83-da8b-4063-8b6f-ef744ef7ad40", "hhid"] <- "C141"
hh_data[hh_data$uuid == "87a2e5e0-be6e-404f-b6eb-e487500d42a5", "hhid"] <- "C142"
hh_data[hh_data$uuid == "a1300eed-1ee8-4597-a6d9-6fcbc282a564", "hhid"] <- "C143"
hh_data[hh_data$uuid == "a768dc23-3691-4e43-991f-50307aae75fe", "hhid"] <- "C144"
hh_data[hh_data$uuid == "3c948753-ae13-4c5a-97eb-48a17ce8d503", "hhid"] <- "C145"
hh_data[hh_data$uuid == "5f5c4e7e-8c0e-4e82-920b-bf0f1672d585", "hhid"] <- "C146"
hh_data[hh_data$uuid == "4376eb8d-e00f-4748-9900-a243ef59ba41", "hhid"] <- "C147"
hh_data[hh_data$uuid == "92f16e21-4452-4776-8664-994a1b1b0110", "hhid"] <- "C148"
hh_data[hh_data$uuid == "50e2e70e-e7b2-4bb9-9cf4-bc8c0d0cabaa", "hhid"] <- "C149"
hh_data[hh_data$uuid == "e4d67faa-5f50-46be-b97c-3cba5ba8343b", "hhid"] <- "C150"
hh_data[hh_data$uuid == "0b07b01d-edf8-44e3-a9df-abb828cc2fbc", "hhid"] <- "C151"
hh_data[hh_data$uuid == "cb3ca5d8-5f02-4732-b9e7-d672f5fa556a", "hhid"] <- "C152"
hh_data[hh_data$uuid == "c7ccac21-b81b-44b7-a497-59629465bb71", "hhid"] <- "C153"
hh_data[hh_data$uuid == "914f6a10-44b2-480c-8e35-e90b7227ccf8", "hhid"] <- "C154"
hh_data[hh_data$uuid == "2ffa0a7f-1ff5-43ed-82f8-aa208b0d9c50", "hhid"] <- "C155"
hh_data[hh_data$uuid == "ad4c04f9-728a-4eaf-b0ba-af986d91c9ba", "hhid"] <- "C156"
hh_data[hh_data$uuid == "74e5f4d2-5d99-41b1-b42b-8543aae5dd2b", "hhid"] <- "C157"
hh_data[hh_data$uuid == "bb0d8777-d499-40e0-81ac-468eb59ddcc0", "hhid"] <- "C158"
hh_data[hh_data$uuid == "b5c34187-3c1a-4336-bfc1-a9c48adb24f0", "hhid"] <- "C159"
hh_data[hh_data$uuid == "6099bc07-3705-420a-918b-7fe277d8507c", "hhid"] <- "D193"
hh_data[hh_data$uuid == "f7c8a979-9922-4fbd-a9d9-50a7a9bef621", "hhid"] <- "D194"
hh_data[hh_data$uuid == "ca8f999a-213c-45f1-a1ee-547e76b3bc22", "hhid"] <- "D198"
hh_data[hh_data$uuid == "5a8e85ed-ef15-4de2-8456-09dca66aa0d3", "hhid"] <- "D199"
hh_data[hh_data$uuid == "4d93d6b5-147a-4d71-a838-ae36fcb3dc28", "hhid"] <- "D200"
hh_data[hh_data$uuid == "4ab4d1a5-3348-4d81-811f-a716b1950bb1", "hhid"] <- "D201"
hh_data[hh_data$uuid == "e62ca187-bda9-49fe-bd74-b979514d0e17", "hhid"] <- "D202"
hh_data[hh_data$uuid == "9dc5bc7e-ab44-43c1-a8f9-39bad0a94d51", "hhid"] <- "D203"
hh_data[hh_data$uuid == "cd07aaaf-473f-44cb-8719-b522c8013afa", "hhid"] <- "D204"
hh_data[hh_data$uuid == "5cd9d057-4707-4277-b7a8-7638cd7a3e15", "hhid"] <- "D205"
hh_data[hh_data$uuid == "3774cd69-25d1-42ad-99f2-65d24f5dda0e", "hhid"] <- "D206"
hh_data[hh_data$uuid == "79228e66-88d0-43be-b907-f52b41f6c1bf", "hhid"] <- "D207"
hh_data[hh_data$uuid == "519ff181-03ed-4324-b5f9-3e21f18c026d", "hhid"] <- "D208"
hh_data[hh_data$uuid == "55dc4a63-cc3c-4b71-b75e-713b9d972fd1", "hhid"] <- "D209"
hh_data[hh_data$uuid == "d91a31f9-a454-482c-afd6-2071fc4d0c79", "hhid"] <- "D210"
hh_data[hh_data$uuid == "95063fa4-3bfb-4f58-ad14-b0bde53508d2", "hhid"] <- "P113"
hh_data[hh_data$uuid == "fb14cd86-04fc-4322-9158-1aedca3972d1", "hhid"] <- "P114"
hh_data[hh_data$uuid == "812ee0ec-1e0d-4cb8-9a2e-9ca7aed48a64", "hhid"] <- "P115"
hh_data[hh_data$uuid == "cda1abb9-c0d4-4ffd-8e09-de9f35f953ab", "hhid"] <- "D211"
hh_data[hh_data$uuid == "9853f8b9-b94c-4ac7-b617-b5ba8394c7b5", "hhid"] <- "P116"
hh_data[hh_data$uuid == "1fbfe905-92b6-4452-931a-c7664fef6fe0", "hhid"] <- "P117"
hh_data[hh_data$uuid == "27082fc7-6cf1-4c69-a3e6-01a10bfa5497", "hhid"] <- "P118"
hh_data[hh_data$uuid == "ea571b52-c909-407d-bd32-c43194116f2c", "hhid"] <- "P119"
hh_data[hh_data$uuid == "2af38915-624b-420b-b695-047c2f498314", "hhid"] <- "P120"
hh_data[hh_data$uuid == "d1cb8eb6-abb9-47f4-a5e9-9c4bba9dc07b", "hhid"] <- "P121"
hh_data[hh_data$uuid == "fd72637c-126f-4ad4-88ca-4c434bd719ff", "hhid"] <- "P122"
hh_data[hh_data$uuid == "eecf8c64-9308-4c9e-b7c4-524919d0e513", "hhid"] <- "P123"
hh_data[hh_data$uuid == "becb4b1b-2fda-4a73-8f1a-0055b12f07c0", "hhid"] <- "P124"
hh_data[hh_data$uuid == "d287f897-d6c9-4c1f-a186-9491237736ef", "hhid"] <- "P125"
hh_data[hh_data$uuid == "a3c3a53e-3925-49da-b486-d3459fdfe8eb", "hhid"] <- "P126"
hh_data[hh_data$uuid == "9273eecb-9242-4bae-8fc1-32131b7469fd", "hhid"] <- "P127"
hh_data[hh_data$uuid == "8af7e12c-710f-4b36-9ae3-2d18c523d814", "hhid"] <- "P128"
hh_data[hh_data$uuid == "8f06ce34-21c9-4bfe-b82b-eb95ffab7f1d", "hhid"] <- "P129"
hh_data[hh_data$uuid == "106ad963-90f1-4d1e-9e9f-1bb3e7f08a09", "hhid"] <- "P130"
hh_data[hh_data$uuid == "cd115ad7-a766-4457-980c-5840df18c616", "hhid"] <- "P131"
hh_data[hh_data$uuid == "5f4f952c-ebda-4b8e-8272-4a7d88627947", "hhid"] <- "D212"
hh_data[hh_data$uuid == "6afa92bf-739f-499c-9ef5-879f1ba4d1e3", "hhid"] <- "D169"
hh_data[hh_data$uuid == "ce7c94b2-c8d4-4cb6-ad93-c48bdf83f289", "hhid"] <- "D189"
hh_data[hh_data$uuid == "cfaf6330-9d68-4ee9-93c8-9d89d799c269", "hhid"] <- "D213"
hh_data[hh_data$uuid == "443dcc6c-e15a-4292-8b87-8231bb9c2c1b", "hhid"] <- "D214"
hh_data[hh_data$uuid == "975d8e6e-89ae-4f14-be5a-55069759679b", "hhid"] <- "D215"
hh_data[hh_data$uuid == "f65f46e4-8394-468f-b216-18dd04baf66c", "hhid"] <- "D216"
hh_data[hh_data$uuid == "e370d31c-f535-4c15-bda7-61a056279fe3", "hhid"] <- "D170"
hh_data[hh_data$uuid == "bd1b2936-e60b-45b9-9141-95422831b897", "hhid"] <- "D217"
hh_data[hh_data$uuid == "05aac145-b554-4746-8356-acdf3c65e09d", "hhid"] <- "D218"
hh_data[hh_data$uuid == "f27eea83-af22-4391-b60a-79849d80b3a5", "hhid"] <- "P132"
hh_data[hh_data$uuid == "06c24763-f967-4624-b9ff-51719be8d75c", "hhid"] <- "P133"
hh_data[hh_data$uuid == "b40ac557-4153-43ef-97c8-428aa2cacbf6", "hhid"] <- "P134"
hh_data[hh_data$uuid == "fb4ff967-374f-41bc-a428-7f32d0bbf35b", "hhid"] <- "P135"
hh_data[hh_data$uuid == "fc068824-fb2c-4ee7-bbd8-bebb11dc54b1", "hhid"] <- "D219"
hh_data[hh_data$uuid == "50840b99-68c1-4ef0-a3ba-ea78c3d882d3", "hhid"] <- "D220"
hh_data[hh_data$uuid == "342606ed-58a5-4997-a49a-69bc4eb62c37", "hhid"] <- "D221"
hh_data[hh_data$uuid == "91264e3f-0248-42e1-86cc-56913d85c8e3", "hhid"] <- "D222"
hh_data[hh_data$uuid == "81bed89f-c128-4582-87e6-f45d2310c387", "hhid"] <- "P136"
hh_data[hh_data$uuid == "070e157a-e275-49cb-ad74-46bd8a5b99f2", "hhid"] <- "D223"
hh_data[hh_data$uuid == "ed8513ff-65f3-4834-8eac-07327a93b7b4", "hhid"] <- "D224"
hh_data[hh_data$uuid == "683f35c4-132e-4fcc-9473-2c7314c31bdb", "hhid"] <- "D225"
hh_data[hh_data$uuid == "b00e109d-c5d0-43f1-8dda-2e65f3000e70", "hhid"] <- "D226"
hh_data[hh_data$uuid == "0b69cc86-dca0-4bda-877f-20f5237ce992", "hhid"] <- "D227"
hh_data[hh_data$uuid == "49b8b18e-b163-4aeb-90fa-f67ba2a6b96e", "hhid"] <- "D228"
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "hhid"] <- "D229"
hh_data[hh_data$uuid == "e150513b-d8ed-40a1-833a-8b8ade718a4d", "hhid"] <- "P137"
hh_data[hh_data$uuid == "8b9930b4-68a5-4f01-bf32-653786808797", "hhid"] <- "P138"
hh_data[hh_data$uuid == "c563e8e0-fe19-4aff-99a5-f8df49da51f4", "hhid"] <- "P139"
hh_data[hh_data$uuid == "9ad2fc74-25b3-4695-a4ec-044ec2a074c5", "hhid"] <- "P140"
hh_data[hh_data$uuid == "24b915f0-3b36-44c7-ba2e-08893650ad29", "hhid"] <- "P141"
hh_data[hh_data$uuid == "210e94d7-6380-4443-8386-31cc8e99a46f", "hhid"] <- "P142"
hh_data[hh_data$uuid == "cb987bc2-7e95-4a2c-9014-c819a13e043f", "hhid"] <- "P143"
hh_data[hh_data$uuid == "f66648a1-b530-4e11-bcfd-4e804c191379", "hhid"] <- "P144"
hh_data[hh_data$uuid == "41013005-1b5e-4e5a-a7a1-6a2039b2fddd", "hhid"] <- "P145"
hh_data[hh_data$uuid == "889d42e8-387b-4808-b067-0b8cec5d66f9", "hhid"] <- "P146"
hh_data[hh_data$uuid == "5a76bd33-def8-4cb7-8b31-82531d81dce2", "hhid"] <- "P147"
hh_data[hh_data$uuid == "3732563f-782f-493b-83fe-d4004368abb2", "hhid"] <- "P148"
hh_data[hh_data$uuid == "a5399a81-adc1-4aff-8167-f7754d449f23", "hhid"] <- "D230"
hh_data[hh_data$uuid == "e23c0607-db63-44ae-beb9-819b7b709979", "hhid"] <- "D231"
hh_data[hh_data$uuid == "bb4c652f-6c72-4d32-92ba-3570bd140f97", "hhid"] <- "D232"
hh_data[hh_data$uuid == "2cc919dd-1287-4fe2-9b9c-a93cc984eadd", "hhid"] <- "D233"
hh_data[hh_data$uuid == "77b58b53-cc7b-4e59-9d09-45663695893f", "hhid"] <- "D234"
hh_data[hh_data$uuid == "0a7d4c30-0651-4342-adc5-5da4348c4095", "hhid"] <- "D235"
hh_data[hh_data$uuid == "d82b1532-82c7-4fa6-a51f-f9556b8501b8", "hhid"] <- "D236"
hh_data[hh_data$uuid == "c1d52371-3fc5-4cee-afd3-02fc8ec10e95", "hhid"] <- "D237"
hh_data[hh_data$uuid == "74b611f6-1829-4251-a884-63bd7dce106b", "hhid"] <- "D238"
hh_data[hh_data$uuid == "5170b982-dafb-476c-81eb-20ab016b03db", "hhid"] <- "D239"
hh_data[hh_data$uuid == "4b94ef88-5dec-41bd-bd73-10785c5b8ddd", "hhid"] <- "D240"
hh_data[hh_data$uuid == "9a6b58ec-b8be-4d7f-9894-2bfeedd9b614", "hhid"] <- "D190"
hh_data[hh_data$uuid == "03c5feca-27f3-4991-99e2-b4a794235015", "hhid"] <- "D191"
hh_data[hh_data$uuid == "7c9bd3ee-8820-4421-8331-2353c4e2ce71", "hhid"] <- "D192"
hh_data[hh_data$uuid == "9c2427ac-25ef-4055-b0b3-9b650f8756c2", "hhid"] <- "D195"
hh_data[hh_data$uuid == "de6a3334-69d8-4891-950d-a417fb86ab46", "hhid"] <- "D196"
hh_data[hh_data$uuid == "180a5752-3f75-49f7-ac35-2f09761ad662", "hhid"] <- "D197"
hh_data[hh_data$uuid == "1c67a3f5-84a5-423b-9250-e85e44bed5f9", "hhid"] <- "D241"
hh_data[hh_data$uuid == "d9cdd5a6-a726-4173-921f-bec8c56198d7", "hhid"] <- "C160"
hh_data[hh_data$uuid == "ef7ca940-a84b-489a-bd99-27dfe54434b7", "hhid"] <- "C161"
hh_data[hh_data$uuid == "e50979a0-9531-4e53-8c5b-c1f5a2df426b", "hhid"] <- "C162"
hh_data[hh_data$uuid == "7816d428-162e-44b9-8b07-79e05be05d0c", "hhid"] <- "C163"
hh_data[hh_data$uuid == "c8c7aeaa-df02-4ed1-9751-aec0bad4616d", "hhid"] <- "C164"
hh_data[hh_data$uuid == "385b8ffa-61f5-4a43-bf72-0ead0df65518", "hhid"] <- "C165"
hh_data[hh_data$uuid == "7e3cead2-3df8-47b1-a6a0-f6c2826dfc23", "hhid"] <- "C166"
hh_data[hh_data$uuid == "dabf2d2b-06dc-4744-af9b-024789a53971", "hhid"] <- "C167"
hh_data[hh_data$uuid == "a782cc58-9427-4c9b-8de4-bd8959af24be", "hhid"] <- "C168"
hh_data[hh_data$uuid == "48b64981-f70f-4faa-b994-e33c901d342e", "hhid"] <- "C169"
hh_data[hh_data$uuid == "eca4b1cf-345b-403c-bbf9-0da5cfe84b53", "hhid"] <- "C170"
hh_data[hh_data$uuid == "12cd50e2-ea3a-4c8c-b757-05a80c48ae47", "hhid"] <- "D242"
hh_data[hh_data$uuid == "167b1892-cb30-4b58-a6ed-a6133434d87b", "hhid"] <- "D243"
hh_data[hh_data$uuid == "d2dc952a-9219-4ca6-a789-0bc7158208d7", "hhid"] <- "D244"
hh_data[hh_data$uuid == "188c55b9-ada2-4b73-a49f-2cb604078848", "hhid"] <- "D245"
hh_data[hh_data$uuid == "9689c946-2a1b-42fc-8aa0-141f75005ff9", "hhid"] <- "D246"
hh_data[hh_data$uuid == "367e8bdc-49f7-4f56-9335-c014e31883ea", "hhid"] <- "D247"
hh_data[hh_data$uuid == "6769294c-cb33-46e1-84ef-a92f58d28a4a", "hhid"] <- "D248"
hh_data[hh_data$uuid == "d5e66538-f980-4297-b6b7-b84bf7bd62a6", "hhid"] <- "D254"
hh_data[hh_data$uuid == "23a3a765-8d8d-4e48-ab4c-5bcb094cf100", "hhid"] <- "D255"
hh_data[hh_data$uuid == "58f718c5-1c25-46f2-b937-fa554f809357", "hhid"] <- "D256"
hh_data[hh_data$uuid == "408552fd-b0f4-4d04-aa4f-9dc5cd75b929", "hhid"] <- "D257"
hh_data[hh_data$uuid == "473660d8-ea4c-48b6-8edc-6ecc4330e500", "hhid"] <- "P149"
hh_data[hh_data$uuid == "55e95987-51bb-44ea-8307-9922510fa0e1", "hhid"] <- "P150"
hh_data[hh_data$uuid == "b823dfc1-6ecf-42ff-959a-1a17e45033a6", "hhid"] <- "P151"
hh_data[hh_data$uuid == "a259fd73-cddb-492f-803e-63061ff7ee6b", "hhid"] <- "P152"
hh_data[hh_data$uuid == "183bf3a0-da5b-4f58-afec-75bba4e4d5d2", "hhid"] <- "P153"
hh_data[hh_data$uuid == "6a05e8c3-0322-493e-8b06-54b8d1d0ddb9", "hhid"] <- "P154"
hh_data[hh_data$uuid == "f3bb555a-acea-4c53-aa3d-e666e79b33d4", "hhid"] <- "P155"
hh_data[hh_data$uuid == "3eed6f6f-c249-42aa-a1a3-65f586233265", "hhid"] <- "P156"
hh_data[hh_data$uuid == "efe503e3-5a57-481d-a28c-8f3e0f9665ef", "hhid"] <- "D249"
hh_data[hh_data$uuid == "9389d1f7-db8c-4655-b273-6b2402748534", "hhid"] <- "D258"
hh_data[hh_data$uuid == "80e485e7-887d-42fb-bde5-0eb7bc49136b", "hhid"] <- "D259"
hh_data[hh_data$uuid == "82b7e777-4c0c-4d59-ad62-d9ad5be49365", "hhid"] <- "D260"
hh_data[hh_data$uuid == "992adfe5-78f6-4b92-afed-78c2a2569a9f", "hhid"] <- "D261"
hh_data[hh_data$uuid == "8e7f6e0b-a072-46b6-9a01-3df75654347f", "hhid"] <- "D262"
hh_data[hh_data$uuid == "a2d22dcb-6597-4329-9c3a-5e99ffd081c7", "hhid"] <- "D263"
hh_data[hh_data$uuid == "7b5a762e-321b-42b4-86af-e1883d36ce4b", "hhid"] <- "D264"
hh_data[hh_data$uuid == "2b15200c-b9c3-4be1-841a-a558efba5c48", "hhid"] <- "D265"
hh_data[hh_data$uuid == "6799e9e1-b86d-4f87-89e7-0e5301f63f48", "hhid"] <- "D266"
hh_data[hh_data$uuid == "bfd7f6df-29f4-42ac-8307-736514886309", "hhid"] <- "D267"
hh_data[hh_data$uuid == "e5abc284-8a85-4a0c-ba00-4de8f23d6ea4", "hhid"] <- "D268"
hh_data[hh_data$uuid == "cd5e433c-8ac0-4095-8d50-9d152a03748c", "hhid"] <- "D269"
hh_data[hh_data$uuid == "7b6d0e9d-3159-4480-9251-b4f2b42ff58d", "hhid"] <- "D270"
hh_data[hh_data$uuid == "480a4539-8e43-445e-a9a4-ef0ac2fabe5c", "hhid"] <- "D250"
hh_data[hh_data$uuid == "777e3672-bb47-48b8-bb06-cd3a8cc60588", "hhid"] <- "D271"
hh_data[hh_data$uuid == "ed549bf3-4104-4545-81db-ec8aa01f5adc", "hhid"] <- "D272"
hh_data[hh_data$uuid == "63672f67-cc79-444c-9eac-c7b71fbcc1ab", "hhid"] <- "D251"
hh_data[hh_data$uuid == "53d28a6c-024b-4964-b434-d4c969240856", "hhid"] <- "D252"
hh_data[hh_data$uuid == "9ef809d7-187a-4d7b-8e6e-5eaf0bd530ba", "hhid"] <- "D273"
hh_data[hh_data$uuid == "1f1d4014-4bc4-4c1d-b033-1cde0081ed2f", "hhid"] <- "D274"
hh_data[hh_data$uuid == "f7efb843-4ec5-4248-af66-97a33e91a1d9", "hhid"] <- "D280"
hh_data[hh_data$uuid == "bcd4f389-bb65-45a2-b415-c3685a9470a5", "hhid"] <- "D281"
hh_data[hh_data$uuid == "57dc326b-041a-4b6c-8caf-93a4841d4ccf", "hhid"] <- "D282"
hh_data[hh_data$uuid == "dce3628d-2cd1-4d7d-8c25-71e687e99acd", "hhid"] <- "D275"
hh_data[hh_data$uuid == "993351c7-836a-42be-a07f-ccc52cadebf8", "hhid"] <- "D276"
hh_data[hh_data$uuid == "0ac980aa-06a7-46e5-b7e4-d0f94c8b01c1", "hhid"] <- "D277"
hh_data[hh_data$uuid == "d0802f2b-e29b-45d1-9b66-b34424f244b0", "hhid"] <- "D253"
hh_data[hh_data$uuid == "5ca7c035-918e-48e9-acbe-6bbc2cba9a2b", "hhid"] <- "D283"
hh_data[hh_data$uuid == "80b3c20d-28c5-49e4-a7a5-7616e9ce1ed8", "hhid"] <- "D278"
hh_data[hh_data$uuid == "8300cd92-e4c7-4f3a-9b9f-45942ec97984", "hhid"] <- "D279"
hh_data[hh_data$uuid == "76a6e718-b737-4cb4-b59f-89e247e93e75", "hhid"] <- "P157"
hh_data[hh_data$uuid == "e9d01462-bac6-4ba7-8c46-2e945be79cbc", "hhid"] <- "P158"
hh_data[hh_data$uuid == "d7f98c9d-c61f-443e-8508-d2f81b5c62cf", "hhid"] <- "P159"
hh_data[hh_data$uuid == "fe63f4e7-00e7-49f4-ae9f-bf81838eda4d", "hhid"] <- "P160"
hh_data[hh_data$uuid == "3949be44-e5e2-4b83-92b3-8c1cb7875b67", "hhid"] <- "P161"
hh_data[hh_data$uuid == "fec8681e-f7ce-459c-9e22-ed34e4ca9328", "hhid"] <- "P162"
hh_data[hh_data$uuid == "59efa485-827d-4b23-8f33-c498af072a26", "hhid"] <- "P163"
hh_data[hh_data$uuid == "015cb3a0-fcd3-4571-bd2a-f6a58f2e5f4e", "hhid"] <- "P164"
hh_data[hh_data$uuid == "ae572aea-00a6-47f8-9e6f-0569963ec02a", "hhid"] <- "D284"
hh_data[hh_data$uuid == "70523af3-a16d-492e-9ab1-f72c6e63f157", "hhid"] <- "D285"
hh_data[hh_data$uuid == "a6d74994-a05a-4478-8d5e-5b729e60fa76", "hhid"] <- "D286"
hh_data[hh_data$uuid == "6d5527a2-7a9e-4b97-a239-fe3778f81db7", "hhid"] <- "D287"
hh_data[hh_data$uuid == "524915c0-cdf1-4838-94e3-6d95c334b6f3", "hhid"] <- "D288"
hh_data[hh_data$uuid == "793b4149-c332-4d96-82a7-9e2c09514475", "hhid"] <- "D289"
hh_data[hh_data$uuid == "e0504595-f4f5-46af-8a14-83bc96d54ce3", "hhid"] <- "D290"
hh_data[hh_data$uuid == "38c026c9-d15e-4173-b151-283686372a14", "hhid"] <- "D291"
hh_data[hh_data$uuid == "26d5759c-e964-46a6-81cf-3b8f38fb80e1", "hhid"] <- "D292"
hh_data[hh_data$uuid == "e486a86c-661d-4804-a684-70b02b06386c", "hhid"] <- "D293"
hh_data[hh_data$uuid == "96b6569c-b6b9-474d-9865-0472236493df", "hhid"] <- "D294"
hh_data[hh_data$uuid == "958593f9-ac57-4089-b1fc-ac089744278c", "hhid"] <- "D295"
hh_data[hh_data$uuid == "53f213f5-7ec1-4dbf-8ee2-fd72565495bd", "hhid"] <- "D296"
hh_data[hh_data$uuid == "ca749910-71b2-429a-945b-27e1bb077619", "hhid"] <- "D297"
hh_data[hh_data$uuid == "c62e280d-bf4d-4d09-8c9a-8fa128812eec", "hhid"] <- "D298"
hh_data[hh_data$uuid == "3929b19e-aba9-48ee-8448-b48adaf05c3f", "hhid"] <- "D299"
hh_data[hh_data$uuid == "02b79af7-1495-4221-8791-3823f2b7fd0f", "hhid"] <- "D300"
hh_data[hh_data$uuid == "70778eb3-5a98-4317-872e-e1a2508514dd", "hhid"] <- "D301"
hh_data[hh_data$uuid == "663f7002-6390-45b4-93af-e6138db0ba63", "hhid"] <- "D302"
hh_data[hh_data$uuid == "c3c6cbfd-0d74-49b4-8396-30415c6336f9", "hhid"] <- "D303"
hh_data[hh_data$uuid == "932cbfee-cd2d-4722-9834-e71f61cad69e", "hhid"] <- "D304"
hh_data[hh_data$uuid == "7989d7d0-e818-471d-b2a3-54dcc4ff37e1", "hhid"] <- "D305"
hh_data[hh_data$uuid == "d61f277e-12e1-4db9-b4b9-177081223558", "hhid"] <- "D306"
hh_data[hh_data$uuid == "531d2600-2e7c-4ff1-b89d-2fff276977a4", "hhid"] <- "D307"
hh_data[hh_data$uuid == "c4c0224f-2fbb-46da-bbee-7d331554a7b1", "hhid"] <- "D308"
hh_data[hh_data$uuid == "03beb62c-efd4-473a-92b6-3b310b4ea4d3", "hhid"] <- "D309"
hh_data[hh_data$uuid == "0b05f9bb-0b17-4422-a774-2cedf1508bde", "hhid"] <- "D310"
hh_data[hh_data$uuid == "98d473c0-7fe9-4e8b-a0cb-9e7066f7596b", "hhid"] <- "D311"
hh_data[hh_data$uuid == "c6e4386d-9345-4e7b-bf19-4f94f91fd441", "hhid"] <- "D312"
hh_data[hh_data$uuid == "5b852a0d-8708-4705-8e44-9cb7ffbbfbfc", "hhid"] <- "D313"
hh_data[hh_data$uuid == "50bd8adb-9bb8-49a0-a1e0-eac5a694a983", "hhid"] <- "D314"
hh_data[hh_data$uuid == "2dc26648-6d33-44ad-a924-9064606a6146", "hhid"] <- "D315"
hh_data[hh_data$uuid == "995b3ef8-caef-4e4a-be47-1b57dd27d007", "hhid"] <- "D316"
hh_data[hh_data$uuid == "21e42fec-9123-443c-b170-fb6343aac8ff", "hhid"] <- "D317"
hh_data[hh_data$uuid == "1cc523fa-006a-43c3-93c7-849cd70e9b76", "hhid"] <- "D318"
hh_data[hh_data$uuid == "eefa851b-4b6b-4c83-9b0d-1ecbdecf61b1", "hhid"] <- "D319"
hh_data[hh_data$uuid == "bf1759ab-67b2-41e6-808b-7a01d690d587", "hhid"] <- "D320"
hh_data[hh_data$uuid == "5f31b037-661b-4fda-80a4-e29e4bb950ad", "hhid"] <- "D321"
hh_data[hh_data$uuid == "d74c7471-fd41-4936-9524-a8bd406b03ea", "hhid"] <- "D322"
hh_data[hh_data$uuid == "95706fce-abee-45ef-b333-b323792a02a4", "hhid"] <- "D323"
hh_data[hh_data$uuid == "f2767ee3-a216-40c8-876d-1ec1f8959e3d", "hhid"] <- "D324"
hh_data[hh_data$uuid == "06ef9d1d-6886-44c0-8f19-84739cb62aea", "hhid"] <- "D325"
hh_data[hh_data$uuid == "b4131eb0-724d-4e20-a041-5e04411473aa", "hhid"] <- "D326"
hh_data[hh_data$uuid == "04bda1f8-962f-4893-91e5-84dfb7a47024", "hhid"] <- "D327"
hh_data[hh_data$uuid == "18a4c3e9-3ecc-42de-ba00-3e88079530e7", "hhid"] <- "D328"
hh_data[hh_data$uuid == "33558912-b7e9-47ba-9da4-91e5d6e72505", "hhid"] <- "D329"
hh_data[hh_data$uuid == "a0a4e283-eef3-4978-ba36-61b993e08308", "hhid"] <- "D330"
hh_data[hh_data$uuid == "48143d47-73c6-4f15-b369-2ed11986646f", "hhid"] <- "D331"
hh_data[hh_data$uuid == "7e329412-6899-47c4-ad65-7e78e8e20caf", "hhid"] <- "D332"
hh_data[hh_data$uuid == "00a47e19-1d49-4a56-bc60-7108a359e8fb", "hhid"] <- "D333"
hh_data[hh_data$uuid == "4bf65c67-8d7b-4e6c-adbe-21b8dcbf777d", "hhid"] <- "D334"
hh_data[hh_data$uuid == "2563cb5f-f323-4189-891c-83200b497d01", "hhid"] <- "D335"
hh_data[hh_data$uuid == "c03ac471-46ad-47ef-a656-a9e0848b42c4", "hhid"] <- "P165"
hh_data[hh_data$uuid == "7c7519cc-d15b-4ec7-8a05-2341558f0f41", "hhid"] <- "P166"
hh_data[hh_data$uuid == "db2c08e2-ed11-45b6-aa41-24fd6b0589ed", "hhid"] <- "P167"
hh_data[hh_data$uuid == "ced29e60-0592-46f3-9f35-f451ce2a2589", "hhid"] <- "P168"
hh_data[hh_data$uuid == "505c9e04-f6f8-4c37-a7d4-8f3635304357", "hhid"] <- "P169"
hh_data[hh_data$uuid == "f7d637f0-13b7-4b3c-9f71-ab8ab71a4f1a", "hhid"] <- "P170"
hh_data[hh_data$uuid == "022dd83a-f45a-4cf5-a8c3-b27e9ae39af9", "hhid"] <- "D336"
hh_data[hh_data$uuid == "bc53e56b-cedf-4484-a953-437882d4d57b", "hhid"] <- "D337"
hh_data[hh_data$uuid == "0717bcb0-969e-41dd-a1ab-dca1ff2dcedc", "hhid"] <- "D338"
hh_data[hh_data$uuid == "7556d724-b742-4ec5-99a7-95a622c27844", "hhid"] <- "D339"
hh_data[hh_data$uuid == "9d0ec7d1-78ab-4d90-b9a0-282994c6a184", "hhid"] <- "D023"
hh_data[hh_data$uuid == "99967ee2-714e-476e-bc50-0a02bb0c4807", "hhid"] <- "D160"
hh_data[hh_data$uuid == "70cd449c-4fc2-49fd-97df-29a4e52def8d", "hhid"] <- "D040"
hh_data[hh_data$uuid == "2742ca6f-3083-4ea0-92d0-df45ef535d71", "hhid"] <- "D013"
hh_data[hh_data$uuid == "817429cf-4380-416c-ba6c-58b37a9e804e", "hhid"] <- "D032"
hh_data[hh_data$uuid == "bd404ccc-4c4e-449f-acd7-9483a19d3391", "hhid"] <- "D024"
hh_data[hh_data$uuid == "9e16118f-7f2d-47a6-911f-b5b0410008da", "hhid"] <- "D018"
hh_data[hh_data$uuid == "c2c8f620-1189-4db0-bd5f-8ef56fcdb822", "hhid"] <- "D019"
hh_data[hh_data$uuid == "9c54daa4-f493-4521-82f6-aa72ef9d894f", "hhid"] <- "D025"
hh_data[hh_data$uuid == "5ed609e6-7495-4b4c-b175-b84284193d76", "hhid"] <- "D020"
hh_data[hh_data$uuid == "6a8c3dc8-1eb9-4d19-9929-90d81ca54f51", "hhid"] <- "D030"
hh_data[hh_data$uuid == "04c85796-4cd0-471c-8491-50f8c1ede24a", "hhid"] <- "D021"
hh_data[hh_data$uuid == "c63d952d-070d-4587-a1ba-652483bf140f", "hhid"] <- "D022"
hh_data[hh_data$uuid == "c26118fb-2210-4e04-8e32-cc7df76647fb", "hhid"] <- "D056"
hh_data[hh_data$uuid == "72c4f4e1-5cea-4008-8c65-e14e8f58542f", "hhid"] <- "D060"
hh_data[hh_data$uuid == "1e91f9f7-7a33-4abe-9997-c66dc81468cd", "hhid"] <- "D057"
hh_data[hh_data$uuid == "d4f68125-5917-485d-a910-f726c44903ae", "hhid"] <- "D071"
hh_data[hh_data$uuid == "8fa40203-4a59-43f8-b410-c3a03c0aa572", "hhid"] <- "D072"
hh_data[hh_data$uuid == "245cf407-b35b-4107-b790-736bbe60aba9", "hhid"] <- "D076"
hh_data[hh_data$uuid == "6c7f8d08-e8cf-456e-b9b2-e74c0c2b1ac8", "hhid"] <- "D341"
hh_data[hh_data$uuid == "02cf0d16-888e-42f9-8a01-8f5b6a5042b0", "hhid"] <- "P166"
hh_data[hh_data$uuid == "25b0e137-ba88-4880-8e73-e802e50d0da0", "hhid"] <- "D348"
hh_data[hh_data$uuid == "0ee65a56-fc66-422c-a421-da96cea8dbaa", "hhid"] <- "D349"
hh_data[hh_data$uuid == "d7054fbd-accd-4d2c-b466-7f540975bc63", "hhid"] <- "D342"
hh_data[hh_data$uuid == "5a91256f-aa62-4028-ad8c-b34ea9fee3f3", "hhid"] <- "D350"
hh_data[hh_data$uuid == "cfdec673-2da7-4958-94f9-00294c3ff2f8", "hhid"] <- "D351"
hh_data[hh_data$uuid == "7b5223c4-c391-450f-aedc-07ba93908488", "hhid"] <- "D352"
hh_data[hh_data$uuid == "b9ddb4d7-77b7-4afa-9e95-a7a1c9d4844c", "hhid"] <- "D353"
hh_data[hh_data$uuid == "31503efc-9c7a-4571-bdfc-41bcd92890e3", "hhid"] <- "D354"
hh_data[hh_data$uuid == "83b6d95a-69fd-4834-b5d5-7644900e65e2", "hhid"] <- "D357"
hh_data[hh_data$uuid == "17b502a4-b151-4605-9a64-bf28c7fa89ed", "hhid"] <- "D355"
hh_data[hh_data$uuid == "c8f43405-aac9-4350-988a-f757a5e00b82", "hhid"] <- "D356"
hh_data[hh_data$uuid == "022a6700-fd55-4272-ab9c-0ba341dc6291", "hhid"] <- "D128"
hh_data[hh_data$uuid == "2d7f9674-bdf2-4bd5-a0a5-7c3d7f86670f", "hhid"] <- "D002"


# Responsible Surveyor Assignment ####
dworzark_lookup <- hh_data %>%
  filter(community == "Dworzark", !is.na(hhid), surveyor_name %in% c("Jamiatu", "Boboieh")) %>%
  arrange(hhid, desc(survey_date)) %>%
  group_by(hhid) %>%
  slice(1) %>%
  ungroup() %>%
  select(hhid, dworzark_resp = surveyor_name) %>%
  mutate(dworzark_resp = as.character(dworzark_resp))

hh_data <- hh_data %>%
  left_join(dworzark_lookup, by = "hhid") %>%
  mutate(resp_surveyor = case_when(
    community == "Cockle Bay" ~ "Musa",
    community == "Portee Rokupa" ~ "Abdulai K",
    community == "Dworzark" & !is.na(dworzark_resp) ~ dworzark_resp,
    TRUE ~ "Unknown"
  )) %>%
  select(-dworzark_resp) %>%
  relocate(resp_surveyor, .after = surveyor_name)

hh_data[which(hh_data$hhid == "D334"), "resp_surveyor"] <- "Boboieh"
hh_data[which(hh_data$hhid == "D342"), "resp_surveyor"] <- "Boboieh"
hh_data[which(hh_data$hhid == "D355"), "resp_surveyor"] <- "Boboieh"
# Individual Field Corrections ####

hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "adult_men"] <- 5
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "adult_women"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "boys"] <- 5
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "girls"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "boys_late_school"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "girls_late_school"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "daily_helpers"] <- 5
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "jerrycans_men"] <- 5
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "jerrycans_women"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "jerrycans_boys"] <- 5
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "jerrycans_girls"] <- 0
hh_data[hh_data$uuid == "a77617e9-6169-430a-95f7-b6bfbfeefdee", "jerrycans_hired"] <- 0

hh_data[hh_data$uuid == "ab13eb06-4086-4e35-9819-4c095a431e61", "boys_attend_school"] <- 1
hh_data[hh_data$uuid == "2f921170-e5d5-4b3a-a025-f034db0493eb", "boys_attend_school"] <- 1
hh_data[hh_data$uuid == "5ca7c035-918e-48e9-acbe-6bbc2cba9a2b", "boys_attend_school"] <- 1
hh_data[hh_data$uuid == "1f1d4014-4bc4-4c1d-b033-1cde0081ed2f", "boys_attend_school"] <- 1
hh_data[hh_data$uuid == "11c2ccc6-9066-42a7-9efd-52c0898bc2c0", "boys_late_school"] <- 1
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "daily_helpers"] <- 1

hh_data[hh_data$uuid == "ef6fb9ad-1fea-4462-bea7-95b0cff9a706", "domestic_potable"] <- "No"
hh_data[hh_data$uuid == "9addc381-17c8-46c3-99cb-0fa92e2a41f8", "domestic_potable"] <- "No"
hh_data[hh_data$uuid == "131ae7f4-ca31-4cea-ac11-c8c4d238d754", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "6afa92bf-739f-499c-9ef5-879f1ba4d1e3", "domestic_potable"] <- "No"
hh_data[hh_data$uuid == "04699513-2a3e-4226-a350-405aa20615e0", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "ce7c94b2-c8d4-4cb6-ad93-c48bdf83f289", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "4b94ef88-5dec-41bd-bd73-10785c5b8ddd", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "1cc523fa-006a-43c3-93c7-849cd70e9b76", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "21e42fec-9123-443c-b170-fb6343aac8ff", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "eefa851b-4b6b-4c83-9b0d-1ecbdecf61b1", "domestic_potable"] <- "No"
hh_data[hh_data$uuid == "00a47e19-1d49-4a56-bc60-7108a359e8fb", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "0b05f9bb-0b17-4422-a774-2cedf1508bde", "domestic_potable"] <- "Yes"
hh_data[hh_data$uuid == "bf1759ab-67b2-41e6-808b-7a01d690d587", "domestic_potable"] <- "Yes"

hh_data[hh_data$uuid == "9273eecb-9242-4bae-8fc1-32131b7469fd", "domestic_source_notes"] <- "It's an illegal cut pipe. Only access in the rainy season"

hh_data[hh_data$uuid == "4f46ca4d-2cad-423c-9b36-a365f8fc79bd", "domestic_source_selection"] <- "89aaf60b-dc5b-497c-912d-028b27a8fc6f"
hh_data[hh_data$uuid == "f142d150-0b0e-4a01-af08-b4c03526da14", "domestic_source_selection"] <- "5b0b68e9-e2c1-4ca0-81db-e9af63f4ca81"
hh_data[hh_data$uuid == "1c67a3f5-84a5-423b-9250-e85e44bed5f9", "domestic_source_selection"] <- "Own"
hh_data[hh_data$uuid == "77c08b76-153c-408e-a56d-64d66d8d5f4e", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "344dc685-8463-4f16-9a16-991132c68202", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "53edf992-28fd-4acd-8d60-c99389c1da19", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "ddabb3c7-7743-4de2-a6c4-be98540a18b0", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "f430af01-59f6-4a30-936d-b835747325a8", "domestic_source_selection"] <- "59985923-b935-4d98-8632-1e684e11e25c"
hh_data[hh_data$uuid == "579ac23f-4b14-4374-9421-d82ab033ba44", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "522943f8-35d8-4c5a-ac59-6bacadf5f639", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "372163d4-796f-4d08-9755-f640b6dc9646", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "1698f65d-ab42-4cbc-b525-0d39882e9e6f", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "41e0c2d8-754e-4482-b2b6-957cb5a2e70c", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "a3135d7e-88ff-44dc-a4e5-08d64faa8611", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "204d1ece-9fb8-47ea-853c-1d8da9996224", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "a38e58cf-6b97-44ae-bf2d-6682463d2345", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "0646a356-b502-4569-8e8b-5d6f4b55cf06", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "aa49fd4b-7c3a-45fe-b450-b9fb46771515", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "7daffbfa-be98-405d-b59e-da17ab96e1b4", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "995b3ef8-caef-4e4a-be47-1b57dd27d007", "domestic_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "683f35c4-132e-4fcc-9473-2c7314c31bdb", "domestic_source_selection"] <- "Own Source"
hh_data[hh_data$uuid == "d0188224-a0b5-4ddd-91f1-84f0aa011cdd", "domestic_source_selection"] <- "e022c950-2877-4ae3-af8a-6470752e1ecd"
hh_data[hh_data$uuid == "18a4c3e9-3ecc-42de-ba00-3e88079530e7", "domestic_source_selection"] <- "797c386b-4147-4084-a5ba-82cc01a0102e"

hh_data[hh_data$uuid == "21925d17-7dd2-40c8-ac42-28dda80be86e", "domestic_source_zone"] <- "Outside"
hh_data[hh_data$uuid == "2cfa5225-a766-4e56-b651-9d6f9eda52dc", "domestic_source_zone"] <- "Nigeria"

hh_data[hh_data$uuid == "061bd737-c256-4fe3-a6b3-acd1dc0b1ce2", "domestic_trips"] <- 7
hh_data[hh_data$uuid == "2cfa5225-a766-4e56-b651-9d6f9eda52dc", "domestic_trips"] <- 7
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "domestic_trips"] <- 4
hh_data[hh_data$uuid == "4c03b1c9-1feb-4ec4-a5c2-39553a526d10", "domestic_trips"] <- 3
hh_data[hh_data$uuid == "5fbfea3b-e19b-442c-bc8b-9204b213d06a", "domestic_trips"] <- 7
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "domestic_trips"] <- 15

hh_data[hh_data$uuid == "029226f2-4892-448b-9be7-5ae500d7181e", "domestic_wait_time"] <- 0
hh_data[hh_data$uuid == "5106cec6-d3ff-4f5f-ad71-650306f8659c", "domestic_wait_time"] <- 10
hh_data[hh_data$uuid == "061bd737-c256-4fe3-a6b3-acd1dc0b1ce2", "domestic_wait_time"] <- 90
hh_data[hh_data$uuid == "2cfa5225-a766-4e56-b651-9d6f9eda52dc", "domestic_wait_time"] <- 90
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "domestic_wait_time"] <- 60
hh_data[hh_data$uuid == "4c03b1c9-1feb-4ec4-a5c2-39553a526d10", "domestic_wait_time"] <- 35
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "domestic_wait_time"] <- 15

hh_data[hh_data$uuid == "029226f2-4892-448b-9be7-5ae500d7181e", "domestic_walk_time"] <- 2
hh_data[hh_data$uuid == "5106cec6-d3ff-4f5f-ad71-650306f8659c", "domestic_walk_time"] <- 1
hh_data[hh_data$uuid == "061bd737-c256-4fe3-a6b3-acd1dc0b1ce2", "domestic_walk_time"] <- 30
hh_data[hh_data$uuid == "2cfa5225-a766-4e56-b651-9d6f9eda52dc", "domestic_walk_time"] <- 30
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "domestic_walk_time"] <- 25
hh_data[hh_data$uuid == "4c03b1c9-1feb-4ec4-a5c2-39553a526d10", "domestic_walk_time"] <- 20
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "domestic_walk_time"] <- 7

hh_data[hh_data$uuid == "98d473c0-7fe9-4e8b-a0cb-9e7066f7596b", "domestic_water_daily_spend"] <- 15
hh_data[hh_data$uuid == "32c671c4-000c-499a-b3fe-a277229d30a1", "domestic_water_daily_spend"] <- 50

hh_data[hh_data$uuid == "01fe9ec0-31b3-42f0-985f-9e92a44ebed5", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "eebea2c9-c357-4d70-be71-59d6b05f681f", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "131ae7f4-ca31-4cea-ac11-c8c4d238d754", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "03c5feca-27f3-4991-99e2-b4a794235015", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "04699513-2a3e-4226-a350-405aa20615e0", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "167b1892-cb30-4b58-a6ed-a6133434d87b", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "53d28a6c-024b-4964-b434-d4c969240856", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "63672f67-cc79-444c-9eac-c7b71fbcc1ab", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "ae572aea-00a6-47f8-9e6f-0569963ec02a", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "1cc523fa-006a-43c3-93c7-849cd70e9b76", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "21e42fec-9123-443c-b170-fb6343aac8ff", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "eefa851b-4b6b-4c83-9b0d-1ecbdecf61b1", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "00a47e19-1d49-4a56-bc60-7108a359e8fb", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "83d4d552-d91e-47b9-8a54-5cbd818ccbd5", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "9066b65c-f20f-4371-a02c-dfe1157c8d66", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "0b05f9bb-0b17-4422-a774-2cedf1508bde", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "bf1759ab-67b2-41e6-808b-7a01d690d587", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "c6e4386d-9345-4e7b-bf19-4f94f91fd441", "drinking_potable"] <- "Yes"
hh_data[hh_data$uuid == "d18fd4a5-f59d-4634-ae6c-0422ec0c1757", "drinking_potable"] <- "Yes"

hh_data[hh_data$uuid == "7b6d0e9d-3159-4480-9251-b4f2b42ff58d", "drinking_source_selection"] <- "3450f005-e6bd-4e1e-9be0-543769c4b475"
hh_data[hh_data$uuid == "77c08b76-153c-408e-a56d-64d66d8d5f4e", "drinking_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "f430af01-59f6-4a30-936d-b835747325a8", "drinking_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "522943f8-35d8-4c5a-ac59-6bacadf5f639", "drinking_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "995b3ef8-caef-4e4a-be47-1b57dd27d007", "drinking_source_selection"] <- "d5005ed9-e864-4ef1-a0d2-32c095ce0e37"
hh_data[hh_data$uuid == "9917f358-248e-43bf-9cd0-bdbdb7be5b3b", "drinking_source_selection"] <- "24a22646-9113-4438-ae4f-1ed984eddfc6"
hh_data[hh_data$uuid == "5e01b6a3-3292-4c3f-9ac6-65bec14a86a0", "drinking_source_selection"] <- "24a22646-9113-4438-ae4f-1ed984eddfc6"
hh_data[hh_data$uuid == "4c926fbd-88b7-4317-885b-e2c9fff4fa59", "drinking_source_selection"] <- "3268ecc8-791b-49d5-b512-9d3fa43224b5"
hh_data[hh_data$uuid == "683f35c4-132e-4fcc-9473-2c7314c31bdb", "drinking_source_selection"] <- "Own Source"
hh_data[hh_data$uuid == "fcbac837-810f-4458-afe5-856ad221ba60", "drinking_source_selection"] <- "d949566d-4238-4650-8d58-32fe58f088c2"
hh_data[hh_data$uuid == "d0188224-a0b5-4ddd-91f1-84f0aa011cdd", "drinking_source_selection"] <- "e022c950-2877-4ae3-af8a-6470752e1ecd"
hh_data[hh_data$uuid == "ca00e9e0-b85f-4f86-9e3c-e9a5e2154a3b", "drinking_source_selection"] <- "e022c950-2877-4ae3-af8a-6470752e1ecd"

hh_data[hh_data$uuid == "53edf992-28fd-4acd-8d60-c99389c1da19", "drinking_source_zone"] <- "Argentina"
hh_data[hh_data$uuid == "ddabb3c7-7743-4de2-a6c4-be98540a18b0", "drinking_source_zone"] <- "Outside of the Community"
hh_data[hh_data$uuid == "05961c13-7d90-4040-a020-0e5a4333422d", "drinking_source_zone"] <- "Outside of the Community"
hh_data[hh_data$uuid == "21925d17-7dd2-40c8-ac42-28dda80be86e", "drinking_source_zone"] <- "Outside of the Community"

hh_data[hh_data$uuid == "3eb4df93-80a4-46da-98fe-c2139f602f12", "drinking_trips"] <- 15
hh_data[hh_data$uuid == "bd8de69d-7a7e-42d9-979d-a652186c6a18", "drinking_wait_time"] <- 0
hh_data[hh_data$uuid == "3eb4df93-80a4-46da-98fe-c2139f602f12", "drinking_wait_time"] <- 0
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "drinking_wait_time"] <- 5
hh_data[hh_data$uuid == "3eb4df93-80a4-46da-98fe-c2139f602f12", "drinking_walk_time"] <- 15
hh_data[hh_data$uuid == "83d4d552-d91e-47b9-8a54-5cbd818ccbd5", "drinking_walk_time"] <- 0
hh_data[hh_data$uuid == "4327f4a6-0e24-4954-87d2-56b6b8535c6b", "drinking_walk_time"] <- 2

hh_data[hh_data$uuid == "98d473c0-7fe9-4e8b-a0cb-9e7066f7596b", "drinking_water_daily_spend"] <- 20
hh_data[hh_data$uuid == "8af210bb-e2a3-4f0b-8a18-353a1702c891", "drinking_water_daily_spend"] <- 20
hh_data[hh_data$uuid == "32c671c4-000c-499a-b3fe-a277229d30a1", "drinking_water_daily_spend"] <- 9

hh_data[hh_data$uuid == "dc0ed15e-13cf-4bc2-990a-532b30906a04", "girls_attend_school"] <- 3
hh_data[hh_data$uuid == "3432940a-be4a-40c8-b9af-620c6e80d317", "girls_attend_school"] <- 0
hh_data[hh_data$uuid == "e62ca187-bda9-49fe-bd74-b979514d0e17", "girls_attend_school"] <- 2

hh_data[hh_data$uuid == "dc0ed15e-13cf-4bc2-990a-532b30906a04", "girls_late_school"] <- 0
hh_data[hh_data$uuid == "c3243c02-6e64-4340-818a-e5feff51c09b", "girls_late_school"] <- 0
hh_data[hh_data$uuid == "11c2ccc6-9066-42a7-9efd-52c0898bc2c0", "girls_late_school"] <- 0
hh_data[hh_data$uuid == "3432940a-be4a-40c8-b9af-620c6e80d317", "girls_late_school"] <- 0

hh_data[hh_data$uuid == "d823678e-a9cf-48b5-b658-e0f10a8da4bf", "hh_gender"] <- "Female"
hh_data[hh_data$uuid == "914f6a10-44b2-480c-8e35-e90b7227ccf8", "hh_gender"] <- "Female"

hh_data[hh_data$uuid == "b49c22a0-0a02-4608-a31a-ed7ee63c0d0f", "injuries_water"] <- 0
hh_data[hh_data$uuid == "7aad59bc-6473-4a37-af9a-e692ab20bfbd", "injuries_water"] <- 0
hh_data[hh_data$uuid == "04699513-2a3e-4226-a350-405aa20615e0", "injuries_water"] <- 2
hh_data[hh_data$uuid == "e52bc04f-3ed1-49ad-8bf2-fd32f9762ce2", "injuries_water"] <- 0
hh_data[hh_data$uuid == "7cf64417-13c5-42b1-818e-91f9b381a9c8", "injuries_water"] <- 0

hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "jerrycans_boys"] <- 0
hh_data[hh_data$uuid == "e23c0607-db63-44ae-beb9-819b7b709979", "jerrycans_boys"] <- 0
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "jerrycans_girls"] <- 0
hh_data[hh_data$uuid == "e23c0607-db63-44ae-beb9-819b7b709979", "jerrycans_girls"] <- 0
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "jerrycans_hired"] <- 25
hh_data[hh_data$uuid == "e23c0607-db63-44ae-beb9-819b7b709979", "jerrycans_hired"] <- 5
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "jerrycans_men"] <- 0
hh_data[hh_data$uuid == "381f7f3e-b3a5-4a51-a8b1-319664ad5954", "jerrycans_women"] <- 0
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "jerrycans_women"] <- 0

hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "men_danger_environment"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "men_danger_people"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "women_danger_environment"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "women_danger_people"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "women_bullying_others"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "women_bullying_owner"] <- "Never (0 times)"
hh_data[hh_data$uuid == "dc1bbb7a-a2a3-424c-ad52-5f0b6b955bbc", "sachet_frequency"] <- "Always (More than 20 times)"

hh_data[hh_data$uuid == "80e485e7-887d-42fb-bde5-0eb7bc49136b", "number"] <- "0"
hh_data[hh_data$uuid == "82b7e777-4c0c-4d59-ad62-d9ad5be49365", "number"] <- "0"
hh_data[hh_data$uuid == "1cc523fa-006a-43c3-93c7-849cd70e9b76", "number"] <- "0"
hh_data[hh_data$uuid == "f7efb843-4ec5-4248-af66-97a33e91a1d9", "number"] <- "0"
hh_data[hh_data$uuid == "00a47e19-1d49-4a56-bc60-7108a359e8fb", "number"] <- "0"
hh_data[hh_data$uuid == "37856d33-d39c-480a-8c39-9e906726a389", "number"] <- "Musa knows how to contact"
hh_data[hh_data$uuid == "5106cec6-d3ff-4f5f-ad71-650306f8659c", "number"] <- "Musa knows how to contact"
hh_data[hh_data$uuid == "66a5c709-81b1-4b33-99f0-ab26dab8780b", "number"] <- "Musa knows how to contact"
hh_data[hh_data$uuid == "f4eb3120-6c72-4687-81dc-a42794299365", "number"] <- "Musa knows how to contact"
hh_data[hh_data$uuid == "9066b65c-f20f-4371-a02c-dfe1157c8d66", "number"] <- "Musa Knows"
hh_data[hh_data$uuid == "d916419c-4b24-4d93-8a99-45e674ee1b1c", "number"] <- "Musa Knows"
hh_data[hh_data$uuid == "fd911b4c-a105-4028-843c-959c39f0bf19", "number"] <- "Moses knows"
hh_data[hh_data$uuid == "5f4f952c-ebda-4b8e-8272-4a7d88627947", "number"] <- "Moses knows"
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "number"] <- "Moses knows"
hh_data[hh_data$uuid == "8e7f6e0b-a072-46b6-9a01-3df75654347f", "number"] <- "Moses can contact her"
hh_data[hh_data$uuid == "bfd7f6df-29f4-42ac-8307-736514886309", "number"] <- "Moses can contact her"
hh_data[hh_data$uuid == "03beb62c-efd4-473a-92b6-3b310b4ea4d3", "number"] <- "Moses can contact him"
hh_data[hh_data$uuid == "531d2600-2e7c-4ff1-b89d-2fff276977a4", "number"] <- "Moses can contact her"
hh_data[hh_data$uuid == "73482313-d8a2-444b-8569-974c05842a71", "number"] <- "Joseph can contact her"

hh_data[hh_data$uuid == "98d473c0-7fe9-4e8b-a0cb-9e7066f7596b", "own_water_source"] <- "No"
hh_data[hh_data$uuid == "ddabb3c7-7743-4de2-a6c4-be98540a18b0", "primary_drinking_source"] <- "Sachet"
hh_data[hh_data$uuid == "d287f897-d6c9-4c1f-a186-9491237736ef", "respondent_gender"] <- "Male"

hh_data[hh_data$uuid == "ab13eb06-4086-4e35-9819-4c095a431e61", "school_age_boys"] <- 1
hh_data[hh_data$uuid == "647d4e83-da8b-4063-8b6f-ef744ef7ad40", "school_age_boys"] <- 0
hh_data[hh_data$uuid == "647d4e83-da8b-4063-8b6f-ef744ef7ad40", "school_age_girls"] <- 0

hh_data[hh_data$uuid == "7cf64417-13c5-42b1-818e-91f9b381a9c8", "sick_water"] <- 5
hh_data[hh_data$uuid == "0086a58b-a395-4ee9-a87d-284f219979dc", "sick_water"] <- 0
hh_data[hh_data$uuid == "029226f2-4892-448b-9be7-5ae500d7181e", "sick_water"] <- 0
hh_data[hh_data$uuid == "bb0d8777-d499-40e0-81ac-468eb59ddcc0", "sick_water"] <- 0
hh_data[hh_data$uuid == "e4d67faa-5f50-46be-b97c-3cba5ba8343b", "sick_water"] <- 0

hh_data[hh_data$uuid == "b49c22a0-0a02-4608-a31a-ed7ee63c0d0f", "standing_water"] <- "Yes"
hh_data[hh_data$uuid == "e0504595-f4f5-46af-8a14-83bc96d54ce3", "stored_amount"] <- 6

hh_data[hh_data$uuid == "8ee7c171-4538-4510-99b2-6de849b3ffd9", "training_received"] <- "Yes"
hh_data[hh_data$uuid == "eefa851b-4b6b-4c83-9b0d-1ecbdecf61b1", "training_received"] <- "No"
hh_data[hh_data$uuid == "ae4ee2a4-cc22-4522-8bd1-0099469e2481", "training_received"] <- "No"
hh_data[hh_data$uuid == "8e7f6e0b-a072-46b6-9a01-3df75654347f", "training_received"] <- "No"
hh_data[hh_data$uuid == "03beb62c-efd4-473a-92b6-3b310b4ea4d3", "training_received"] <- "No"
hh_data[hh_data$uuid == "98d473c0-7fe9-4e8b-a0cb-9e7066f7596b", "training_received"] <- "No"

# Structural Corrections ####

hh_data <- hh_data %>%
  mutate(hh_size = adult_men + adult_women + boys + girls) %>%
  relocate(hh_size, .before = adult_men) %>%
  relocate(gps_location, .before = any_of("comments")) %>%
  relocate(uuid, hhid, community, zone, survey_month, surveyor_name, resp_surveyor, number) %>%
  relocate(boys, girls, school_age_boys, school_age_girls, boys_attend_school, girls_attend_school, boys_late_school, girls_late_school, .after = adult_women) %>%
  relocate(any_of(c("source_meets_needs", "share_source")), .after = any_of("own_water_source")) %>%
  relocate(any_of("domestic_source_notes"), .after = any_of("domestic_source_selection")) %>%
  relocate(any_of(c("treatment_chlorine", "treatment_simplefilter", "treatment_advancefilter",
                    "treatment_boil", "treatment_settle", "treatment_other")), .after = any_of("treatment_frequency")) %>%
  select(-any_of(c("survey_zone", "primary_domestic_source", "surveyor_other", "primary_drinking_source", "surveyor")))

# Zones ####
most_recent_zone <- hh_data %>%
  filter(!is.na(hhid), !is.na(zone)) %>%
  arrange(hhid, desc(survey_date)) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid, zone_recent = zone)

hh_data <- hh_data %>%
  left_join(most_recent_zone, by = "hhid") %>%
  mutate(zone = if_else(!is.na(zone_recent), zone_recent, zone)) %>%
  select(-zone_recent)

# Source selections ####
dec_values <- hh_data %>%
  filter(survey_month == "Dec") %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid,
         drinking_source_type_dec = drinking_source_type,
         drinking_source_selection_dec = drinking_source_selection,
         domestic_source_type_dec = domestic_source_type,
         domestic_source_selection_dec = domestic_source_selection)

hh_data <- hh_data %>%
  left_join(dec_values, by = "hhid") %>%
  mutate(
    drinking_copied = survey_month == "Nov" &
      is.na(drinking_source_selection) &
      drinking_source_type != "Sachet" &
      drinking_source_type == drinking_source_type_dec &
      !is.na(drinking_source_selection_dec),
    domestic_copied = survey_month == "Nov" &
      is.na(domestic_source_selection) &
      domestic_source_type != "Sachet" &
      domestic_source_type == domestic_source_type_dec &
      !is.na(domestic_source_selection_dec),
    drinking_source_selection = if_else(drinking_copied, drinking_source_selection_dec, drinking_source_selection),
    domestic_source_selection = if_else(domestic_copied, domestic_source_selection_dec, domestic_source_selection)
  ) %>%
  select(-drinking_source_type_dec, -drinking_source_selection_dec,
         -domestic_source_type_dec, -domestic_source_selection_dec,
         -drinking_copied, -domestic_copied)

# Moses November HH size correction ####
december_vals <- hh_data %>%
  filter(survey_month == "Dec", !is.na(hhid), str_detect(hhid, "^[CPD]\\d{3}$")) %>%
  mutate(hh_total_dec = adult_men + adult_women + boys + girls) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid,
         adult_men_dec = adult_men, adult_women_dec = adult_women,
         boys_dec = boys, girls_dec = girls,
         school_age_boys_dec = school_age_boys, school_age_girls_dec = school_age_girls,
         boys_attend_school_dec = boys_attend_school, girls_attend_school_dec = girls_attend_school,
         hh_total_dec)

january_vals <- hh_data %>%
  filter(survey_month == "Jan", !is.na(hhid), str_detect(hhid, "^[CPD]\\d{3}$")) %>%
  mutate(hh_total_jan = adult_men + adult_women + boys + girls) %>%
  distinct(hhid, .keep_all = TRUE) %>%
  select(hhid,
         adult_men_jan = adult_men, adult_women_jan = adult_women,
         boys_jan = boys, girls_jan = girls,
         school_age_boys_jan = school_age_boys, school_age_girls_jan = school_age_girls,
         boys_attend_school_jan = boys_attend_school, girls_attend_school_jan = girls_attend_school,
         hh_total_jan)

hh_data <- hh_data %>%
  mutate(hh_total = adult_men + adult_women + boys + girls) %>%
  left_join(december_vals, by = "hhid") %>%
  left_join(january_vals, by = "hhid") %>%
  mutate(
    hh_total_ref = coalesce(hh_total_dec, hh_total_jan),
    adult_men_ref = coalesce(adult_men_dec, adult_men_jan),
    adult_women_ref = coalesce(adult_women_dec, adult_women_jan),
    boys_ref = coalesce(boys_dec, boys_jan),
    girls_ref = coalesce(girls_dec, girls_jan),
    school_age_boys_ref = coalesce(school_age_boys_dec, school_age_boys_jan),
    school_age_girls_ref = coalesce(school_age_girls_dec, school_age_girls_jan),
    boys_attend_school_ref = coalesce(boys_attend_school_dec, boys_attend_school_jan),
    girls_attend_school_ref = coalesce(girls_attend_school_dec, girls_attend_school_jan),
    replace_flag = survey_month == "Nov" &
      surveyor_name == "Moses" &
      !is.na(hh_total_ref) &
      hh_total > hh_total_ref + 5,
    replace_flag = replace_na(replace_flag, FALSE),
    adult_men = if_else(replace_flag, adult_men_ref, adult_men),
    adult_women = if_else(replace_flag, adult_women_ref, adult_women),
    boys = if_else(replace_flag, boys_ref, boys),
    girls = if_else(replace_flag, girls_ref, girls),
    school_age_boys = if_else(replace_flag, school_age_boys_ref, school_age_boys),
    school_age_girls = if_else(replace_flag, school_age_girls_ref, school_age_girls),
    boys_attend_school = if_else(replace_flag, boys_attend_school_ref, boys_attend_school),
    girls_attend_school = if_else(replace_flag, girls_attend_school_ref, girls_attend_school)
  ) %>%
  select(-ends_with("_dec"), -ends_with("_jan"), -ends_with("_ref"), -replace_flag, -hh_total)

# GPS Location Triangulation ####

cockle_shapefile <- st_read("shapefiles/CockleBay.shp", quiet = TRUE) %>% st_transform(4326)
dworzark_shapefile <- st_read("shapefiles/Dworzark.shp", quiet = TRUE) %>% st_transform(4326)
portee_shapefile <- st_read("shapefiles/PorteeRokupa.shp", quiet = TRUE) %>% st_transform(4326)

cockle_buffer <- cockle_shapefile %>% st_transform(32628) %>% st_buffer(100) %>% st_transform(4326)
dworzark_buffer <- dworzark_shapefile %>% st_transform(32628) %>% st_buffer(100) %>% st_transform(4326)
portee_buffer <- portee_shapefile %>% st_transform(32628) %>% st_buffer(100) %>% st_transform(4326)

hh_with_coords <- hh_data %>%
  filter(!is.na(gps_location)) %>%
  separate(gps_location, into = c("lat", "lon", "elevation", "accuracy"),
           sep = " ", remove = FALSE) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

hh_sf <- st_as_sf(hh_with_coords, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

hh_sf <- hh_sf %>%
  mutate(
    within_boundary = case_when(
      community == "Cockle Bay" ~ st_within(geometry, cockle_buffer, sparse = FALSE)[,1],
      community == "Dworzark" ~ st_within(geometry, dworzark_buffer, sparse = FALSE)[,1],
      community == "Portee Rokupa" ~ st_within(geometry, portee_buffer, sparse = FALSE)[,1],
      TRUE ~ FALSE
    ),
    lat = ifelse(within_boundary, lat, NA),
    lon = ifelse(within_boundary, lon, NA)
  )

mean_coords <- hh_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(hhid)) %>%
  group_by(hhid, community) %>%
  summarize(
    mean_lat = mean(lat, na.rm = TRUE),
    mean_lon = mean(lon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_lat = ifelse(is.nan(mean_lat), NA, mean_lat),
    mean_lon = ifelse(is.nan(mean_lon), NA, mean_lon)
  )

hh_data <- hh_data %>%
  left_join(mean_coords %>% select(hhid, mean_lat, mean_lon), by = "hhid") %>%
  mutate(
    gps_location = ifelse(!is.na(mean_lat) & !is.na(mean_lon),
                          paste(mean_lat, mean_lon, "0 0"),
                          gps_location)
  ) %>%
  select(-mean_lat, -mean_lon)