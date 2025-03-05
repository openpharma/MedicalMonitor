### Script to reverse-engineer raw data from the current clinsightful_data object
devtools::load_all()
library(dplyr)
library(tidyr)

clinsight_names <- metadata$column_names

varnames <- metadata$items_expanded |> 
  distinct(var, item_name) |> 
  filter(
    n() == 1 | grepl("LBORRES$|VSORRES$", var), 
    .by = item_name
  ) 

# Variables not in clinsightful_data but are mentioned in metadata.xlsx:
# dplyr::anti_join(varnames, clinsightful_data, by = "item_name")
# var            item_name              
# 1 AE_AESER_AEOUT SAE outcome            
# 2 WHO_WHOCAT     WHO.subclassification  
# 3 DMOD_DAT       DoseModificationDate   
# 4 DMOD_REAS      DoseModificationReason 
# 5 DMOD_DOSE      DoseModificationNewDose

# All items in clinsightful_data are also in metadata, as expected:
# dplyr::anti_join(clinsightful_data, varnames, by = "item_name")
# # A tibble: 0 × 24

cd_new <- dplyr::left_join(clinsightful_data, varnames, by = "item_name")

labvars <- c(
  "LBORRES" = "item_value", 
  "LBORNR_Lower" = "lower_lim", 
  "LBORNR_Upper" = "upper_lim", 
  "LBORRESU" = "item_unit", 
  "LBCLSIG"  = "significance", 
  "LBREASND" = "reason_notdone"
)

lab_data <- cd_new |> 
  filter(grepl("_LBORRES$|VSORRES", var)) |> 
  rename(all_of(labvars)) |>
  mutate(
    var = gsub("_VSORRES$|_LBORRES$", "", var)
  ) |> 
  mutate(across(all_of(names(labvars)), as.character)) |> 
  pivot_longer(
    all_of(names(labvars)),
    names_to = "suffix", values_to = "item_value"
    ) |> 
  filter(
    # since other vars in vital signs do not exist in the data:
    !(item_group == "Vital signs" & !suffix %in% c("LBORRES", "LBREASND")),
    # remove derived vars:
    !var == "VS_WEIGHTCHANGE" 
    ) |> 
  mutate(
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBORRES", "VSORRES", suffix),
    suffix = ifelse(item_group == "Vital signs" & suffix == "LBREASND", "VSREASND", suffix),
    var = ifelse(is.na(suffix), var, paste0(var, "_", suffix))
  ) 

other_data <- cd_new |> 
  filter(!grepl("_LBORRES$|VSORRES", var)) |> 
  # keep item_value of course, but remove other lab values:
  select(-all_of(labvars[-1]))

all_data <- dplyr::bind_rows(other_data, lab_data) |> 
  # remove columns that are created during metadata merging:
  select(-region, -suffix, -db_update_time, -day, -vis_day, 
         -vis_num, -item_type, -item_group, -region) |> 
  mutate(
    item_value = ifelse(item_value == "(unit missing)", NA_character_, item_value),
  ) |> 
  # revert names
  rename(
    c(
      all_of(setNames(clinsight_names$name_new, clinsight_names$name_raw)),
      "ItemName" = "item_name",
      "EventName" = "event_name",
      "EventLabel" = "event_label"
    )
  ) |> 
  # create more real-world site codes:
  mutate(
    SiteCode = gsub("_[[:digit:]]+$", "", SubjectId),
    SiteCode = sub("_", "", SiteCode)
  )

#### Verify if outcome is the same as clinsightful_data after merging:
merged_data <- all_data |> 
  merge_meta_with_data(meta = metadata)
attr(merged_data, "synch_time") <- "2023-09-15 10:10:00 UTC"

###### Check what is needed to get exactly the same dataset:
###### 

old_clinsight_data <- clinsightful_data |> 
  ##### db_update_time is not used anymore and should be removed from 
  ##### clinsightful_data:
  select(-db_update_time) |> 
  ##### more realistic site codes needed:
  mutate(
    site_code = simplify_string(gsub("_[[:digit:]]+$", "", simplify_string(subject_id))),
    site_code = toupper(sub("_", "", site_code))
  ) |> 
  # because day and vis_day are incorrect in the current clinsight_data:
  select(-day, -vis_day) |> 
  # because old weight change since screening was incorrect. 
  # In the new dataset it is actually calculated:
  filter(item_name != "Weight change since screening") |> 
  arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat)

new_clinsight_data <- merged_data |> 
  select(c(any_of(names(clinsightful_data)), "form_type")) |> 
  select(-form_type, -day, -vis_day) |> 
  filter(item_name != "Weight change since screening") |> 
  # same column order makes comparisons easier
  arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat)

waldo::compare(old_clinsight_data, new_clinsight_data)

raw_data <- split(all_data, ~FormId)

######### Create raw data files 
######### 

lapply(
  names(raw_data), 
  \(x) {
    # To mimic data with two name rows (usually a long and a short name)
    df <- as.data.frame(lapply(raw_data[[x]], as.character))
    df <- df[c(1, 1:nrow(df)),]
    df[1,]<- as.list(names(raw_data[[x]]))
    names(df) <- simplify_string(names(df))
    readr::write_csv(
    df, 
    file = file.path(app_sys("raw_data"), paste0("clinsight_raw_", x, ".csv"))
  )
  }
)
merged_clinsight_data <- clinsight::get_raw_csv_data(
  app_sys("raW_data"), 
  synch_time = "2023-09-15 10:10:00 UTC"
  ) |> 
  merge_meta_with_data(metadata)

# waldo::compare(
#   merged_clinsight_data |> 
#     arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat), 
#   merged_data |> 
#     arrange(site_code, subject_id, item_group, item_name, event_date, event_repeat)
#   )
#   No difference apart from the order

## Use data-raw/clinsightful_data.R for recreating clinsightful_data from raw data
