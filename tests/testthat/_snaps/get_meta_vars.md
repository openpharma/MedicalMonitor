# get_meta_vars collects data and metadata in one list object: creates expected output

    Code
      get_meta_vars(appdata, metadata)
    Output
      $items
      $items$`Adverse events`
                          AE Number                       AE Name 
                        "ae_number"                     "ae_name" 
                               AESI                 AE start date 
                             "aesi"               "ae_start_date" 
                        AE end date          AE date of worsening 
                      "ae_end_date"        "ae_date_of_worsening" 
                  AE CTCAE severity   AE CTCAE severity worsening 
                "ae_ctcae_severity" "ae_ctcae_severity_worsening" 
               AE Treatment related           AE Treatment action 
             "ae_treatment_related"         "ae_treatment_action" 
                    AE Other action         Serious Adverse Event 
                  "ae_other_action"       "serious_adverse_event" 
                 SAE Awareness date                SAE Start date 
               "sae_awareness_date"              "sae_start_date" 
                       SAE End date                  SAE Category 
                     "sae_end_date"                "sae_category" 
                        SAE outcome             SAE Date of death 
                      "sae_outcome"           "sae_date_of_death" 
                   SAE Death reason 
                 "sae_death_reason" 
      
      $items$`CBC regular`
        Haemoglobin   Haematocrit     Platelets   Neutrophils   Lymphocytes 
      "haemoglobin" "haematocrit"   "platelets" "neutrophils" "lymphocytes" 
          Monocytes 
        "monocytes" 
      
      $items$`Conc. Procedures`
                         CP Number                      CP Name 
                       "cp_number"                    "cp_name" 
                     CP Indication                CP Start Date 
                   "cp_indication"              "cp_start_date" 
                        CP Ongoing                  CP End Date 
                      "cp_ongoing"                "cp_end_date" 
        CP Related Medical History                CP Related AE 
      "cp_related_medical_history"              "cp_related_ae" 
      
      $items$Electrolytes
             Sodium     Potassium      Chloride   Bicarbonate       Calcium 
           "sodium"   "potassium"    "chloride" "bicarbonate"     "calcium" 
          Magnesium 
        "magnesium" 
      
      $items$General
                            Age                       Sex                      ECOG 
                          "age"                     "sex"                    "ecog" 
                       Eligible             Eligible_Date        WHO.classification 
                     "eligible"           "eligible_date"      "who_classification" 
          WHO.subclassification                      Race     ChildbearingPotential 
        "who_subclassification"                    "race"   "childbearingpotential" 
                MenopauseReason       DiscontinuationDate     DiscontinuationReason 
              "menopausereason"     "discontinuationdate"   "discontinuationreason" 
                DisconDeathDate             DrugAdminDate             DrugAdminDose 
              "discondeathdate"           "drugadmindate"           "drugadmindose" 
           DoseModificationDate    DoseModificationReason   DoseModificationNewDose 
         "dosemodificationdate"  "dosemodificationreason" "dosemodificationnewdose" 
                DrugDiscontDate         DrugDiscontReason 
              "drugdiscontdate"       "drugdiscontreason" 
      
      $items$`Liver function`
        Bilirubin         AST         ALT         gGT 
      "bilirubin"       "ast"       "alt"       "ggt" 
      
      $items$`Medical History`
            MH Number         MH Name   MH Start Date      MH Ongoing     MH End Date 
          "mh_number"       "mh_name" "mh_start_date"    "mh_ongoing"   "mh_end_date" 
         MH Treatment      MH Comment 
       "mh_treatment"    "mh_comment" 
      
      $items$Medication
                         CM Number                CM Trade Name 
                       "cm_number"              "cm_trade_name" 
              CM Active Ingredient                CM Indication 
            "cm_active_ingredient"              "cm_indication" 
                           CM Dose                      CM Unit 
                         "cm_dose"                    "cm_unit" 
                     CM Unit Other                 CM Frequency 
                   "cm_unit_other"               "cm_frequency" 
                CM Frequency Other                     CM Route 
              "cm_frequency_other"                   "cm_route" 
                    CM Route Other                CM Start Date 
                  "cm_route_other"              "cm_start_date" 
                        CM Ongoing                  CM End Date 
                      "cm_ongoing"                "cm_end_date" 
        CM Related Medical History                CM Related AE 
      "cm_related_medical_history"              "cm_related_ae" 
      
      $items$`Renal function`
        Creatinine         eGFR        Urate 
      "creatinine"       "egfr"      "urate" 
      
      $items$Response
         Response   Responder 
       "response" "responder" 
      
      $items$`Vital signs`
              Systolic blood pressure        Diastolic blood pressure 
            "systolic_blood_pressure"      "diastolic_blood_pressure" 
                                Pulse                            Resp 
                              "pulse"                          "resp" 
                          Temperature   Weight change since screening 
                        "temperature" "weight_change_since_screening" 
                                  BMI                          Weight 
                                "bmi"                        "weight" 
      
      
      $all_forms
              main_tab             form
      1  Common events   Adverse events
      2  Common events       Medication
      3  Common events Conc. Procedures
      4  Common events  Medical History
      5     Study data      Vital signs
      6     Study data     Electrolytes
      7     Study data   Renal function
      8     Study data   Liver function
      9     Study data      CBC regular
      10    Study data         Response
      
      $subject_id
       [1] "BEL_08_45"  "BEL_04_133" "BEL_04_772" "BEL_07_193" "BEL_07_431"
       [6] "BEL_07_497" "BEL_07_645" "BEL_08_736" "BEL_08_885" "BEL_09_361"
      [11] "BEL_09_464" "BEL_09_556" "DEU_01_541" "DEU_01_977" "DEU_02_387"
      [16] "DEU_02_482" "DEU_02_866" "DEU_02_968" "NLD_06_72"  "NLD_03_207"
      [21] "NLD_05_282" "NLD_05_561" "NLD_06_755" "NLD_06_893" "NLD_06_959"
      
      $Sites
      # A tibble: 9 x 2
        site_code region
        <chr>     <chr> 
      1 BEL04     BEL   
      2 BEL07     BEL   
      3 BEL08     BEL   
      4 BEL09     BEL   
      5 DEU01     DEU   
      6 DEU02     DEU   
      7 NLD03     NLD   
      8 NLD05     NLD   
      9 NLD06     NLD   
      
      $table_names
                 Edit date                 Date                Event 
          "edit_date_time"         "event_date"        "event_label" 
                     Event                   eN                 Form 
              "event_name"       "event_repeat"         "item_group" 
                         N                Query             Resolved 
             "form_repeat"              "query"           "resolved" 
                    Author               Status              Subject 
                "reviewer"             "status"         "subject_id" 
                      Time                   Dx                 Type 
               "timestamp" "WHO.classification"               "type" 
      
      $form_level_data
               item_group item_scale use_unscaled_limits review_required
      1    Adverse events         NA                  NA            TRUE
      2        Medication         NA                  NA            TRUE
      3  Conc. Procedures         NA                  NA            TRUE
      4   Medical History         NA                  NA            TRUE
      5       Vital signs      FALSE                TRUE            TRUE
      6      Electrolytes       TRUE               FALSE            TRUE
      7    Renal function       TRUE               FALSE            TRUE
      8    Liver function       TRUE               FALSE           FALSE
      9       CBC regular       TRUE               FALSE            TRUE
      10         Response      FALSE               FALSE            TRUE
      11          General         NA                  NA            TRUE
      

