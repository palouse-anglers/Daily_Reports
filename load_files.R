# load other peoples functions

library("XML")
library("xml2")
library("tidyverse")
library("highcharter")
library("googlesheets4")
library("googledrive")


options(googledrive_quiet=TRUE)

# paths -------------------------------------------------------------------

# spreadsheets ------------------------------------------------------------

# download_2023_spreadsheet <- 
#   drive_get(as_id("15M1zH7LF1ceNxNezROKcTCgcCfqlW7yY")) %>% 
#   drive_download(path="C:/Users/humme/Documents/Daily_Reports/2023/spreadsheet/tagging_2023.xlsx",overwrite = TRUE)

lgr_daily_names <- c("Elastomer HCH1",
                     "HCH1", "WCH1",
                     "HCH0", "WCH0",
                     "HSTHD","WSTHD",	
                     "HSOC",
                     "WSOC",	
                     "COHO")

download_2024_spreadsheet <- 
  drive_get(as_id("1_RpuJnZFD86aYpLJrP7QOL2ezCjx4Svs")) %>% 
  drive_download(
    path="C:/Users/humme/Documents/Daily_Reports/2024/spreadsheet/tagging_2024.xlsx",overwrite = TRUE)


# --------------WDFW Transport: Table Row = Handled/transported---------
wdfw_trans_2024 <- 
  readxl::read_xlsx("C:/Users/humme/Documents/Daily_Reports/2024/spreadsheet/tagging_2024.xlsx",
                    sheet = "WDFW tran report totals",skip = 2) %>%
  select(1,13:22)%>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(TAG_DATE=update(as.Date(DATE),year=2024),.before="DATE")%>%
  select(-DATE) %>%
  set_names(c("TAG_DATE",lgr_daily_names)) %>%
  mutate(Type="WDFD Trans",
         Table="Handled/transported")  

# -------------------Transport------------------------------------------
transport_2024 <- 
  readxl::read_xlsx("C:/Users/humme/Documents/Daily_Reports/2024/spreadsheet/tagging_2024.xlsx",
                    sheet = "Transport tagged",skip = 3) %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(TAG_DATE=update(lubridate::as_date(as.numeric(TAG_DATE)),year=2024))%>%
  mutate(across(c(HC:TAGGED),~as.numeric(.x)),
         across(c(HC:TAGGED),~replace_na(.x,0)),
         TAG_DATE= TAG_DATE- day(1)
  ) %>%
  mutate(Type="Transport",Table="Tagged/transported")%>%
  select(TAG_DATE,ELAST_FC,HSFC,WSFC,HC,WC,HS,WS,W_SOCK,H_SOCK,COHO,Type,Table)

#------------------------ Survival---------------------------------------
survival_2024 <- 
  readxl::read_xlsx("C:/Users/humme/Documents/Daily_Reports/2024/spreadsheet/tagging_2024.xlsx",
                    sheet = "Survival tagged",skip = 3) %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(TAG_DATE=update(lubridate::as_date(as.numeric(TAG_DATE)),year=2024))%>%
  mutate(across(c(HC:TAGGED),~as.numeric(.x)),
         across(c(HC:TAGGED),~replace_na(.x,0)),
         TAG_DATE= TAG_DATE- day(1)
) %>%
  mutate(Type="Survival",Table="Tagged/bypassed") %>%
  select(TAG_DATE,ELAST_FC,HSFC,WSFC,HC,WC,HS,WS,W_SOCK,H_SOCK,COHO,Type,Table)



# Bycatch
Bycatch_2024 <- 
  readxl::read_xlsx("C:/Users/humme/Documents/Daily_Reports/2024/spreadsheet/tagging_2024.xlsx",
                    sheet = "Bycatch",skip = 3) %>%
  janitor::clean_names(case = "screaming_snake") %>%
  mutate(TAG_DATE=update(lubridate::as_date(as.numeric(TAG_DATE)),year=2024)) %>%
  mutate(across(c(CATFISH:TOTALS),~as.numeric(.x)),
         across(c(CATFISH:TOTALS),~replace_na(.x,0))
         ) %>%
  mutate(Type="Bycatch",Table=NA_character_)



table_row_names <- 
c("Tagged/bypassed",
"Tagged/transported",
"Handled/bypass",
"Handled/transported",
"Examined (GBT Only)",
"Handled/not examined (GBT only)",
"Sacrificed",
"Morts")


# survuval and transport
daily_spreadsheet <- 
  survival_2024 %>%
    bind_rows(transport_2024)

# wdfw trans
wdfw_trans_2024
 

# xml files ---------------------------------------------------------------

get_xml_from_drive <- function(year){

  
drive_paths <- 
  googledrive::drive_ls(path = "https://drive.google.com/drive/folders/1HCAvBYxgUqPsQa6A-QnJiQoRrpv--b_p")
  
# table of directories
xml_files <- drive_paths %>%
  filter(name=={{year}}) %>%
  googledrive::drive_ls() %>%
  filter(name %in% c("P4 Files","P4_Data")) %>%
  googledrive::drive_ls(recursive = TRUE,pattern = ".*xml")  
# table of ids
# xml_ids <- 
#   map_dfr(xml_files$id,
#   ~drive_ls(as_id(.x))) %>%
#   select(name,id) %>%
#   drive_reveal(., "created_time")

  walk2(xml_files$id,xml_files$name,
       ~drive_get(as_id(.x)) %>%
       drive_download(overwrite = TRUE,
       path = glue::glue("C:/Users/humme/Documents/Daily_Reports/{year}/xml_files/{.y}")
       )
   )
}




# Save xml files ----------------------------------------------------------

# get_xml_from_drive("2023")

get_xml_from_drive("2024")

# paths to files ----------------------------------------------------------


# srv_path <- "../../Downloads/Steve/Steve/P4_Files_SRV/"
# dmm_path <- "../../Downloads/Steve/Steve/P4_Files_DMM/"

# read_xml ----------------------------------------------------------------

xml_to_dataframe <- function(file_path,...){
  
  srv_paths <- fs::dir_info({{file_path}},type = "file") %>%
    mutate(ext=fs::path_ext(path)) %>%
    filter(ext=="xml") %>%
    pull(path)
  
  # loop through paths and read xml files
  # combine rowwise 
  df <- map_dfr(srv_paths,
          ~ xmlParse(.x) %>%
            xmlToDataFrame(nodes = getNodeSet(., "//MRREvent")) %>%
            mutate(Type=case_when(
                   str_detect(.x,"SRV") & str_detect(.x,"PRE") ~ "Pre-Survival",
                   str_detect(.x,"SRV") & str_detect(.x,"SV") ~ "Survival",
                   str_detect(.x,"MRT") ~ "Mortality",
                   str_detect(.x,"GAA") ~ "Gordon Axel",
                   str_detect(.x,"ILR") ~"Lemhi",
                   TRUE ~ "Transport/Unknown"),
                   path=basename(.x)
                   
          )) %>%
    # clean date
    mutate(Date_Time=lubridate::ymd_hms(EventDate),
           Date=as.Date(Date_Time))
  
cli::cli_alert_info("Dates from {.val {range(df$Date)[1]}} to {.val {range(df$Date)[2]}}")

return(df)  
  
  }







# Test
# xmljune_16 <- 
#   xml_to_dataframe(file_path = glue::glue("{getwd()}/2023/xml_files/")) 
# 
# 
# april_9_2024 <- 
#   xml_to_dataframe(file_path = glue::glue("{getwd()}/2024/xml_files/"))


april_2024 <-
  xml_to_dataframe(file_path = glue::glue("{getwd()}/2024/xml_files/"))



# pre survival tag check --------------------------------------------------



pre_survival_check <- function(data){
   
pre_survival <- 
  data %>%
  filter(Type %in% "Pre-Survival") %>%
  pull(PITTag)

any(data$PITTag[!data$Type == "Pre-Survival"] %in% pre_survival)

}


# Check any pre tags slipped through
pre_survival_check(april_2024)
  
# Item plot ---------------------------------------------------------------

code_lookup <- read_csv("code_lookup.csv")

make_xml_summary <- function(data){
  
  data %>%
  left_join(code_lookup,by=c("SpeciesRunRearType"="Code")) %>% 
  rename(Species=Description) %>%
  group_by(Type,Date,Species) %>%
  add_tally(name = "Daily_Count") %>%
  arrange(Date) %>%
  distinct(Date,Species,Daily_Count,.keep_all=TRUE) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(Daily_Total=sum(Daily_Count))%>%
  ungroup()
}




# April 10th 2024 ---------------------------------------------------------

 april_2024 %>%
   filter(Date %in% '2024-04-23') %>%
         make_xml_summary() %>%
  select(Type,Date,Species,Daily_Count,WDFW)

 daily_spreadsheet %>%
   filter(TAG_DATE %in% '2024-04-23')
 
 wdfw_trans_2024 %>%
   filter(TAG_DATE %in% '2024-04-23')
 
 
 
 april_10_2024 %>%
   make_xml_summary() %>%
   filter(Date %in% "2024-04-10") %>%
  pivot_wider(names_from = "Species",
              values_from = "Daily_Count") 


ddm_df_sum <- dmm_df   %>%
  mutate(Species=case_when(
    SpeciesRunRearType=="32H" ~ "32Hatchery",
    SpeciesRunRearType=="32W" ~ "32Wild",
    TRUE ~ SpeciesRunRearType)
  ) %>%
  group_by(Date,Species) %>%
  add_tally(name = "Daily_Count") %>%
  arrange(Date) %>%
  distinct(Date,Species,Daily_Count) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(Daily_Total=sum(Daily_Count))%>%
  ungroup() 


# srv_df_sum %>%
# group_by(Date) %>%
# nest() %>%
# mutate(plot=map(data,
#     ~hchart(data,
#     "item",
#     hcaes(
#       name = Species,
#       y = Daily_Count,
#       color=c("#000","#64A12D")
#     ))
# ))
#     
#     
#     
#     ,
#     name = "Representatives",
#     showInLegend = TRUE,
#     size = "100%",
#     center = list("50%", "75%"),
#     startAngle = -100,
#     endAngle  = 100
#   ) %>%
#   hc_tooltip(formatter = JS("function(){
#                             return (
#                             ' <br>Date: ' + this.point.Date +
#                             ' <br>Total: ' + this.point.Daily_Count
#                             )
#                             }"))
