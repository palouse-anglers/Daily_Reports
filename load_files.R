library("XML")
library("xml2")
library("tidyverse")
library("highcharter")


srv_path <- "../../Downloads/Steve/Steve/P4_Files_SRV/"
dmm_path <- "../../Downloads/Steve/Steve/P4_Files_DMM/"

# read_xml ----------------------------------------------------------------

xml_to_dataframe <- function(file_path,...){
  
  srv_paths <- fs::dir_info({{file_path}},type = "file") %>%
    mutate(ext=fs::path_ext(path)) %>%
    filter(ext=="xml") %>%
    pull(path)
  
  # loop through paths and read xml files
  # combine rowwise 
  map_dfr(srv_paths,
          ~ xmlParse(.x) %>%
            xmlToDataFrame(nodes = getNodeSet(., "//MRREvent"))) %>%
    # clean date
    mutate(Date_Time=lubridate::ymd_hms(EventDate),
           Date=as.Date(Date_Time))
  
}


# apply the function to load everything into a single dataframe 
srv_df <- xml_to_dataframe(srv_path)
dmm_df <- xml_to_dataframe(dmm_path)




# t=srv_df %>%
#   mutate(Species=case_when(
#     SpeciesRunRearType=="32H" ~ "HSTHD",
#     SpeciesRunRearType=="32W" ~ "WSTHD",
#     TRUE ~ "Other"
#     )
#   ) %>%
#   group_by(Date,Species) %>%
#   add_tally(name = "Daily_Count") %>%
#   # mutate(across(c(Weight,Length,MarkTemperature),
#   #               ~mean(as.numeric(.x),na.rm = TRUE),
#   #               .names = "mean_{.col}")
#   #        ) %>%
#   arrange(Date) %>%
#   pivot_wider(names_from = "Species",
#               values_from = c("Daily_Count","mean_MarkTemperature")) %>%
#   ungroup()
# 
# nrow(t)


# t2 <- t %>% 
#   #filter(Date=="2023-06-02") %>%
#   #distinct(Date,HSTHD,WSTHD) %>%
#   group_by(Date) %>%
#   mutate(across(c(HSTHD,WSTHD),~sum(.x,na.rm = TRUE)),
#             Daily_Total= HSTHD+WSTHD
#             ) %>%
#   distinct(Date,HSTHD,WSTHD,.keep_all = TRUE)
#   
  


# srv_df %>%
#   mutate(Weight= as.numeric(Weight),
#          Length= round(as.numeric(Length),2),
#          Species=case_when(
#            SpeciesRunRearType=="32H" ~ "Hatchery",
#            SpeciesRunRearType=="32W" ~ "Wild",
#            TRUE ~ "Other")
#   ) %>%
#   hchart(., "column", hcaes(x = Date, y = Length, group = Species)) %>%
#   hc_tooltip(formatter = JS("function(){
#                             return ('Tag:' + this.point.PITTag + 
#                             ' <br>Date: ' + this.point.Date +
#                              ' <br>Length: ' + this.point.Length
#                             )
#                             }"))





# srv_df %>%
#   filter(Date=="2023-06-02") %>%
#   mutate(Species=case_when(
#     SpeciesRunRearType=="32H" ~ "HSTHD",
#     SpeciesRunRearType=="32W" ~ "WSTHD",
#     TRUE ~ "Other")
#   ) %>%
#   group_by(Date,Species) %>%
#   add_tally(name = "Daily_Count") %>%
#   arrange(Date) %>%
#   pivot_wider(names_from = "Species",
#               values_from = "Daily_Count") %>%
#   ungroup() %>%
#   group_by(Date) %>%
#   mutate(across(c(HSTHD,WSTHD),~sum(.x,na.rm = TRUE)),
#          Daily_Total= HSTHD+WSTHD
#   ) %>%
#   distinct(Date,HSTHD,WSTHD,.keep_all = TRUE) %>%
#  

# Item plot ---------------------------------------------------------------

  
  srv_df_sum <- srv_df  %>%
    mutate(Species=case_when(
    SpeciesRunRearType=="32H" ~ "Hatchery",
    SpeciesRunRearType=="32W" ~ "Wild",
    TRUE ~ "Other")
  ) %>%
  group_by(Date,Species) %>%
  add_tally(name = "Daily_Count") %>%
  arrange(Date) %>%
  distinct(Date,Species,Daily_Count) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(Daily_Total=sum(Daily_Count))%>%
  ungroup() 

srv_df_sum_wide <- srv_df_sum%>%
  pivot_wider(names_from = "Species",
              values_from = "Daily_Count") 


ddm_df_sum <- dmm_df   %>%
  mutate(Species=case_when(
    SpeciesRunRearType=="32H" ~ "Hatchery",
    SpeciesRunRearType=="32W" ~ "Wild",
    TRUE ~ "Other")
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
