# clean dates, organize calendar and write agenda_final
#library(tidyverse)
library(gt)
library(gtExtras)
library(lubridate)
library(dplyr)

# update these two files each quarter
#schedule <- read.csv("documents/course-schedule_ex2change.csv")
schedule <- read.csv("documents/course-schedule.csv")

imp_dates <- read.csv("documents/dates-calendar.csv")
colnames(imp_dates) = c("Date", "Topic")

imp_dates <- imp_dates %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))

class_start <- imp_dates$Date[1]
class_end <- imp_dates$Date[2]

add_dates <- imp_dates %>% 
  filter(!Topic %in% c("class_start", "class_end"))
#################################################

# should not have to change anything!
main <- tibble(Date = seq(class_start,
                          class_end, by = "day") ) %>% 
  mutate(day = wday(Date)) %>% 
  filter(day %in% c(2, 4, 6) |  #mon, wed, fri
           Date == class_start) %>% #NU Tue
  select(Date) %>% 
  filter(!Date %in% c(add_dates$Date))
# must equal length of course-schedule.csv (27 days)

#match dates with topic covered
agenda_classes <- cbind(main, schedule)

#add holidays, reading week and final
agenda <- bind_rows(agenda_classes, add_dates) %>% 
  arrange(Date)


#set blanks as empty instead of NA
agenda[is.na(agenda)] <- "" 

#add due dates
agenda <- agenda %>% 
  mutate(due_ymd = case_when(wday(Date) == 6 ~ Date + 2,
                             wday(Date) == 3 ~ Date + 0,
                             TRUE ~ Date + 1),
           #if_else(wday(Date) %in% c(3,6), Date+2,Date +1),
         due = format(due_ymd, "%a %b %d"))


#adjust due date for MLK in Winter Quarter
mlk_row <- which(stringr::str_detect(agenda$Topic, "MLK"))

if(length(mlk_row) != 0){
  agenda$submit_icon[mlk_row] <- agenda$submit_icon[mlk_row-1]
  agenda$submit_link[mlk_row] <- agenda$submit_link[mlk_row-1]
  agenda$tutorial_icon[mlk_row] <- agenda$tutorial_icon[mlk_row-1]
  agenda$tutorial_link[mlk_row] <- agenda$tutorial_link[mlk_row-1]
  agenda$due_ymd[mlk_row-1] <- agenda$due_ymd[mlk_row]
  agenda$due[mlk_row-1] <- agenda$due[mlk_row]
}

# format all data
agenda_final <- agenda %>% 
  mutate(Week = week(Date),
         Week = Week - min(Week)+1,
         Week = ifelse(is.na(Week == lag(Week)), 1,
                       ifelse(Week == lag(Week),
                              "", Week) ),
         date_ymd = Date,
         Date = format(Date, "%a %b %d"),
         # Week = ifelse(Week == "", "",
         #        paste0("[Week ", Week,
         #               "](/weeks/week", 
         #               Week, ".html)")),
         Reading = paste0(Reading, ReadingLink) ) %>%
  mutate(Slides = paste0(slide_icon, slide_link),
         Activity = paste0(activity_icon, activity_link),
         submit_act = paste0(submit_icon, submit_link),
         Tutorial = paste0(tutorial_icon, tutorial_link),
         Solutions = paste0(solution_icon, solution_link)) %>% 
  select(-c(ReadingLink, slide_icon, slide_link, activity_icon, 
         activity_link, submit_icon, submit_link, tutorial_icon, 
         tutorial_link, solution_icon, solution_link))

agenda_final$reading_hw <- c(agenda_final$Reading[-1], "")

write.csv(agenda_final, "documents/agenda_final.csv", row.names = FALSE)
