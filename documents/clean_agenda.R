# clean dates, organize calendar and write agenda_final
library(tidyverse)
library(gt)
library(gtExtras)
library(lubridate)

# update these two files each quarter
schedule <- read_csv("documents/course-schedule.csv")

imp_dates <- read_csv("documents/dates-calendar.csv")

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
  select(Date)
# must equal length of course-schedule.csv (27 days)

#match dates with topic covered
agenda_classes <- cbind(main, schedule)

#add reading week and final
agenda <- bind_rows(agenda_classes, add_dates)

#set blanks as empty instead of NA
agenda[is.na(agenda)] <- "" 

#add due dates
agenda <- agenda %>% 
  mutate(due_ymd = if_else(wday(Date) == 6, Date+2,Date +1),
         due = format(due_ymd, "%a %b %d"))

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
         Tutorial = paste0(tutorial_icon, tutorial_link)) %>% 
  select(-c(ReadingLink, slide_icon, slide_link, activity_icon, 
         activity_link, submit_icon, submit_link, tutorial_icon, 
         tutorial_link))

agenda_final$reading_hw <- c(agenda_final$Reading[-1], "")

write_csv(agenda_final, "documents/agenda_final.csv")