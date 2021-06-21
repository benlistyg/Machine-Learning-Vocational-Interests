# This code matches the hand-entered college major data from Open Psychometrics with the standardized list from ACT

# Note some observations required hand-coding as part of the matching procedure and are not reflected in the code

# ACT College Majors acquired from: http://www.act.org/content/act/en/research/reports/act-publications/college-choice-report-class-of-2013/college-majors-and-occupational-choices/college-majors-and-occupational-choices.html

library(dplyr)
library(tibble)

'%!in%' <- function(x,y)!('%in%'(x,y))

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

# 294 majors, 18 categories

ACT <- read.csv('Study 1/ACTMajors.csv', na.strings = c("", "NA")) %>% 
  mutate(Specific.Major = toupper(Specific.Major),
         General.Major = toupper(General.Major)) %>% 
  apply(., 2, trimws) %>% 
  data.frame() %>% 
  tibble()

# Complete list of majors in the education sample after filtering based on character length > 1 and removing observations with "AND" in the college major name
# educational_choice$major %>% table() %>% data.frame() %>% melt() %>% arrange(-value) %>% nrow()
# see lines 27 and 28 in study1.R
# This list was matched to the ACT majors list

majors <- read.csv('Study 1/majors.csv', na.strings = c("", "NA", "NA ")) %>% 
  apply(., 2, trimws) %>% 
  data.frame() %>% 
  tibble() %>% 
  select(-value) %>% 
  mutate(Specific.Major = toupper(Specific.Major),
         General.Major = toupper(General.Major)) %>% 
  unique()

majors$original_major <- gsub(' +',' ', majors$original_major) 

# Combined majors maps free response to the ACT standardized list.
# 3360 lines in total
# IMPORATNT TO NOTE:
  # Left joining the educational_choice table on combined majors should yield identical rows before/after join ()
combined_majors <- left_join(x = majors,
                             y = ACT,
                             by = c('Specific.Major')) %>%
  mutate(General.Major = coalesce(General.Major.x, General.Major.y)) %>% 
  select(-General.Major.x, -General.Major.y)

# Uncomment to generate CSV
# write.csv(x = combined_majors,file = 'combined_majors.csv')