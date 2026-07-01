##Script to analyse Pathology QLD testing data
##01.01.2026
##Authors - Emily Mitchell and Rehan Villani


#load tidyverse and readxl and patchwork
library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(cowplot)
library(grid)
library(Hmisc)
library(readxl)
library(stats)
library(broom)

#set dir
rm(list = ls())
#setwd('E')
date = strftime(Sys.Date(),"%y%m%d")

##service data collected from 2017 - 2024. Data available on request from authors.
# import data 2023/2024a
PQ_QIMR_23_24_a <- read_csv("Service_data_on_request", 
                           skip = 9)
#import data 2023/2024b
PQ_QIMR_23_24_b <- read_csv("Service_data_on_request", 
                            skip = 9)
#import data 2023/2024c
PQ_QIMR_23_24_c <- read_csv("Service_data_on_request", 
                            skip = 9)
#import data 2023/2024d
PQ_QIMR_23_24_d <- read_csv("Service_data_on_request", 
                            skip = 9)
#import 2017-2022
PQ_QIMR_17to22 <- read_csv("Service_data_on_request", 
                           skip = 9)

#join 2017-2022 with 2023 b,c,d (2023 is in 4 files for transfer)
Joined17_23_raw = bind_rows(PQ_QIMR_17to22,PQ_QIMR_23_24_a, PQ_QIMR_23_24_b, PQ_QIMR_23_24_c, PQ_QIMR_23_24_d)
#looking at the raw data
head(Joined17_23_raw)

#date recieved to the service - RECDATE
summary(as.factor(Joined17_23_raw$RECDATE))
summary(as.factor(Joined17_23_raw$LOCATION))

#how many tests are referred out - ie GENREF = referred to testing elsewhere
length(str_detect(Joined17_23_raw$ALLTESTS, "GENREF"))

# counting and checking data table - Quality tests code = EXTQC and ASU 
summary(as.factor(Joined17_23_raw$LOCATION))
nrow(Joined17_23_raw)
length(Joined17_23_raw$LOCATION)
length(Joined17_23_raw$LOCATION[Joined17_23_raw$LOCATION == "EXTQC"])
length(Joined17_23_raw$LOCATION[Joined17_23_raw$LOCATION == "ASU"])
length(Joined17_23_raw$LOCATION[Joined17_23_raw$LOCATION == "NA"])

# remove quality control items EXTQC and ASU 
Joined17_23_rem_QC = filter(Joined17_23_raw, LOCATION != "EXTQC" & LOCATION != "ASU")
head(Joined17_23_rem_QC)

#select the tests from COVID WARDS
Joined17_23_rem_QC$WARD
#count wards
nrow(filter(Joined17_23_rem_QC, WARD == "VAX~QLONG"|
              WARD == "VAX~QOVAX"|
              WARD == "VAX~QMIX" |
              WARD == "STARS~QOVAX" |
              WARD == "CB~QOVAX" |
              WARD == "FUP~QOVAX" |
              WARD == "RBWH~QOVAX"))
#select wards
covid_w = filter(Joined17_23_rem_QC, WARD == "VAX~QLONG"|
              WARD == "VAX~QOVAX"|
              WARD == "VAX~QMIX" |
              WARD == "STARS~QOVAX" |
              WARD == "CB~QOVAX" |
              WARD == "FUP~QOVAX" |
              WARD == "RBWH~QOVAX")

#COVID tests
#count tests
nrow(filter(Joined17_23_rem_QC, str_detect(REFTST, "SARS")|str_detect(REFTST, regex("COVID", ignore_case = TRUE))|str_detect(REFTST, regex("q fever", ignore_case = TRUE))))
#select tests
covid_t = filter(Joined17_23_rem_QC, str_detect(REFTST, "SARS")|
                   str_detect(REFTST, regex("COVID", ignore_case = TRUE))|
                   str_detect(REFTST, regex("q fever", ignore_case = TRUE)))

n_distinct(Joined17_23_rem_QC$LABNO)
head(Joined17_23_rem_QC)

#filter the dataset to remove the COVID (QC also removed already)
Joined17_23_rem_QC_rem_COVID = Joined17_23_rem_QC %>% 
  filter(!LABNO %in% covid_t$LABNO)%>% 
  filter(!LABNO %in% covid_w$LABNO)
                                      

####DATA analysis and clean up
PQ_data_all = Joined17_23_rem_QC_rem_COVID
head(PQ_data_all)

#Add year column 
PQ_data_all = mutate(PQ_data_all, DATE = REQDATE)
summary(as.factor(PQ_data_all$DATE))

#noting that all the weird REQDATE 
summary(as.factor(PQ_data_all$REQDATE))
summary(as.factor(PQ_data_all$RECDATE))
summary(as.factor(PQ_data_all$DATE))

#keeping included years based on  date test recieved
PQ_data_all$YEAR <- format(as.Date(PQ_data_all$RECDATE, format="%d/%m/%Y"),"%Y")
summary(as.factor(PQ_data_all$YEAR))

PQ_data_clean = PQ_data_all %>% filter(YEAR == "2017"|
                     YEAR == "2018"|
                     YEAR == "2019"|
                     YEAR == "2020"|
                     YEAR == "2021"|
                     YEAR == "2022"|
                     YEAR == "2023")

#checking what was excluded
excludedbyyr = PQ_data_all %>% filter(!LABNO %in% PQ_data_clean$LABNO)
excludedbyyr
#excludedbyyr = mutate(excludedbyyr, DATE = REQDATE)

#converting some of the error ??:?? RECDATES
excludedbyyr$RECDATE = sub("^\\?\\?:\\?\\?\\s+", "", excludedbyyr$RECDATE)
excludedbyyr$YEAR <- format(as.Date(excludedbyyr$RECDATE, format = "%d-%b-%y"), "%Y")
excludedbyyr_rejoin = excludedbyyr %>% filter(YEAR == "2017"|
                                                YEAR == "2018"|
                                                YEAR == "2019"|
                                                YEAR == "2020"|
                                                YEAR == "2021"|
                                                YEAR == "2022"|
                                                YEAR == "2023")

# select the 'corrected' years and add to the PQ DATA
PQ_data_clean = rbind(PQ_data_clean, excludedbyyr_rejoin)
summary(as.factor(PQ_data_clean$YEAR))

#look a the data stats
str(PQ_data_clean)
summary(PQ_data_clean)


#what remains excluded
excluded_2 = PQ_data_all %>% filter(!LABNO %in% PQ_data_clean$LABNO) 
excluded_2 %>% mutate_all(as.factor) %>% summary()
#13948 still excluded. looks like the excluded were all either prior to 2017 or 2024

#FINAL DATA
PQ_data = PQ_data_clean
PQ_data %>% mutate_all(as.factor) %>% summary()

#SAVE OF clean data  for efficient final analysis

#Save temp file of clean data table
write.csv(PQ_data,file = paste(gsub(":", "-", Sys.Date()),"PQ_data_clean1.csv"), row.names = FALSE)

##############looking at the specialties#############------------------------------------

#looking at the specialties - ward code identifies specialty - data library/translation manually generated
#import ward table 
Wards_table <- read_excel("Wards_table.xlsx", sheet = "Sheet1")

#summary of wards table information
summary(as.factor(Wards_table$Description))

#Add Ward descriptions
colnames(PQ_data)
colnames(Wards_table)
all_w_ward_desc = left_join(PQ_data, Wards_table, join_by(WARD ==Mnemonic))

#add specialty column 
joined_w_year_specialty = mutate(all_w_ward_desc, SPECIALTY = WARD)

#import created list of unknown wards
Genomics_test_list_clean_Sheet5_ <- read_csv("Genomics test list (clean)(Sheet5).csv", 
                                             col_names = FALSE, skip = 1)
unknowns = as.vector(Genomics_test_list_clean_Sheet5_$X1)

#rename all specialties
##run outpatients first
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Outp")|str_detect(Description, "Medical Ou")|str_detect(Description, "OPD")|str_detect(Description, "Ambulato")|str_detect(WARD, "OPD"),"Outpatients", .))

##run specialties 
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Alco"),"Addiction Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Anaest"),"Anaesthesia", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Cardio")|str_detect(Description, "Hypertension")|str_detect(Description, "Heart")|str_detect(Description, "Cath")|str_detect(Description, "Electro")|str_detect(Description, "Cardiac")|str_detect(WARD, "MOT~PCH")|str_detect(Description, "Coronary")|
                          str_detect(WARD, "CPAS~PCH")|str_detect(WARD, "CECHO~PAH")|str_detect(WARD, "CIUTOE~CNH")|str_detect(WARD, "CIUEST~CNH"),"Cardiology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Dermatology"),"Dermatology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Emerg")|str_detect(Description, "ED S")|str_detect(Description, regex("emg", ignore_case = TRUE))|str_detect(WARD, "EDCDU~LGH")|str_detect(WARD, "WAA~PAH")|str_detect(WARD, "GEAC~GCUH")|str_detect(WARD, "STTA~LGH"),"Emergency Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Endo")& str_detect(Description, "Endoscopy", negate = TRUE)|str_detect(Description, "Diab")|str_detect(WARD, "ENDO~CAHLC"), "Endocrinology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Gastro")|str_detect(Description, "Hep")|str_detect(Description, "Liver")|str_detect(Description, "Bowel"),"Gastroenterology and Hepatology ", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "General")&str_detect(Description, "Med")|str_detect(Description, "^General \\(")|str_detect(Description, "General")&str_detect(Description, "Ward")|str_detect(Description, "Medical Unit")|str_detect(Description, "Medical \\(")|str_detect(Description, "Medical 5")|
                           str_detect(Description, "Medical C")|str_detect(Description, "Medical Ward")|str_detect(Description, "Medical A")|str_detect(Description, "Medical B")|str_detect(Description, "Med A")|str_detect(Description, "Medical 2")|str_detect(Description, "Gen Med")|str_detect(Description, "Internal")|str_detect(Description, "^Inpatient"),"General Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Family Prac")|str_detect(Description, "General Prac")|str_detect(Description, "Primary")|str_detect(Description, "Community")|str_detect(Description, "Kirwan")|str_detect(WARD, "GPUB~KILH")|str_detect(WARD, "ARMY")|str_detect(Description, "Prison")|str_detect(Description, "Correcti"),"General Practice", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Geri")|str_detect(Description, "geriatric")|str_detect(Description, "Elder")|str_detect(Description, "Older")|str_detect(Description, "Aged")|str_detect(WARD, "GLAD~CBH"),"Geriatric Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Haem")& str_detect(Description, "Oncology", negate=TRUE)|str_detect(WARD, "HAEM")& str_detect(WARD, "CSHEAM", negate = TRUE)|str_detect(Description, "Throm"), "Haematology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Immunology")|str_detect(Description, "Allergy")|str_detect(WARD, "IMM"),"Immunology and Allergy", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Infectious")|str_detect(WARD, "COV")|str_detect(Description, "COVID"),"Infectious Disease Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Intensive")|str_detect(Description, "^Crit")|str_detect(SPECIALTY, "HDU")|str_detect(SPECIALTY, "HCU"),"Intensive Care Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Admin"),"Medical Administration", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Nephrology")|str_detect(Description, "Renal")|str_detect(Description, "Dial")|str_detect(WARD, "KHS~ACQUIR"),"Nephrology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Neuro")|str_detect(Description, "Neuro ")|str_detect(Description, "Spinal")|str_detect(Description, "Stroke")|str_detect(WARD, "NEU")|str_detect(WARD, "BIRU~PAH"),"Neurology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Nuclear"),"Nuclear Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Gynae")|str_detect(Description, "Preg")|str_detect(Description, "Postnatal")|str_detect(Description, "Midw")|str_detect(Description, "Baby")|str_detect(Description, "Women")|str_detect(Description, "Babies")|str_detect(Description, "Obstetric")|str_detect(Description, "Maternal")|
                          str_detect(Description, "Maternity")|str_detect(Description, "Labour")|str_detect(Description, "Birth")|str_detect(Description, "Prenatal")|str_detect(Description, "Antenatal")|str_detect(Description, "AnteNatal")|str_detect(WARD, "GMATU~GCUH")|str_detect(WARD, "VCGS~NIPT"),"Obstetrics and Gynaecology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Occup")&str_detect(Description, "Therapy", negate = TRUE),"Occupational and Environmental Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Ophth")|str_detect(Description, "Eye")|str_detect(Description, "Opth"),"Ophthalmology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Pain")&str_detect(WARD, "CPAS~PCH", negate= TRUE),"Pain Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Palliative")|str_detect(Description, "Pallative")|str_detect(Description, "Pali"),"Palliative Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Path")&str_detect(Description, "Speech", negate = TRUE)&str_detect(Description, "Sp", negate = TRUE)|str_detect(WARD, "QML")|str_detect(SPECIALTY, "DHMP")|str_detect(Description, "Sullivan \\&")|str_detect(Description, "Tissue")|str_detect(Description, "Mater L")|str_detect(WARD, "LIS~PALMNR")|
                          str_detect(Description, "Forens")|str_detect(WARD, "SEALS")|str_detect(WARD, "PATHN")|str_detect(Description, "Karyo")|str_detect(WARD, "~PP")& str_detect(WARD, "PP~PP", negate = TRUE)& str_detect(WARD, "PPNP~PP", negate = TRUE)& str_detect(WARD, "IPC~PP", negate = TRUE)& str_detect(WARD, "IIHC~PP", negate = TRUE)|str_detect(Description, "Metab")|str_detect(WARD, "ACL~ACLNSW")|str_detect(WARD, "LAB"),"Pathology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Public Health")|str_detect(Description, "TSI")|str_detect(Description, "Aboriginal")|str_detect(WARD, "HNE~HNEPH")|str_detect(WARD, "IIHC~PP"),"Public Health Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Mental")|str_detect(Description, "Psyc")|str_detect(Description, "Cog")|str_detect(Description, "MH ")|str_detect(Description, "Mntl"),"Psychiatry", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Radiology")|str_detect(Description, "Medical Imaging")|str_detect(WARD, "XRAY")|str_detect(WARD, "MEDIM~MKH")|str_detect(WARD, "US~PCH")|str_detect(WARD, "MID~IPH")|str_detect(WARD, "PETC~ALZHC"),"Radiology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Rehab")|str_detect(WARD, "REHAB"),"Rehabilitation Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Respira")|str_detect(Description, "Thoracic M")|str_detect(Description, "^Thoracic")|str_detect(Description, "Resp ")|str_detect(Description, "Cystic F")|str_detect(Description, "Sleep")|str_detect(WARD, "CHEST~MKH")|str_detect(WARD, "CC~RKH"),"Respiratory and Sleep Medicine", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Rheum"),"Rheumatology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Sexual"),"Sexual Health Medicine", .))

#discovered specialties 
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Mortuary")|str_detect(Description, "Morgue"),"Mortuary", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Private"),"Private Practice", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(WARD, "PRIMO")|str_detect(Description, "WGS")|str_detect(Description, "Project")|str_detect(Description, "Trial")|str_detect(Description, "Study")|str_detect(Description, "Research"),"Research", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Speech")|str_detect(Description, "Allied")|str_detect(Description, "Audio")|str_detect(Description, "Nutri")|str_detect(WARD, "TWDIET~TNH")|str_detect(Description, "^Physio")|str_detect(Description, "Pod")|
                          str_detect(Description, "Occup")& str_detect(Description, "Therapy")|str_detect(Description, "Hearing") ,"Allied Health", .))

#unknown specialties
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Unknown")|str_detect(Description, "Virtual")|str_detect(Description, "North \\(R")|str_detect(Description, "South \\(")|str_detect(Description, "Transit")|
                          str_detect(Description, "Specialist")& str_detect(Description, "OPD", negate = TRUE)|str_detect(Description, "West \\(R")|str_detect(Description, "East \\(")|str_detect(Description, "Hospital")&str_detect(Description, "Private", negate = TRUE)&str_detect(Description, "PP", negate = TRUE)|
                          str_detect(Description, "West \\(N")|str_detect(Description, "Home Ward")|str_detect(Description, "Discharge")|str_detect(Description, "^Await")|str_detect(SPECIALTY, "SS")|str_detect(WARD, "HOME")|str_detect(WARD, "CDU")|str_detect(Description, "^Pre-A")|str_detect(Description, "^Acute \\(")|str_detect(WARD, "RFDS")|str_detect(Description, "Preadm")|joined_w_year_specialty$WARD %in% unknowns,"Unknown Specialty", .))

#Run genetics, oncology, surgery and paeds last in that order as these override other categories 
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "GHQ")|str_detect(Description, "Genetic")|str_detect(Description, "Genomic"),"Clinical Genetics", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Onc")|str_detect(Description, "Adem")|str_detect(Description, "Cancer")|str_detect(LOCATION, "3763")|str_detect(Description, "Chemo")|str_detect(WARD, "LEUK~4022QG")|str_detect(Description, "Myel")|str_detect(Description, "myel")|str_detect(Description, "Oncl")|str_detect(WARD, "CRADBRCA~PAH"), "Oncology", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(Description, "Surg")|str_detect(Description, "surg")|str_detect(Description, "Colp")|str_detect(Description, "Theatre")|str_detect(WARD, "TSORT~TNH")|str_detect(WARD, "TSENTE~TNH")|str_detect(WARD, "MOPS~BNH")|str_detect(Description, "Orthop")|str_detect(Description, "Transplant")|str_detect(Description, "Extended Day Surgery")|str_detect(Description, "Plastic")|str_detect(Description, "^Extend Day")|str_detect(Description, "Endoscopy")|
                          str_detect(Description, "Bronchoscopy")|str_detect(Description, "Max")|str_detect(Description, "Oper")|str_detect(Description, "Ortho.")|str_detect(Description, "oper")|str_detect(Description, "Operating")|str_detect(Description, "Urolo")|str_detect(WARD, "SUR")|str_detect(WARD, "VAS")|str_detect(WARD, "UROL")|str_detect(WARD, "ENT~")|str_detect(Description, "Procedur")|str_detect(Description, "Surgical")|str_detect(Description, "Day Proc"),"Surgery", .))
joined_w_year_specialty <- joined_w_year_specialty %>% mutate_at(vars(c(SPECIALTY)), ~if_else (str_detect(WARD, "QCH")|str_detect(WARD, "LCCH")|str_detect(WARD, "PEAD~")|str_detect(Description, "PEAD")|str_detect(Description, "PAED")|str_detect(WARD, "BABY")|str_detect(WARD, "PAEDD")|str_detect(WARD, "KID")|str_detect(Description, "CDS")|str_detect(Description, "Neonat")|str_detect(Description, "Paed")|str_detect(Description, "Pead")|str_detect(Description, "Youth")|
                          str_detect(Description, "Nursery")|str_detect(Description, "Child")|str_detect(Description, "Chld")|str_detect(Description, "Cld")|str_detect(Description, "Infant")|str_detect(Description, "Developm")|str_detect(Description, "Adolesc")|str_detect(WARD, "TCCYPNBG~TNH")|str_detect(WARD, "MCON~RCH")|str_detect(Description, "Chld H"), "Paediatrics", .))

##previously had joined outpatients with surgery. Have separated this, but moved to Supp as difficult to interpret
#join outpatients with surgery - as per PQ scientist recommendation
#summary(as.factor(joined_w_year_specialty$SPECIALTY))
#joined_w_year_specialty$SPECIALTY_original = joined_w_year_specialty$SPECIALTY
#joined_w_year_specialty$SPECIALTY = as.factor(ifelse(joined_w_year_specialty$SPECIALTY == "Surgery"| joined_w_year_specialty$SPECIALTY == "Outpatients",
#                                                     "Surgery and Outpatients", joined_w_year_specialty$SPECIALTY))

#create specialty lists for QC
SPEC = count(joined_w_year_specialty, SPECIALTY, sort = TRUE)
DESC = count(joined_w_year_specialty, Description, SPECIALTY, sort = TRUE)

#import test/panel codes 
Genomics_test_list_clean_Sheet1_ <- read_csv("E:/project_genetics_use/Emily M data/raw_data/Genomics test list (clean)(Sheet1).csv")
Genomics_test_list_clean_Sheet1_ = add_row(Genomics_test_list_clean_Sheet1_, QHPS_test_code = "GENREF", Name = "Genetic Referral")
Genomics_test_list_clean_Sheet1_ = add_row(Genomics_test_list_clean_Sheet1_, QHPS_test_code = "REFER", Name = "Referral")
Genomics_test_list_clean_Sheet1_ = add_row(Genomics_test_list_clean_Sheet1_, QHPS_test_code = "WES", Name = "Whole Exome Somatic Analysis")
Genomics_test_list_clean_Sheet1_ = add_row(Genomics_test_list_clean_Sheet1_, QHPS_test_code = "COLON", Name = "COLON Cancer GA")
Genomics_test_list_clean_Sheet1_ = add_row(Genomics_test_list_clean_Sheet1_, QHPS_test_code = "BRCA", Name = "Breast Cancer GA")
Genomics_test_list_clean_Sheet1_ = filter(Genomics_test_list_clean_Sheet1_, QHPS_test_code != "DNAEX1"& QHPS_test_code != "DNAEX2"& QHPS_test_code != "DNAEX3"&QHPS_test_code != "DNAEX4"& QHPS_test_code != "DNAEX5"& QHPS_test_code != "DNAEX6"& QHPS_test_code != "DNAEX7")
Genomics_test_list_clean_Sheet1_ = filter(Genomics_test_list_clean_Sheet1_, QHPS_test_code != "VCWGS"& QHPS_test_code != "WGSURG" & QHPS_test_code != "VARDE5"& QHPS_test_code != "VARDE4"&QHPS_test_code != "VARDE3"& QHPS_test_code != "VARDE2"& QHPS_test_code != "VARDE1"& QHPS_test_code != "MGBANK")
Genomics_test_list_clean_Sheet1_ = filter(Genomics_test_list_clean_Sheet1_, QHPS_test_code != "MOLMIS"& QHPS_test_code != "BMCUL")

#turn column of test codes into string
list = as.vector(Genomics_test_list_clean_Sheet1_$QHPS_test_code)
list

#separate out all tests in new columns
joined_w_year_specialty = mutate(joined_w_year_specialty, GENTEST = ALLTESTS)
joined_w_year_specialty=joined_w_year_specialty %>% separate_wider_delim(GENTEST,delim = ",", names = letters[1:15], too_few = "align_start")

#define genetic test or not using list and if it had been referred out 
cols_to_check = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o")

joined_w_year_specialty <- joined_w_year_specialty %>%
  mutate(TEST = if_else(
    !is.na(REFTST) | if_any(all_of(cols_to_check), ~ .x %in% list),
    "Genetic test",
    "nottest"
  ))


#undetermined test or not due to character limit
joined_w_year_specialty <- joined_w_year_specialty %>%
  mutate(
    TEST = case_when(
      TEST == "nottest" & rowSums(across(all_of(cols_to_check), ~ str_detect(.x, "\\..."))) > 0 ~ "Undetermined",
      TRUE ~ TEST
    )
  )

#annotate PQ data with genetic test status
colnames(joined_w_year_specialty)
#select columns from specialty conversion data
cols_to_join_gentest = joined_w_year_specialty %>% select(c(LABNO,TEST))

### JOIN TO complete data set
#select columns from specialty conversion data
colnames(joined_w_year_specialty)
cols_to_join_specialty = joined_w_year_specialty %>% select(c(LABNO,Description,SPECIALTY,TEST))

#join specialty columns to the PQ data
PQ_data = left_join(PQ_data, cols_to_join_specialty, join_by(LABNO))
#summary(PQ_data)


#look at what is not a 'genetic test'
#summary(as.factor(PQ_data$GENTEST))
summary(as.factor(PQ_data$TEST))

PQ_data %>% filter(TEST != "Genetic test")
#counting the number of tests that are not 'genetic tests'
PQ_data %>% filter(TEST != "Genetic test") %>% nrow()
#what type of tests are in the 'not genetic tests' category
PQ_data %>% filter(TEST != "Genetic test") %>% summarise(as.factor(ALLTESTS))

colnames(PQ_data)
PQ_data$GENREF = as.factor(ifelse( !is.na(PQ_data$REFTST),"External", "Internal"))
summary(PQ_data$GENREF)
summary(as.factor(PQ_data$TEST))

table(PQ_data$TEST,PQ_data$GENREF)

#Save temp file of clean data table - second version 
write.csv(PQ_data,file = paste(gsub(":", "-", Sys.Date()),"PQ_data_clean2.csv"), row.names = FALSE)

####LOOKING AT THE GENETIC TESTS ONLY############################

#Filter for gene tests
summary(as.factor(joined_w_year_specialty$TEST))
gene_test_only = filter(joined_w_year_specialty, TEST == "Genetic test")
str(gene_test_only)
PQ_gene_test_only = filter(PQ_data, TEST == "Genetic test")
str(PQ_gene_test_only)
summary(as.factor(PQ_data$TEST))

#Add gen ref column 
gene_test_only$GENREF = ifelse( !is.na(gene_test_only$REFTST),"External", "Internal")
z = filter(gene_test_only, GENREF == "External")
nongenref = filter(gene_test_only, GENREF != "External")

summary(as.factor(gene_test_only$GENREF))

#filter out nottest
tests = filter(joined_w_year_specialty, TEST!= "nottest")
no_tests = filter(joined_w_year_specialty, TEST== "nottest")

undetermined = filter(joined_w_year_specialty, TEST== "Undetermined")

genetest_data = count(gene_test_only, YEAR)
genetest_data$YEAR = as.numeric(genetest_data$YEAR)
str(genetest_data)

#Save temp file of clean data table - gene tests only
write.csv(PQ_data,file = paste(gsub(":", "-", Sys.Date()),"PQ_data_clean_genetestonly.csv"), row.names = FALSE)

#graph plot of total number of tests with trendline
genetest_data_genref= count(gene_test_only, YEAR, GENREF)
genetest_data_genref$YEAR = as.numeric(genetest_data_genref$YEAR)
str(genetest_data_genref)

genetest_data_genref_ext = filter(genetest_data_genref, GENREF == "External")
genetest_data_genref_int = filter(genetest_data_genref, GENREF == "Internal")

#version 2 of plot - with trendlines - preferred version
plot_gentest_tl_B = ggplot(genetest_data, aes(YEAR, n)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "grey60")+ 
  scale_x_continuous(breaks = unique(genetest_data$YEAR))+
  labs(y = "Total number of Tests", x = "Year")+
  theme(legend.position = "none")+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE, color = "grey19")+
  geom_point(data = genetest_data_genref_ext, aes(YEAR, n), color = "#F8766D", size = 3) +
  geom_smooth(data = genetest_data_genref_ext, aes(YEAR, n), method = "lm", se = FALSE, color = "#F8766D",linewidth = 1.5)+
  geom_point(data = genetest_data_genref_int, aes(YEAR, n), color = "#00BFC4", size = 3) +
  geom_smooth(data = genetest_data_genref_int, aes(YEAR, n), method = "lm", se = FALSE, color = "#00BFC4", linewidth = 1.5)
plot_gentest_tl_B

genetest_data$YEAR = as.numeric(genetest_data$YEAR)
genetest_data_genref$YEAR = as.numeric(genetest_data_genref$YEAR)

ggplot(genetest_data, aes(YEAR, n)) +
  geom_point(color = "red")

#counting the number of tests per specialty
SPEC2 = count(gene_test_only, SPECIALTY, sort = TRUE)

#plot of the tests per specialty 
ggplot(SPEC2, aes(x = reorder(SPECIALTY,n),y=n))+
         geom_col()+ 
         coord_flip()+ scale_y_log10()

#how many specialties have done more than 100 tests
sum(SPEC2$n > 100)

#how many specialties have done more than 1000 tests
sum(SPEC2$n > 1000)

#plot of the tests per specialty for only the specialties that have done more than 100 tests
plot_spec_top = SPEC2 %>% filter(n > 100) %>% ggplot(., aes(x = reorder(SPECIALTY,n),y=n))+
  geom_col()+ 
  coord_flip()+ scale_y_log10()+
  labs(y = "Test number (log10)", x = "Specialty")+ 
  theme(axis.text.y = element_text(size = 12))
plot_spec_top

gene_only_counts = count(gene_test_only, YEAR, TEST,GENREF)

#count the number of gene_test_only totals in GENREF
count_gene_test_only = gene_test_only %>% group_by(YEAR) %>% filter(GENREF =="External") %>% count()

head(count_gene_test_only)
genetest_count = gene_test_only %>% group_by(YEAR) %>% filter(GENREF =="External") %>% count()
genetest_count_ex = gene_test_only %>% group_by(YEAR) %>% filter(GENREF =="External") %>% count()
genetest_count_in = gene_test_only %>% group_by(YEAR) %>% filter(GENREF =="Interal") %>% count()

########specialties

#count tests by specialties by year
spec_count_year_long = count(gene_test_only, SPECIALTY, YEAR, sort = TRUE)
spec_count_year = spread(spec_count_year_long, YEAR, n)
spec_count_year$TOTAL = rowSums(spec_count_year[2:8], na.rm = TRUE)
spec_count_year = arrange(spec_count_year, desc(TOTAL))
#write.csv(spec_count_year, "spec_count_year.csv", row.names = F)
spec_count = count(gene_test_only, SPECIALTY, sort = TRUE)
#write.csv(spec_count, "spec_count.csv", row.names = F)

#double check no 2024
filter(spec_count_year_long, YEAR == "2024")

#ggsave("specialties.png",plot = g_spec, width  = 8, height = 10, scale = 1.5)

#count wards
WARDS = count(gene_test_only, WARD, sort = TRUE)
SPEC = count(gene_test_only, SPECIALTY, sort = TRUE)
SPEC_GENE_ONLY =  count(gene_test_only, SPECIALTY, sort = TRUE)
descr = filter(gene_test_only, str_detect(SPECIALTY, "~"))

#graph only specialties that have more than 50 tests a year
no_small = filter(spec_count_year, spec_count_year[["2017"]] >= 50&spec_count_year[["2018"]] >= 50&spec_count_year[["2019"]] >= 50&spec_count_year[["2020"]] >= 50&spec_count_year[["2021"]] >= 50&spec_count_year[["2022"]] >= 50&spec_count_year[["2023"]] >= 50)
no_small_gathered = gather(no_small, "YEAR", "n", 2:8)
no_small_swap = spread(no_small_gathered, SPECIALTY, n)
no_small_swap = no_small_gathered %>% select(-TOTAL) %>% pivot_wider(names_from = YEAR, values_from = n)

#graph all specialties by year and save - but only the smaller specialties ####DEV######
#filter for only the top 15 using specialties
SPEC_GENE_ONLY %>% filter(n > 500)

#plot the top 15 specialties
spec_count_top15 = filter(no_small[1:15,])
spec_count_top15_long = spec_count_top15 %>% select(-TOTAL) %>% pivot_longer(,cols = c(2:8),names_to = "YEAR", values_to = "n")
g_spec_top = spec_count_top15_long %>% #select(-TOTAL) %>% pivot_longer(,cols = c(2:8),names_to = "YEAR", values_to = "n")
  ggplot(., aes(YEAR, n, group = SPECIALTY, colour = SPECIALTY))+ 
  geom_point()+ geom_smooth(method = "lm",se=FALSE) + 
  facet_wrap(vars(SPECIALTY), scale = "free_y", ncol = 3)+ theme_bw() +
  theme(legend.position="none", strip.text = element_text(size=13), 
        axis.text = element_text( size = 10), plot.title = element_text(size = 10))
g_spec_top

#stats on if they fit the linear regression
stats <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(stats) <- c("var1", "var2", "correlation", "pvalue")

###spec_count_year ####  ##correlation test - spearman
head(spec_count_year)
head(spec_count_year_long)

spec_count_year_long$YEAR <- as.numeric(spec_count_year_long$YEAR)
spec_count_year_long_1 = spec_count_year_long %>% filter(SPECIALTY == "Paediatrics")
  cor.test(spec_count_year_long_1$YEAR, spec_count_year_long_1$n, method = "spearman")

cor_results <- spec_count_year_long %>%
  group_by(SPECIALTY) %>%
  do({
    if (nrow(.) >= 3 && length(unique(.$n)) > 1) {
      test <- cor.test(~ YEAR + n, data = ., method = "spearman")
      tibble(
        rho = unname(test$estimate),   # correlation coefficient
        p.value = test$p.value         # p-value
      )
    } else {
      tibble(rho = NA, p.value = NA)
    }
  }) %>%
  ungroup()
arrange(cor_results, desc(rho))

results2 <- spec_count_year_long %>% filter(n >50) %>%
  group_by(SPECIALTY) %>%
  do({
    if (nrow(.) >= 3 && length(unique(.$n)) > 1) {
      test <- cor.test(~ YEAR + n, data = ., method = "spearman")
      tibble(
        rho = unname(test$estimate),   # correlation coefficient
        p.value = test$p.value         # p-value
      )
    } else {
      tibble(rho = NA, p.value = NA)
    }
  }) %>%
  ungroup()
arrange(results2, desc(rho))


##look at spec_count_year_long
# Correlation in loop
for(i in colnames(spec_count_year_long[,1])) {
  for(j in colnames(spec_count_year_long[,2])) {
    a <- cor.test(spec_count_year[[2]], spec_count_year[[3]], method = "spearman")
    stats <- rbind(stats, data.frame(
      "var1" = i,
      "var2" = j,
      "correlation" = a$estimate,
      "pvalue" = a$p.value) )
  }
}

# Remove rownames
rownames(stats) <- NULL
write.csv(stats, paste(gsub(":", "-", Sys.Date()),"_speciality_stats.csv", row.names = F))

g_spec_no_small <- ggplot(filter(no_small_gathered, YEAR != "2024"), aes(YEAR, n, group = SPECIALTY, colour = SPECIALTY))+ geom_point()+ theme_bw() + geom_smooth(method = "lm",se=FALSE) + facet_wrap(vars(SPECIALTY), scale = "free_y", ncol = 3)+ theme(legend.position="none", strip.text = element_text(size=13), axis.text = element_text( size = 8))
ggsave("specialties_no_small.png",plot = g_spec_no_small, width  = 8, height = 10, scale = 1.5)

#correlation for gene tests only 
gene_only_counts$YEAR <- as.numeric(gene_only_counts$YEAR)
filter = filter(gene_only_counts, YEAR != "2024")
cor.test(filter$YEAR, filter$n, method = "spearman")

#correlation for gene tests only minus 2022
filter2 = filter(filter, YEAR != "2022")
cor.test(filter2$YEAR, filter2$n, method = "spearman")

#correlation for genref tests only 
grefcount = count(z, YEAR)
gene_only_counts$YEAR <- as.numeric(gene_only_counts$YEAR)
cor.test(gene_only_counts$YEAR, gene_only_counts$n, method = "spearman")


###-------REVISION-----------------------------------------------------------


#select the 'uncertain/unreliable' specialties
#Outpatients’, ‘Surgery’ or ‘Emergency Medicine’ , Unknown
SPEC2
summary(as.factor(SPEC2$SPECIALTY))

spec_tricky = SPEC2 %>% filter(SPECIALTY %in%  c("Surgery","Outpatients","Unknown Specialty", "Emergency Medicine"))
spec_NOtricky = SPEC2 %>% filter(!(SPECIALTY %in%  c("Surgery","Outpatients","Unknown Specialty", "Emergency Medicine")))


#plot of the tests per specialty for only the specialties that have done more than 100 tests
#ALL TESTS
plot_spec_top_2 = SPEC2 %>% filter(n > 100) %>% ggplot(., aes(x = reorder(SPECIALTY,n),y=n))+
  geom_col()+ 
  coord_flip()+ scale_y_log10()+
  labs(y = "Test number (log10)", x = "Specialty")+ 
  theme(axis.text.y = element_text(size = 12))
plot_spec_top_2


#NOT TRICKY to interpret TESTS
plot_spec_top_notricky = spec_NOtricky %>% filter(n > 100) %>% ggplot(., aes(x = reorder(SPECIALTY,n),y=n))+
  geom_col()+ 
  coord_flip()+ scale_y_log10()+
  labs(y = "Test number (log10)", x = "Specialty")+ 
  theme(axis.text.y = element_text(size = 12))
plot_spec_top_notricky

#THE TRICKY to interpret TESTS
plot_spec_tricky = spec_tricky %>% filter(n > 100) %>% ggplot(., aes(x = reorder(SPECIALTY,n),y=n))+
  geom_col()+ 
  coord_flip()+ scale_y_log10()+
  labs(y = "Test number (log10)", x = "Specialty")+ 
  theme(axis.text.y = element_text(size = 12))
plot_spec_tricky

#looking at what is listed in the 'tricky to interpret' tests
gene_test_tricky = gene_test_only %>%  filter(SPECIALTY %in%  c("Surgery","Outpatients","Unknown Specialty", "Emergency Medicine"))
count(gene_test_tricky, SPECIALTY, sort = TRUE)
colnames(gene_test_tricky)
count(gene_test_tricky, Description, sort = TRUE)

###FIGURE NEW FIGURE WITH JUST NOT TRICKY specialties
#figure 4 - new after reveiw
plot_grid(plot_spec_top_notricky, g_spec_top,
          align = "h", axis = "b", rel_heights = c(1.5, 2),#rel_widths = c(1, 2),
          labels = c("A","B"),
          ncol = 1, nrow = 2)

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_pq_test_specialties.png",sep=""), 
       width = 8, height = 12, scale = 1.2, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 4.pdf",sep=""), 
       width = 8, height = 12, scale = 1.2, device='pdf', dpi=600)

#also a supp table of the 'tricky'

plot_spec_tricky = spec_tricky %>% filter(n > 100) %>% ggplot(., aes(x = reorder(SPECIALTY,n),y=n))+
  geom_col()+ 
  coord_flip()+ scale_y_log10()+
  labs(y = "Test number (log10)", x = "Specialty")+ 
  theme(axis.text.y = element_text(size = 12))
plot_spec_tricky

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_pq_tricky_test_specialties.png",sep=""), 
       width = 6, height = 5, scale = 1.2, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Supp figure tricky.pdf",sep=""), 
       width = 6, height = 5, scale = 1.2, device='pdf', dpi=600)


##----REVISION------------------------------------------------

#adding information on the billing categories------------------------------------------

#summary of the billing categories - raw data all tests
summary(as.factor(PQ_data$CATEGORY))
PQ_data %>% count(CATEGORY) %>% arrange(desc(n))

#import data from sarah sep 2025 - data on billing categories
PQ_bill_cat<- read_csv("E:/project_genetics_use/rehan V data/AUSLAB Billing CATEGORIES.csv")
head(PQ_bill_cat)

#adjusting in a new billing_category column - based on reviewer request
summary(as.factor(PQ_bill_cat$type_adj))
#filter to look at NAs
PQ_bill_cat %>% filter(is.na(type_adj))

#filter out na - all blanks
PQ_bill_cat = PQ_bill_cat %>% filter(!is.na(type_adj))


#SC type description as is = 


#join new bill_cats to test list
gene_test_only = left_join(gene_test_only,PQ_bill_cat, join_by (CATEGORY == MNEMONIC) , keep = TRUE)
head(gene_test_only)
colnames(gene_test_only)

summary(as.factor(gene_test_only$`A/C TYPE`))
summary(as.factor(gene_test_only$`A/C TYPE DESCRIPTION`))
summary(as.factor(gene_test_only$type_adj))

#looking at the nAs
gene_test_only %>% filter(is.na(type_adj))

#fill the empty/NA with unknown, the two category codes (for funding source) are not in our AUS LAB categories
gene_test_only <- gene_test_only %>% 
  mutate(type_adj_2 = if_else(is.na(type_adj), "Unknown", type_adj))

summary(as.factor(gene_test_only$type_adj_2))

#count the number of public billed  ##DEV NOT WORK
#sum(gene_test_only$`AC TYPE DESCRIPTION` == "Public Bill")

#graph of the funding category
gene_test_only%>% filter(`A/C TYPE DESCRIPTION` != "NA")%>% 
  ggplot(., aes(x=fct_rev(fct_infreq(`A/C TYPE DESCRIPTION`))))+
  geom_bar()+
  scale_y_log10()+
  labs(x = "Funding type", y = "Number of tests")+
  coord_flip()

##veiwing LUMPED FUNDING CATEGORIES - BASED ON 'TYPE' OF FUNDING
#make same graph but separate by genref
colnames(gene_test_only)
plot_PQ_fund_cat = gene_test_only %>% 
  ggplot(., aes(x=fct_rev(fct_infreq(type_adj_2)), fill = GENREF))+
  geom_bar()+
  scale_y_log10()+
  theme(legend.title = element_blank())+ 
  labs(x = "Funding type", y = "Number of tests")+
  coord_flip()

plot_PQ_fund_cat

##################FINAL FIGURES AND DATA ################################

#Save temp file of clean data table
write.csv(PQ_data,file = paste(gsub(":", "-", Sys.Date()),"PQ_data_clean.csv"), row.names = FALSE)


#figure 3 - version 2 -  
plot_grid(plot_gentest_tl_B, plot_PQ_fund_cat,
          align = "h", axis = "b", rel_widths = c(1, 1.15),
          labels = c("A","B"),
          ncol = 2, nrow = 1)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_pq_testing_summary_v2.png",sep=""), 
       width = 10, height = 4, scale = 1.2, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 3.pdf",sep=""), 
       width = 10, height = 4, scale = 1.2, device='pdf', dpi=600)


###FIGURE QLDauslab specialties testing
#figure 4
plot_grid(plot_spec_top, g_spec_top,
          align = "h", axis = "b", rel_heights = c(1.5, 2),#rel_widths = c(1, 2),
          labels = c("A","B"),
          ncol = 1, nrow = 2)

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_pq_test_specialties.png",sep=""), 
       width = 8, height = 12, scale = 1.2, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript_review response", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 4.pdf",sep=""), 
       width = 8, height = 12, scale = 1.2, device='pdf', dpi=600)

#Save complete file of clean data table - use for later figures


