## Script to analyse Pathology QLD testing data into test types category
### script to translate all tests into a 'test type'
##01.01.2026 
##Authors - Rehan Villani developed from script by Emily Mitchell


#load tidyverse and readxl and patchwork
library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(cowplot)
library(grid)
library(Hmisc)
library(RColorBrewer)
library(readxl)
library(stats)
library(broom)

#set dir
rm(list = ls())
#setwd("")
date = strftime(Sys.Date(),"%y%m%d")

##generated data table with Rscript in previous script
test_cat = PQ_data
colnames(PQ_data)
summary(PQ_data)

#summary of the columns that have information regarding test type
summary(as.factor(PQ_data$ALLTESTS))
summary(as.factor(PQ_data$REFTST))

cols_to_factor = c("YEAR")
PQ_data[cols_to_factor] <- lapply(PQ_data[cols_to_factor], factor)

#convert the info columns into test category columns on the temp data table

#put in 'cat temp' the test category based on the refst category
test_cat$cat_temp = test_cat$REFTST
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, "WES")|str_detect(REFTST, regex("ngswes ",ignore_case = TRUE))|str_detect(ALLTESTS, "WES")|str_detect(REFTST, regex("trio",ignore_case = TRUE))|str_detect(REFTST, regex("exome",ignore_case = TRUE)),"WES", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, "WGS")|str_detect(REFTST, regex("genomic ",ignore_case = TRUE))|str_detect(REFTST, regex("genome ",ignore_case = TRUE))|str_detect(Description, "WGS"),"WGS", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("panel",ignore_case = TRUE)),"NGS Panel", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("array",ignore_case = TRUE))|str_detect(ALLTESTS, "SNP")|str_detect(REFTST, regex("snp",ignore_case = TRUE))|str_detect(REFTST, regex("acgh",ignore_case = TRUE)),"Array", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("sanger",ignore_case = TRUE))|str_detect(ALLTESTS, "SANGER"),"Sanger", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("pcr",ignore_case = TRUE)),"PCR", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("karyo",ignore_case = TRUE))|str_detect(ALLTESTS, "CHR")&str_detect(ALLTESTS, "CHRF", negate = TRUE),"Karyotype", .))
test_cat <- test_cat %>% mutate_at(vars(c(cat_temp)), ~if_else (str_detect(REFTST, regex("fish",ignore_case = TRUE))|str_detect(ALLTESTS, "CHRF"),"FISH", .))

#colnames(gene_test_only)
colnames(test_cat)

#categorise gen ref 

test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("array",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("acgh",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("snp",ignore_case = TRUE)), "Array",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("wes",ignore_case = TRUE))& str_detect(test_cat$REFTST, regex("west", ignore_case = TRUE),negate = TRUE)|str_detect(test_cat$REFTST,regex("NGSWES"))|str_detect(test_cat$REFTST, regex("exome", ignore_case = TRUE))|str_detect(test_cat$REFTST, regex("trio wes", ignore_case = TRUE)), "WES",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("panel",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("brca plus",ignore_case = TRUE)), "Panel",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("pcr",ignore_case = TRUE)), "PCR",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("predict",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("familial var",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("confirm",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("segre",ignore_case = TRUE))|str_detect(test_cat$REFTST, "c\\."), "Single variant testing",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("wgs",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("genome",ignore_case = TRUE)), "WGS",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("methyl",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("methl",ignore_case = TRUE)), "Methylation Studies",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("next generation",ignore_case = TRUE)), "NGS",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("fish",ignore_case = TRUE)), "FISH",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("cytoge",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("brcab",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("chrt",ignore_case = TRUE))|str_detect(test_cat$REFTST,regex("chrb",ignore_case = TRUE)), "Cytogenetics",test_cat$REFTST)
test_cat$test_cat = ifelse(str_detect(test_cat$REFTST,regex("single gene",ignore_case = TRUE)), "Single Gene",test_cat$REFTST)

summary(as.factor(test_cat$cat_temp))

#categorise genref into test purpose 
test_cat$PURPOSE = test_cat$REFTST
test_cat$PURPOSE = ifelse(str_detect(test_cat$PURPOSE,regex("monitoring",ignore_case = TRUE)), "Therapy",test_cat$PURPOSE)
test_cat$PURPOSE = ifelse(str_detect(test_cat$PURPOSE,regex("segregation",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("trio",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("exome",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("familial hyp",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("snp",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("panel",ignore_case = TRUE))|
                     str_detect(test_cat$PURPOSE,regex("acgh",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("brca plus",ignore_case = TRUE)), "Diagnostic", test_cat$PURPOSE)
test_cat$PURPOSE = ifelse(str_detect(test_cat$PURPOSE,regex("carrier",ignore_case = TRUE)), "Carrier",test_cat$PURPOSE)
test_cat$PURPOSE = ifelse(str_detect(test_cat$PURPOSE,regex("pre-natal",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("prenatal",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("pre n",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("pre -",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("pregen",ignore_case = TRUE)), "Prenatal", test_cat$PURPOSE)
test_cat$PURPOSE = ifelse(str_detect(test_cat$PURPOSE,regex("predictive",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("familial",ignore_case = TRUE))&str_detect(test_cat$PURPOSE,regex("mutation",ignore_case = TRUE))|str_detect(test_cat$PURPOSE,regex("familial",ignore_case = TRUE))&str_detect(test_cat$PURPOSE,regex("variant",ignore_case = TRUE)), "Predictive",test_cat$PURPOSE)

##import test list table 
testlist_table <- read_excel("data/Genomics test list_categories.xlsx")
summary(testlist_table)
summary(as.factor(testlist_table$`Test Category`))

colnames(testlist_table)
summary(as.factor(testlist_table$QHPS_test_code))
colnames(test_cat)
head(test_cat)
summary(as.factor(test_cat$ALLTESTS))

#add in a Sanger specific column to the testlist_table

Sanger_ext_terms = c("Sanger","predctive","mutation","predic")
testlist_table$sanger_ext = as.factor(ifelse(str_detect(testlist_table$Name, regex(paste(unlist(Sanger_ext_terms), collapse = "|"), ignore_case = TRUE)),
                                             "Sanger",
                                             "not_determined"))

summary(testlist_table$sanger_ext)
          
#Create a column per 'test type' and then indicate which are, are not, or 'not_determined'
str(PQ_data)
table(PQ_data$GENREF,PQ_data$REFTST)

###extracting out columns indicating if the test row is in a broad test type category-----------------------
####creating column on ALL of the data
#WGS - column indicating if the test item includes WGS
#make a list of the terms that refer to a WGS test
wgs_terms = testlist_table %>% filter(`Test Category` == "WGS") %>% pull(QHPS_test_code) %>% list()
wgs_terms

str(PQ_data)

#WGS
summary(as.factor(PQ_data$ALLTESTS))
PQ_data$ALLTESTS %in% "NGSWGS" %>% summary()
PQ_data$ALLTESTS %in% wgs_terms %>% summary()
str_detect(PQ_data$ALLTESTS,regex("NGSWGS",ignore_case = TRUE))%>% summary()
str_detect(PQ_data$ALLTESTS,regex("VCWGS",ignore_case = TRUE))%>% summary()
str_detect(PQ_data$ALLTESTS,regex("WGSURG",ignore_case = TRUE))%>% summary()
str_detect(PQ_data$REFTST,regex("wgs",ignore_case = TRUE))%>% summary()

str_detect(PQ_data$ALLTESTS, regex(paste(unlist(wgs_terms), collapse = "|"), ignore_case = TRUE)) %>% summary()

PQ_data$test_wgs = as.factor(ifelse(str_detect(PQ_data$REFTST,regex("wgs",ignore_case = TRUE))|
                                      str_detect(PQ_data$REFTST,regex("genome",ignore_case = TRUE))|
                                      str_detect(PQ_data$ALLTESTS, regex(paste(unlist(wgs_terms), collapse = "|"), ignore_case = TRUE)),
                                    "WGS",
                                    "not_determined"))
summary(PQ_data$test_wgs)

#WES - column indicating if the test item includes WES
wes_terms = testlist_table %>% filter(`Test Category` == "WES") %>% pull(QHPS_test_code) %>% list()
PQ_data$test_wes = as.factor(ifelse(str_detect(PQ_data$REFTST,regex("wes",ignore_case = TRUE))| 
                                      str_detect(PQ_data$REFTST, regex("west", ignore_case = TRUE))|
                                      str_detect(PQ_data$REFTST,regex("NGSWES", ignore_case = TRUE))|
                                      str_detect(PQ_data$REFTST, regex("exome", ignore_case = TRUE))|
                                      str_detect(PQ_data$REFTST, regex("trio wes", ignore_case = TRUE))|
                                      str_detect(PQ_data$ALLTESTS, regex(paste(unlist(wes_terms), collapse = "|"), ignore_case = TRUE)),
                                    "WES","not_determined"))

summary(PQ_data$test_wes)

#Panel - column indicating if the test item includes Panel
panel_terms = testlist_table %>% filter(`Test Category` == "Panel") %>% pull(QHPS_test_code) %>% list()
PQ_data$test_panel = as.factor(ifelse(str_detect(PQ_data$REFTST,regex("panel",ignore_case = TRUE))|
                                        str_detect(PQ_data$REFTST,regex("brca plus",ignore_case = TRUE))| 
                                        str_detect(PQ_data$REFTST,regex("Comprehensive",ignore_case = TRUE)) |
                                        str_detect(PQ_data$ALLTESTS, regex(paste(unlist(panel_terms), collapse = "|"), ignore_case = TRUE)),
                                      "panel","not_determined"))
summary(PQ_data$test_panel)

panel_remainder = PQ_data %>% filter(test_panel == "not_determined") 
head(panel_remainder)
panel_remainder %>%  count

#Sanger - by adding column indicating if the test item includes Sanger
#extended has been extended for additional PQ codes -PT, PD, GS - predictive, prenatal and genescreen
sanger_terms = testlist_table %>% filter(`Test Category` == "Sanger") %>% pull(QHPS_test_code) %>% list()
PQ_data$test_sanger_ext = as.factor(ifelse(str_detect(PQ_data$ALLTESTS,regex("sanger",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("sanger",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("variant",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("mutation",ignore_case = TRUE))|
                                             str_detect(PQ_data$ALLTESTS, regex(paste(unlist(sanger_terms), collapse = "|"), ignore_case = TRUE))|                        str_detect(PQ_data$ALLTESTS,regex("PT",ignore_case = TRUE))|
                                             str_detect(PQ_data$ALLTESTS,regex("PT",ignore_case = TRUE))|
                                             str_detect(PQ_data$ALLTESTS,regex("PD",ignore_case = TRUE))|
                                             str_detect(PQ_data$ALLTESTS,regex("GS",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("PT",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("PD",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("GS",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("predictive",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("prenatal",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex("genescreen",ignore_case = TRUE))|
                                             str_detect(PQ_data$REFTST,regex(snggene_terms_add,ignore_case = TRUE)), 
                                           "sanger_ext","not_determined"))
summary(PQ_data$test_sanger_ext)

sanger_remainder = PQ_data %>% filter(test_sanger_ext != "Sanger") 
head(sanger_remainder)
sanger_remainder %>%  count(REFTST)

#PCR column
pcr_terms = testlist_table %>% filter(grepl("pcr", `Test Category`, ignore.case = TRUE)) %>% pull(QHPS_test_code) %>% list()
PQ_data$test_pcr = as.factor(ifelse(str_detect(PQ_data$REFTST,regex("pcr",ignore_case = TRUE))|
                                      str_detect(PQ_data$REFTST,regex("qpcr",ignore_case = TRUE))|
                                      str_detect(PQ_data$ALLTESTS, regex(paste(unlist(pcr_terms), collapse = "|"), ignore_case = TRUE)),
                                    "PCR",
                                    "not_determined"))
summary(PQ_data$test_pcr)


#Karyotype
kary_terms = testlist_table %>% filter(grepl("Karyotype", `Test Category`, ignore.case = TRUE)) %>% pull(QHPS_test_code) %>% list()
PQ_data$test_karyotype = as.factor(ifelse(str_detect(test_cat$REFTST,regex("karyotype",ignore_case = TRUE))|
                                            str_detect(PQ_data$ALLTESTS, regex(paste(unlist(kary_terms), collapse = "|"), ignore_case = TRUE)),
                                          "Karyotype","not_determined"))
summary(PQ_data$test_karyotype)

#Array - column indicating if the test item includes Array
array_terms = testlist_table %>% filter(`Test Category` == "Array") %>% pull(QHPS_test_code) %>% list()
PQ_data$test_array = as.factor(ifelse(str_detect(PQ_data$REFTST,regex("array",ignore_case = TRUE))|
                                        str_detect(PQ_data$REFTST,regex("acgh",ignore_case = TRUE))|
                                        str_detect(PQ_data$REFTST,regex("snp",ignore_case = TRUE))|
                                        str_detect(PQ_data$ALLTESTS, regex(paste(unlist(array_terms), collapse = "|"), ignore_case = TRUE)),
                                      "Array","not_determined"))
summary(PQ_data$test_array)

#FISH
fish_terms = testlist_table %>% filter(grepl("fish", `Test Category`, ignore.case = TRUE)) %>% pull(QHPS_test_code) %>% list()
PQ_data$test_fish = as.factor(ifelse(str_detect(test_cat$REFTST,regex("fish",ignore_case = TRUE))|
                                       str_detect(PQ_data$ALLTESTS, regex(paste(unlist(fish_terms), collapse = "|"), ignore_case = TRUE)),
                                     "FISH","not_determined"))
summary(PQ_data$test_fish)



#create a column that collates all

PQ_data$testtype_summary = as.factor(case_when(PQ_data$test_wgs == "WGS" ~ "WGS",
                                               PQ_data$test_wes == "WES" ~ "WES",
                                               PQ_data$test_panel == "panel" ~ "panel",
                                               PQ_data$test_array == "Array" ~ "Array",
                                               PQ_data$test_sanger_ext == "Sanger_ext" ~ "Sanger",
                                               PQ_data$test_pcr == "PCR" ~ "PCR",
                                               PQ_data$test_karyotype == "Karyotype" ~ "Karyotype",
                                               PQ_data$test_fish == "FISH" ~ "FISH"
                                     ))

summary(PQ_data$testtype_summary)

snglgene_remainder = PQ_data %>% filter(test_snggene == "not_determined") 
head(snglgene_remainder)
snglgene_remainder %>%  count(REFTST)

table(PQ_data$test_snggene, PQ_data$test_wes)

#indicating if the row is likely research
res_terms = testlist_table %>% filter(`Test Category` == "pcr") %>% pull(QHPS_test_code) %>% list()
PQ_data$test_res = as.factor(ifelse(str_detect(test_cat$REFTST,regex("research",ignore_case = TRUE))|
                                      str_detect(test_cat$RSTAT,regex("research",ignore_case = TRUE)), 
                                    "Research","not_determined"))
summary(PQ_data$test_res)

#####generation of plots -----------------------------------------
#plot for wgs
summary(PQ_data$test_wgs)

plot_wgs = PQ_data %>% filter(test_wgs == "WGS") %>%
  count(test_wgs, YEAR) %>%
  ggplot(., aes(YEAR, n, fill = YEAR)) + 
  geom_col()+
  labs(title = "WGS", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_wgs


#plot WES
summary(PQ_data$test_wes)
plot_wes = PQ_data %>% filter(test_wes == "WES") %>%
  count(test_wes, YEAR) %>%
  ggplot(., aes(YEAR, n, fill = YEAR)) + 
  geom_col()+
  labs(title = "WES", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_wes


#plot panel tests
summary(PQ_data$test_panel)
plot_panel = PQ_data %>% filter(test_panel == "panel") %>%
  count(test_panel, YEAR) %>%
  ggplot(., aes(YEAR, n,fill = YEAR)) + 
  geom_col()+
  labs(title = "Panel", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
  
plot_panel


#plot sanger - extended with all codes as per PQ

plot_sanger_ext =PQ_data %>% filter(test_sanger_ext == "sanger_ext") %>%
  count(test_snggene, YEAR) %>%
  ggplot(., aes(YEAR, n,fill = YEAR)) + 
  geom_col()+
  labs(title = "Sanger", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_sanger_ext

#plot array
summary(PQ_data$test_array)
plot_array = PQ_data %>% filter(test_array == "Array") %>%
  count(test_array, YEAR) %>%
  ggplot(., aes(YEAR, n, fill = YEAR)) + 
  geom_col()+
  labs(title = "Array", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_array

#plot PCR
summary(PQ_data$test_pcr)
plot_pcr =PQ_data %>% filter(test_pcr == "PCR") %>%
  count(test_pcr, YEAR) %>%
  ggplot(., aes(YEAR, n, fill = YEAR)) + 
  geom_col()+
  labs(title = "PCR", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_pcr


#plot FISH
summary(PQ_data$test_fish)
plot_fish =PQ_data %>% filter(test_fish == "FISH") %>%
  count(test_fish, YEAR) %>%
  ggplot(., aes(YEAR, n, fill = YEAR)) + 
  geom_col()+
  labs(title = "FISH", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_fish


#plot Karyotype
summary(PQ_data$test_karyotype)

plot_karyotype =PQ_data %>% filter(test_karyotype == "Karyotype") %>%
  count(test_karyotype, YEAR) %>%
  ggplot(., aes(YEAR, n,fill = YEAR)) + 
  geom_col()+
  labs(title = "Karyotype", x = "YEAR", y = "Number of tests")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_grey(start=0.7, end=0.2)+
  theme(legend.position = "none")
plot_karyotype

##### filtering THE CATEGORIES input data so that it is 'gene tests and not 'all tests'-----------------------------
colnames(PQ_data)
PQ_data %>% filter(TEST == "Genetic test") %>% count()
PQ_data %>% filter(str_detect(PQ_data$ALLTESTS,regex("refer",ignore_case = TRUE)) == TRUE) %>% count
PQ_data %>% filter(str_detect(PQ_data$ALLTESTS,regex("genref",ignore_case = TRUE)) == TRUE) %>% count()

PQ_data %>% filter(str_detect(PQ_data$REFTST,regex("refer",ignore_case = TRUE)) == TRUE) %>% count()

##now to look at the categories --------------------------------------------------------
head(PQ_data)
summary(as.factor(PQ_data$TEST))

###summary overall
colnames(PQ_data)
count(PQ_data, REFTST, sort = TRUE) %>% print(n = 20)
count(PQ_data, TESTTYPE_gene, sort = TRUE) %>% print(n = 20)

alltest_only_external_cat = PQ_data %>% filter(GENREF == "External")
gene_test_only_external_cat = gene_test_only %>% filter(GENREF == "External")

colnames(gene_test_only_external_cat)

gene_test_only_external_cat <- gene_test_only_external_cat %>%
  mutate(across(where(is.character), as.factor))
summary(gene_test_only_external_cat)
colnames(gene_test_only_external_cat)

table(gene_test_only_external_cat$GENREF,gene_test_only_external_cat$test_array)

gene_test_only_external_cat %>% select(GENREF,26:38) %>% group_by(GENREF) %>% summary()

#now trying to look at ALL the gen tests, not just the external ones
#filter the PQ data for jsut gene test only tests 
summary(as.factor(PQ_data$TEST))
gene_test_only_3 = PQ_data %>% filter(TEST == "Genetic test")

gene_test_only_3 = gene_test_only_3 %>%  mutate(across(where(is.character), as.factor))

summary(gene_test_only_3$GENREF)

table(gene_test_only_3$GENREF,gene_test_only_3$test_array)

gene_test_only_3 %>% group_by(GENREF) %>% count(test_wgs)
gene_test_only_3 %>% group_by(GENREF) %>% count(test_pcr)
gene_test_only_3 %>% group_by(GENREF) %>% count(test_sanger_ext)
gene_test_only_3 %>% group_by(GENREF) %>% count(test_array)
gene_test_only_3 %>% group_by(GENREF) %>% count(test_karyotype)
gene_test_only_3 %>% group_by(GENREF) %>% count(test_fish)

#look closer at the GENREF category
gene_test_only_3 %>% group_by(GENREF)

count(gene_test_only_3, REFTST, sort = TRUE) %>% print(n = 20)
count(gene_test_only_3, TESTTYPE_gene, sort = TRUE) %>% print(n = 20)

##assessing the non-categorised
summary(gene_test_only_3)
gene_test_only_3 %>% filter(REFTST == " ") %>% count()

##---------------------------------------------------------------------
#figure 5 - plot gene tests per year for each test category
#potentialy test categories - plot's not included - research, somatic, single gene

colnames(gene_test_only_3)
plot_grid(plot_wgs, plot_wes,plot_panel,
          plot_array, plot_fish,plot_pcr,
          plot_sanger_ext, 
          plot_karyotype, 
          align = "h", axis = "b", rel_widths = c(1, 1),
          labels = c("A","B","C","D","E","F","G","H"),
          ncol = 2, nrow = 4)


ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_pq_testcategories_wGENREF.png",sep=""), 
       width = 5, height = 7, scale = 1.2, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 5.pdf",sep=""), 
       width = 5, height = 7, scale = 1.2, device='pdf', dpi=600)

