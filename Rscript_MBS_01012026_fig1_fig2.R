##Script to analyse MBS test items number
##01.07.2026 - v4
##Authors - Emily Mitchell and Rehan Villani


#load tidyverse and readxl and patchwork
library(tidyverse)
library(readxl)
library(patchwork)
library(ggpubr)
library(cowplot)
library(grid)
library(Hmisc)

#set dir
rm(list = ls())
#setwd('')

date = strftime(Sys.Date(),"%y%m%d")

##import dependent data - collected from MBS webpage. 
#import MBS item availability data
available_item_numbers <- read_excel("data/MBS_avitemnumbers.xlsx", 
                                            sheet = "av_item_nos")
available_item_numbers <- rename(available_item_numbers, "Available" = "number of item numbers")

#import MBS download data
#data was downloaded on 
MBS_download <- read_excel("data/MBS download.xlsx", 
                           sheet = "All data", skip = 3)

###The items that are listed on MBS

#number of item numbers each year
item_number = count(MBS_download, Year)
item_number_year = filter(item_number,Year!="Total")
item_number_year[item_number_year=="YTD 2024"]<- "2024"
item_number_year <- rename(item_number_year, "Used" = "n")
item_numbers = merge(x=item_number_year, y=available_item_numbers, by.x = "Year", by.y = "year")
item_numbers_gathered = gather(item_numbers, "availability", "n", 2:3)

#plot of MBS items used vs available per year
plot_1 <- ggplot(filter(item_numbers_gathered, Year != "2024"), mapping = aes(fill = availability, x = Year, y = n)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(y = "MBS Item Numbers", fill = NULL) +
  theme(legend.position = "bottom")+
  scale_fill_grey(start=0.7, end=0.2)
plot_1

#create pie chart used vs. unused
used_v_unused = data.frame(group = c("Used", "Unused"), value=c(128, 30))

###The items that are being used by clinical service, those on MBS record

#number of tests/services each year 
test_number = count(MBS_download, Year, wt = Total)
test_number_year = filter(test_number,Year!="Total")
test_number_year[test_number_year=="YTD 2024"]<- "2024"
test_number_year = add_row(test_number_year, Year= "2024", n = 435515)
test_number_year = add_column(test_number_year, proj = c('actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'projected'))
test_number_year_numeric = as.numeric(test_number_year$Year)

#plot of number of services
#graphs of 2017 - 2023
no_2024 = filter(test_number_year, Year!="2024")

#plot of number of services / year with trend line - not 2024

plot_tspy = test_number_year %>% filter(., Year!="2024")%>%
  ggplot(., aes(fill = Year, x=Year, y = n)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  labs (y = "Total Services", fill = NULL)+
  theme_bw() + theme(legend.position = "null", legend.title = element_blank())+ 
  scale_fill_grey(start=0.7, end=0.2)+
  geom_smooth(method=lm, data=no_2024, aes(x = Year, y= n, group = proj), se=FALSE, linewidth=1, colour = "black") 
plot_tspy

#save plot
ggsave(path = "/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"testperyr_total.png",sep=""), 
       height = 4, width = 6, scale = 1.5, device='png', dpi=600)


##state by state analysis--------------------------

#MBS tests gathered by state
MBS_gathered = gather(MBS_download, key = "State", value = "n", 3:11)
MBS_gathered_years = filter(MBS_gathered, Year != "Total", State != "Total")

#drop item numbers
MBS_gathered_years$`Item Number` <- NULL 
MBS_gathered_yearstest = MBS_gathered_years %>% group_by(., State,Year) %>% summarise(n = sum(n))

#exclude 2024 - incomplete
remove_2024_total = filter(MBS_gathered_yearstest, Year != "YTD 2024")

#number of tests QLD
qld_test_number = count(MBS_download, Year, wt = QLD)
qld_test_number_year = filter(qld_test_number,Year!="Total")
qld_test_number_year[qld_test_number_year=="YTD 2024"]<- "2024"
qld_test_number_year = add_row(qld_test_number_year, Year= "2024", n = 101380)
qld_test_number_year = add_column(qld_test_number_year, proj = c('actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'actual', 'projected'))

#what is the total number of tests performed in QLD for the time period 2017-2023
qld_test_number_year %>% filter(Year != "2024") %>% summarise(total_sum = sum(n))

##analysis of the money value of MBS testing------------

#benefits paid each year 
MBS_funding<- read.delim("data/MBS funding summary.txt", sep = "\t")
MBS_funds_gathered = gather(MBS_funding, key = "State", value = "amount", 3:11)

#remove total for each year
MBS_funds_gathered = filter(MBS_funds_gathered, State!= "Total", Year != "Total")

#plot the amount claimed per year
ggplot(MBS_funds_gathered, aes( x = Year, y = amount )) + 
  geom_bar(stat = "identity", fill = "#F8766D", position = "dodge") + 
  scale_y_continuous(labels = scales::comma_format()) + 
  labs (y = "Total Amount Claimed ($)")

#select only QLD funds
qld_funds = filter(MBS_funds_gathered, State =="QLD")

#plot only QLD funds
ggplot(qld_funds, aes(fill=State, x = Year, y = amount )) +
  geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::comma_format()) +
  labs (y = "Total Amount Claimed ($)", fill = NULL)

total_funds = filter(MBS_funds_gathered, State =="Total")

#amt per year per state, separate line
plot_mbsnys = ggplot(MBS_funds_gathered, aes(fill=State, x=Year, y=amount, colour = State)) + 
    geom_point() + scale_y_continuous(labels = scales::comma_format())+
    geom_smooth(method = lm, se=FALSE,aes(x = Year, y= amount, group = State)) + 
    theme_bw()+ labs(y = "Total Services") + 
    theme(legend.position = "none")
plot_mbsnys
  
#MBS item number referral reasons/test purposes
MBS_2 <- read_excel("data/MBS 2.xlsx", sheet = "item_purpose_all")
MBS_purpose <- read_excel("data/MBS 2.xlsx", sheet = "item_purpose")
MBS_purpose_count = count(MBS_purpose, Cat)
head(MBS_purpose)

##-----------------

#services per item number
services_each_number = count(MBS_gathered, MBS_gathered$`Item Number`, wt = n)
number_cat_amount = merge(x = MBS_purpose, y = services_each_number, by.x = "Item Number", by.y = "MBS_gathered$`Item Number`", all = TRUE)

p3 = ggplot(number_cat_amount, aes(x = Cat, y = n, fill = Cat)) +
  geom_bar(stat = "identity") + theme_bw() + theme(legend.position = "none")+ 
  labs(x = "Category", y = "Total Services")+ 
  scale_y_continuous(labels = scales::comma_format()) 
p3

#categories of testing purpose - attached to MBS_download 
joined = left_join(MBS_purpose, MBS_download, by = join_by(`Item Number`), relationship = "many-to-many")
grouped = group_by(joined, Year, Cat) %>% summarise(n = sum(Total))

p4 = grouped %>% filter(, Year!= "Total", Year!= 'NA', Year != "YTD 2024") %>%
  ggplot(., aes(x = Year, y = n, fill = Cat, color = Cat))+ 
  geom_point(stat = "identity", size = 3)+ scale_y_continuous(labels = scales::comma_format())+ 
  geom_smooth(data = filter(grouped, Year!= "Total", Year!= 'NA', Year != "YTD 2024"), method = lm, aes(group = Cat), se=FALSE) + 
  labs(y = "Total Services")
p4

#normalise data to population
population <- read_excel("data/Australia_state_population_310104_sheet2.xlsx", sheet = "pop")
population <- rename(population, "Year" = "...1")
population_gathered = gather(population, key = "State", value = 'population', 2:10)
pop_states = filter(population_gathered, State!= "Australia")
pop_states <- pop_states %>% mutate(Year = as.character(Year))
states_year_test = filter(MBS_gathered_yearstest, Year != "YTD 2024")

#add population rows to n tests
pop_added = left_join(states_year_test, pop_states, relationship = "many-to-many")
n_normalised = add_column(pop_added, pop_added$n/pop_added$population)
pop_added$n_normalised = as.numeric(pop_added$n/pop_added$population)

#add amount/cost of serivces
pop_added = left_join(pop_added, MBS_funds_gathered, join_by(State, Year))
pop_added$amt_normalised = as.numeric(pop_added$amount/pop_added$population)

#number per year per state, separate line
plot_nyr_line = pop_added %>% ggplot(., aes(fill=State, x=Year, y=n, colour = State)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma_format())+
  geom_smooth(method = lm, se=FALSE,aes(x = Year, y= n, group = State)) + 
  theme_bw()+ labs(y = "Total Services")+
  theme(legend.position = "none")
plot_nyr_line

#number per year per state normailsed to the population size
plot_npy_norm = pop_added %>% ggplot(., aes(fill=State, x=Year, y=n_normalised, colour = State)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma_format())+
  geom_smooth(method = lm, se=FALSE,aes(x = Year, y= n_normalised, group = State)) + 
  theme_bw()+ labs(y = "Total Services (normalised)")
plot_npy_norm

#amount per year per state, separate line
plot_mbsnys_line = pop_added %>% ggplot(., aes(fill=State, x=Year, y=amount, colour = State)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma_format())+
  geom_smooth(method = lm, se=FALSE,aes(x = Year, y= amount, group = State)) + 
  theme_bw()+ labs(y = "Amount paid")+
  theme(legend.position = "none")
plot_mbsnys_line

#number per year per state normailsed to the population size
plot_mbsnys_norm = pop_added %>% ggplot(., aes(fill=State, x=Year, y=amt_normalised, colour = State)) + 
  geom_point() + 
  scale_y_continuous(labels = scales::comma_format())+
  geom_smooth(method = lm, se=FALSE,aes(x = Year, y= amt_normalised, group = State)) + 
  theme_bw()+ labs(y = "Amount paid (normalised)")
plot_mbsnys_norm

#statistics for normalised 
s = select(pop_added, State, Year, n_normalised)
t = spread(s, State, 'pop_added$n/pop_added$population') 

t = pivot_wider(s, names_from = State, values_from = n_normalised)
t$Year <- as.numeric(t$Year)
stats1 <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(stats1) <- c("var1", "var2", "correlation", "pvalue")

# Correlation in loop
for(i in colnames(t[,1])) {
  for(j in colnames(t[,2:9])) {
    a <- cor.test(t[[i]], t[[j]], method = "spearman")
    stats1 <- rbind(stats1, data.frame(
      "var1" = i,
      "var2" = j,
      "correlation" = a$estimate,
      "pvalue" = a$p.value) )
  }
}

stats1

# Remove rownames
rownames(stats1) <- NULL
write.csv(stats1, "results/stats1_corr_normalised.csv", row.names = F)

#statistics for total  
s2 = select(normalised, State, Year, n)
t2 = spread(s2, State, n)
t2$Year <- as.numeric(t2$Year)
stats2 <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(stats2) <- c("var1", "var2", "correlation", "pvalue")

# Correlation in loop
for(i in colnames(t2[,1])) {
  for(j in colnames(t2[,2:9])) {
    a <- cor.test(t2[[i]], t2[[j]], method = "spearman")
    stats2 <- rbind(stats2, data.frame(
      "var1" = i,
      "var2" = j,
      "correlation" = a$estimate,
      "pvalue" = a$p.value) )
  }
}

# Remove rownames
rownames(stats2) <- NULL
write.csv(stats2, "results/stats2_corr_total.csv", row.names = F)

##figures ----------------------------------

#Figure 1 - total MBS item number and category of test
plot_grid(plot_tspy, plot_1, p4,
          align = "h", axis = "b", rel_widths = c(1, 1,1.2),
          labels = c("A","B","C"),
          ncol = 3, nrow = 1)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_npy_services_all.png",sep=""), 
       width = 10, height = 4, scale = 1.5, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 1.pdf",sep=""), 
       width = 10, height = 4, scale = 1.5, device='pdf', dpi=600)

#figure 2 - services and amount paid normalised to the state population
plot_grid((plot_npy_norm + 
             theme(legend.position = "none")), plot_mbsnys_norm,
          align = "h", axis = "b", rel_widths = c(1, 1.15),
          labels = c("A","B"),
          ncol = 2, nrow = 1)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_num_amt_normtopop.png",sep=""), 
       width = 6, scale = 1.5, device='png', dpi=600)

ggsave(path = "E:/project_genetics_use/manuscript/Submission_01012026/results", 
       filename = paste(gsub(":", "-", Sys.Date()),"_Figure 2.pdf",sep=""), 
       width = 8, scale = 1.5, device='pdf', dpi=600)
