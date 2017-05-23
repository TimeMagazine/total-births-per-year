if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# load data
ssa <- read.csv("data/ssa.csv")

# make sure FIPS codes keep their leading zeroes...
cdc <- read.csv("data/cdc.csv", 
  colClasses = c("integer", "character", "character", "character", "integer")                      
)

# first, let's double-check that the states add up to the totals in the CDC data
cdc_us <- cdc[cdc$state=="United States",]
cdc_states <- aggregate(births ~ year, data=cdc[cdc$gender != "total",], FUN=sum)
cdc_compare <- merge(cdc_us, cdc_states, by="year")

# they match! You can also verify that these are the same totals as extracted in 
# 'sources/Natality, 2007-2015, national.txt'
# let's keep a clean global environment, eh?
rm(cdc_us, cdc_states, cdc_compare)

# Now let's compare to the SSA data. We no longer need those national totals
cdc = cdc[cdc$state != "United States",]
cdc_by_gender <- aggregate(births ~ year + gender, data=cdc, FUN=sum)

# and cast it to compare to SSA
cdc_by_gender <- setNames(
  dcast(cdc_by_gender, year ~ gender, value.var = "births"),
  c("year", "female_cdc", "male_cdc", "total_cdc")
)

colnames(ssa) <- c("year", "female_ssa", "male_ssa", "total_ssa")

ssa_cdc <- merge(ssa, cdc_by_gender, by="year")

ssa_cdc$female_percent <- ssa_cdc$female_ssa / ssa_cdc$female_cdc
ssa_cdc$male_percent <- ssa_cdc$male_ssa / ssa_cdc$male_cdc
ssa_cdc$total_percent <- ssa_cdc$total_ssa / ssa_cdc$total_cdc

# curious! While the total figures almost precisely match,
# the CDC reports fewer female babies and more male babies

ggplot(ssa_cdc, aes(year)) + 
  geom_line(aes(y = total_cdc, color = "CDC")) + 
  geom_line(aes(y = total_ssa, color = "SSA")) +
  expand_limits(y=0) +
  scale_colour_manual('Source:', values = c(CDC='green',SSA='orange')) +
  ggtitle("Total Births") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ssa_cdc, aes(year)) + 
  geom_line(aes(y = female_cdc, color = "CDC")) + 
  geom_line(aes(y = female_ssa, color = "SSA")) +
  expand_limits(y=0) +
  scale_colour_manual('Source:', values = c(CDC='green',SSA='orange')) +
  ggtitle("Total Births: Female") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(ssa_cdc, aes(year)) + 
  geom_line(aes(y = male_cdc, color = "CDC")) + 
  geom_line(aes(y = male_ssa, color = "SSA")) +
  expand_limits(y=0) +
  scale_colour_manual('Source:', values = c(CDC='green',SSA='orange')) +
  ggtitle("Total Births: Male") +
  theme(plot.title = element_text(hjust = 0.5))
