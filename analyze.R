library(tidyverse)
library(openxlsx)

base_dir <- "### Path to project directory (with ending forward slash) ###"

# input dir must contain two csv files
input_dir <- paste0(base_dir, "input/")

# Excel file will be saved to base_dir
# Graphs will be saved to graphs_dir

graphs_dir <- paste0(base_dir, "graphs/")

# if graphs_dir doesn't exist, create it
if(!file.exists(graphs_dir)) dir.create(file.path(graphs_dir))

### Read data in

# sessionCounts dataset will be called "s"
# addToCart dataset will be called "a"

s <- read_csv(paste0(input_dir, "DataAnalyst_Ecom_data_sessionCounts.csv"))
a <- read_csv(paste0(input_dir, "DataAnalyst_Ecom_data_addsToCart.csv"))

## Transforming sessionCounts data

# first, we observe that exact dates are given. we want to make a new variable
# rounding down to the first of the month

# use lubridate "floor_date" and "mdy"
s <- s %>% mutate(month = floor_date(mdy(dim_date), unit = "month"))

# tabulate dim_date and new month variable to make sure it's correct

# table(s$dim_date, s$month)

### Data Exploration

# Now we'll explore the data, doing "sanity checks" and looking 
# for potential errors that need to be dropped or categories that need to be
# modified

## First, let's look at the browsers and device categories in a 2-way table

table(s$dim_browser, s$dim_deviceCategory, useNA = 'always')

# We see there are potentially some items to address with the browser variable,
# such as:

# 1) should "Safari" and "Safari (in-app)" be considered the same browser? Same 
# with "Android Browser" and "Android Webview"

# 2) There are several error entries. For example, I see "(not set)",
# "error", "anonymous". Should these be combined or dropped?

# 3) Other entries that don't look like names of browsers. For example,
# "Blackberry", "Playstation 3", "DESKTOP", Nokia and LG device names, "Python-urllib", etc.
# Should these be dropped, renamed, or put into a separate category?

# 4) There are other oddities, like "Job Search" (1 entry), "Mobile", (1 desktop entry)
# How should these be handled?

# 5) Should we only keep the top 5 or 10 most popular browsers? Does it make sense
# to consider these unpopular browsers with few entries? 

# The best answers to these questions will depend on the client's goals and 
# our knowledge about the data source. Here, the client is just looking to understand
# all website performance, and we additionally have no knowledge of the data 
# source. For these reasons, we will not make any modifications to the browser variable.

# Quick sanity check. There should not be any transactions when
# sessions is zero.

s[s$sessions == 0 & s$transactions > 0,]

# We found 2 rows where sessions is zero and transactions are positive. 
# We will drop these two rows.

nrow(s)
s <- s[-which(s$sessions == 0 & s$transactions > 0),]
nrow(s)

# We use nrow(s) before and after to make sure only the 2 desired rows are dropped
# We could also use an assert statement if we want to be more formal

# Other sanity checks. 4 rows with 0 sessions and positive quantity
nrow(s[s$sessions == 0 & s$QTY > 0,])

# 160 rows with zero transactions and positive quantity
nrow(s[s$transactions == 0 & s$QTY > 0,])

# 580 rows with quantity less than transactions
nrow(s[s$QTY < s$transactions,])

# How should these problematic rows be handled? Depending on knowledge of the data
# source, it may be best to drop these. Or there could be a reason to keep 
# them in. Without any knowledge of the data source, we will leave these rows
# as is.


## Now let's briefly look at the numeric variables

# Tabulate the date variable to make sure there's no errors
table(s$dim_date, useNA = 'always')

# looks fine. Now make histograms of the other variables. Use log to adjust
# for skewness

hist(log(s$sessions), breaks=50)
hist(log(s$transactions), breaks=50)
hist(log(s$QTY), breaks=50)

# Don't see any visual irregularities
# End of data exploration

### Creating Tables

# The first sheet should contain a Month * Device aggregation of the data with 
# the following metrics: Sessions, Transactions, QTY, and ECR (= Transactions / Sessions)

# We group by month and device, and summarize using the sum function
# Then create the ECR variable
# Next, pivot wider because we'll want separate lines for each device category
# Finally, sort the table descending by month

s_final <- s %>% 
  group_by(month, dim_deviceCategory) %>%
  summarize(sessions = sum(sessions), transactions = sum(transactions),
            quantity = sum(QTY)) %>%
  mutate(ecr = transactions/sessions, qpt = quantity/transactions) %>%
  arrange(desc(month))


# The second sheet should contain a Month over Month comparison (for the most recent two months
# in the data) for all available metrics (including Adds to Cart), showing: the most recent month’s
# value, the prior month’s value, and both the absolute and relative differences between them

# call this table "m" for month-over-month

m <- a %>% # start with addstocart data
  mutate(month = paste0(dim_year, "-", dim_month)) %>% #paste year and month into one variable
  mutate(month = ym(month)) %>% #format as date 
  select(month, addsToCart) %>% #keep new month and addstocart columns only
  right_join(s_final, by=join_by(month)) %>% #join sessioncounts data by month
  pivot_wider(names_from = dim_deviceCategory, #pivot wider so month is only id variable
              values_from = c(sessions, transactions, quantity, ecr, qpt)) %>%
  arrange(desc(month)) %>% #sort descending by month
  slice_head(n=2) %>% # keep only 2 most recent months
  pivot_longer(!month, names_to = "metric", #now we have to transpose data. pivot long first
               values_to = "amount") %>%
  pivot_wider(names_from = month, values_from = amount) %>% #then pivot wide
  rename(june_2013=`2013-06-01`, may_2013=`2013-05-01`) %>% #rename variables
  mutate(absolute_diff=june_2013 - may_2013, # create differences columns
         relative_diff=(june_2013 - may_2013)/may_2013) %>%
  relocate(metric, may_2013, june_2013, absolute_diff, relative_diff) #reorder columns


### Save tables to Excel worksheets

wb <- openxlsx::createWorkbook(creator = "jseyhun")
addWorksheet(wb, "aggregate")
addWorksheet(wb, "month-to-month")
writeData(wb, "aggregate", s_final)
writeData(wb, "month-to-month", m)
saveWorkbook(wb, paste0(base_dir, "output.xlsx"))

### Create graphs for slide deck

ggplot(s_final) +
  geom_line(aes(month, sessions, # create line plot
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  labs(x="Month", y="Sessions", color="Device Category", # add title and labels
       title = "Sessions", subtitle = "July 2012 - June 2013") +
  scale_y_continuous(labels = scales::label_comma(), #format y axis 
                     n.breaks=10, limits=c(0,NA)) + #start y axis at 0
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + #format x axis
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1), #adjust title and x axis labels
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank()) # remove x axis minor gridlines

ggsave(paste0(graphs_dir, "sessions.png"))

ggplot(s_final) +
  geom_line(aes(month, transactions, 
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_comma(), n.breaks=10, limits=c(0,NA)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x="Month", y="Transactions", color="Device Category", 
       title = "Transactions", subtitle = "July 2012 - June 2013") +
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank()) 

ggsave(paste0(graphs_dir, "transactions.png"))


ggplot(s_final) +
  geom_line(aes(month, quantity, 
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_comma(), n.breaks=10, limits=c(0,NA)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x="Month", y="Quantity", color="Device Category", 
       title = "Quantity", subtitle = "July 2012 - June 2013") +
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank())

ggsave(paste0(graphs_dir, "quantity.png"))


ggplot(s_final) +
  geom_line(aes(month, ecr, 
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_comma(), n.breaks=10, limits=c(0,NA)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x="Month", y="ECR", color="Device Category", 
       title = "ECR", subtitle = "July 2012 - June 2013") +
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank()) 

ggsave(paste0(graphs_dir, "ecr.png"))

ggplot(s_final) +
  geom_line(aes(month, qpt, 
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_comma(), n.breaks=10, limits=c(0,NA)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x="Month", y="QPT", color="Device Category", 
       title = "Quantity Per Transaction (QPT)", subtitle = "July 2012 - June 2013") +
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank()) 

ggsave(paste0(graphs_dir, "qpt.png"))


### Extra

# Let's say the client wants to spend $1 million dollars on a paid ad campaign
# Should they target desktop, mobile, or tablet?
# Assume average transaction size is $40
# Assume average cost per click is $1
# Spending $1 million at $1 CPC will get 1 million clicks for each device category
# Assume $1 million will be spent uniformly over 12 months
# So ads will generate 83333.33 clicks per month, per device category
# Use given ECR data

# calculate revenue, then group by device and take cumulative sum of revenue
s_extra <- s_final %>%
  mutate(revenue=83333.33*ecr*40) %>%
  arrange(month) %>%
  group_by(dim_deviceCategory) %>%
  mutate(cum_revenue = cumsum(revenue))
  

ggplot(s_extra) +
  geom_line(aes(month, cum_revenue, 
                group=dim_deviceCategory, colour=dim_deviceCategory), linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_dollar(), n.breaks=10) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x="Month", y="", color="Device Category", 
       title = "Cumulative Revenue from $1M Ad Campaign", subtitle = "July 2012 - June 2013",
       caption = "Assuming $1 CPC and $40 Average Transaction Size. Final Revenue: Desktop - $1.36M, Mobile - $463K, Tablet - $936K") +
  theme(axis.text.x = element_text(angle=45, size=8, hjust=1),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        panel.grid.minor.x = element_blank()) +
  geom_hline(yintercept = 1000000, linetype=2, linewidth=1.2)

ggsave(paste0(graphs_dir, "revenue.png"))


