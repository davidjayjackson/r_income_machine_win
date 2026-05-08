# Load Libraries

library(tidyverse)
library(lubridate)
library(DBI)
library(RMariaDB)
library(tidyquant)

rm(list=ls())
# 
con <- dbConnect(
  MariaDB(),
  host     = "localhost",
  user     = "root",
  password = "dJj135790",
  dbname   = "invest",
  port = 3333
)

symbols <- c("JEPI","QYLD","SCHD")
stocks <- tq_get(symbols, get = "stock.prices",from="2000-01-01")
dbWriteTable(con,"stocks",stocks, overwrite=TRUE)
dbListTables(con)
dbListFields(con,"stocks")
dbGetQuery(con,"SELECT min(date),max(date) from stocks")

# -----------------------------
# 4) Get dividend history
#    Use the last 12 months to estimate annual dividends/share
# -----------------------------
div_tbl <- tq_get(symbols, get = "dividends",from="2000-01-01") %>%
  mutate(date = as.Date(date))
last_date <- max(div_tbl$date, na.rm = TRUE)

annual_div_tbl <- div_tbl %>%
  filter(!is.na(value)) %>%
  filter(date > last_date %m-% months(12)) %>%
  group_by(symbol) %>%
  summarise(
    annual_dividend_per_share = sum(value, na.rm = TRUE),
    .groups = "drop" )

dbWriteTable(con,"dividends",div_tbl,overwrite=TRUE)
dbListTables(con)
dbListFields(con,"dividends")
dbGetQuery(con,"SELECT min(date), max(date) from dividends")
# Plot Dividends

ggplot(div_tbl) + geom_col(aes(x=date,y=value)) + facet_wrap(~symbol,scales="free_x")