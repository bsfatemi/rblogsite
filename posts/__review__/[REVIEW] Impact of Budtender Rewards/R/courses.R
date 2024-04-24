library(fst)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(ggthemes)
library(hcaconfig)
library(DBI)




# "select sum(icr.incentive_value)
# from reward_user_qualification ruq
# inner join incentive_campaign_reward icr on ruq.incentive_campaign_reward_id = icr.id
# inner join incentive_campaign ic on ruq.incentive_campaign_id = ic.id
# where ruq.user_id = cc.user_id
# and ic.brand_id = completion_brand_id) as total_incentive_value"

# 0 is sample
# 1 & 2 are giftcards (1 is amazon only, 2 could be cash or a giftcard),
# 3 is a coupon

# DT <- fread("posts/Zoltrain Course Completions/course_completions_by_dispensary_brand.csv")

zDT <- fread("posts/Zoltrain Course Completions/data/Result_39.csv")
setnames(zDT, c("reward_id", "reward_date", "reward_value",
               "brand_id", "brand_name", "disp_id", "disp_name",
               "disp_state", "user_id", "reward_type"))


zDT[, c("reward_id", "brand_id", "disp_id") := NULL]


zDT[, reward_date := as.Date(reward_date)]


summDT <- zDT[, .(
  total_rewards = .N,
  total_value = sum(reward_value)
), .(disp_name, brand_name, ym = make_date(year(reward_date), month(reward_date)))]

pdata <- summDT[str_detect(disp_name, "Highway Cannabis - Marina Del Rey")]

brands <- c("ABSOLUTE EXTRACTS",
            "CARE BY DESIGN",
            "CHEMISTRY",
            "KANHA")

# ggplot(pdata[brand_name %in% c("ABX", "KANHA", "Care By Design", "Chemistry")]) +
ggplot(pdata) +
  geom_bar(aes(ym, total_value, fill = total_rewards), stat = "identity") +
  scale_x_date(date_breaks = "month") +
  facet_grid(brand_name ~ ., scales = "free") +
  ggthemes::theme_fivethirtyeight() +
  xlab("") +
  ggtitle("Highway Cannabis - Marina Del Rey", "Budtender Total Rewards Value Redeemed by Month") +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(fill = guide_legend(title = "Total Rewards Qty")) +
  theme(strip.text.y = element_text(angle = 0))


# HW -------------------------------------------------------------------------------------

cn <- dbc("prod2", "integrated")

oid <- "f38cd5d0-f1e2-475c-ac14-d70d1595abb4"
fac <- "marinadelrey"
dstart <- as_datetime("2022-08-22")
dstop <- as_datetime("2022-10-22")

qry <- str_glue(
  "SELECT *
  FROM population2
  WHERE org_uuid = '{oid}'
  AND facility = '{fac}'
  AND order_time_utc > '{dstart}'
  AND order_time_utc < '{dstop}'"
)

EVENT_DT <- dbGetQuery(cn, qry)
setDT(EVENT_DT)

EVENT_DT[, .N, brand_name]

zt_brands <- c("ABSOLUTE EXTRACTS",
               "CARE BY DESIGN",
               "CHEMISTRY",
               "KANHA")
               # "CLSICS",
               # "PURE",
               # "PAX",
               # "BINSKE",
               # "LAGUNITAS",
               # "PACIFIC STONE")


eventSumm <- EVENT_DT[, .(
  order_items = sum(product_qty),
  order_list = sum(item_list_price),
  order_disc = order_disc[1],
  order_subtot = order_subtot[1],
  order_tot = order_tot[1],
  has_zoltrain_brand = any(brand_name %in% zt_brands)
), keyby = .(
  order_time_utc,
  order_id,
  customer_id,
  phone,
  order_type
)]

eventSumm[, .(
  aveOrderTotal = mean(order_tot),
  aveOrderDisc = (-1 * sum(order_disc)) / sum(order_list),
  aveOrderItems = round(mean(order_items), 0),
  ordersPerDay = .N / length(unique(as_date(order_time_utc))),
  salesPerDay = sum(order_tot) / length(unique(as_date(order_time_utc))),
  totalSales = sum(order_tot)
  ), has_zoltrain_brand]

eventSumm[, , has_zoltrain_brand]
eventSumm[, mean(order_items), has_zoltrain_brand]
pdata <- eventSumm[, sum(order_list), .(has_zoltrain_brand, year(order_time_utc), week(order_time_utc))]

pdata[, week := factor(week, levels = c(26:53, 1:5))]


ggplot(pdata[!week %in% c(26, 53, 5)], aes(week, V1, color = has_zoltrain_brand)) +
  geom_point() +
  geom_smooth() +
  facet_grid(has_zoltrain_brand ~ ., scales = "free")

dstart <- as_datetime("2022-06-21")
dstop <- as_datetime("2022-08-21")

qry <- str_glue(
  "SELECT *
  FROM population2
  WHERE org_uuid = '{oid}'
  AND facility = '{fac}'
  AND order_time_utc > '{dstart}'
  AND order_time_utc < '{dstop}'"
)

PRE_DT <- dbGetQuery(cn, qry)
setDT(PRE_DT)


# HWPOP <- h[org_uuid == oid & facility == "marinadelrey"]



rdays <- as.numeric(rstop - rstart, units = "days")

PRE <- HWPOP[ order_time_utc > rstart - days(rdays) & order_time_utc < rstart ]
EVENT <- HWPOP[ order_time_utc < rstop & order_time_utc > rstart ]


PRE[, reward_period := "Pre-Rewards"]
EVENT[, reward_period := "Earning Rewards"]


DT <- rbindlist(list(PRE, EVENT))[!is.na(category3)]

DT[, as.numeric(max(order_time_utc) - min(order_time_utc), units = "days"), reward_period]


f <- function(x) {
  DT[
    !(brand_name == "PACIFIC STONE" & category3 == "VAPES") &
      !(brand_name == "CHEMISTRY" & category3 == "EATS") &
      !(brand_name == "CARE BY DESIGN" & category3 == "TABLETS/CAPSULES"), .(
    brand = x,
    salesPerDay = sum(item_subtotal * (brand_name == x)),
    pctOfSales = sum(item_subtotal * (brand_name == x)) / sum(item_subtotal * (brand_name != x))
  ), keyby = .(category3, reward_period)]
}


RES <- rbindlist(lapply(brands, f))


cats <- c("VAPES", "FLOWER", "EATS")

OUT <- rbindlist(list(
  RES,
  DT[category3 %in% cats, .(
    brand = "ALL OTHERS",
    salesPerDay = sum(item_subtotal * (!brand_name %in% brands)),
    pctOfSales = sum(item_subtotal * (!brand_name %in% brands)) / sum(item_subtotal)
  ), keyby = .(category3, reward_period)]
))[category3 %in% cats]

OUT[, reward_period := factor(reward_period, levels = c("Pre-Rewards", "Earning Rewards"))]
OUT[, brand := factor(brand, levels = c(brands, "ALL OTHERS"))]

setkeyv(OUT, c("brand", "category3", "reward_period"))

wOUT <- dcast(OUT, category3 + brand ~ reward_period, value.var = "salesPerDay")[
  !(`Pre-Rewards` == 0 & `Earning Rewards` == 0)
]

wOUT[, pctChange := (`Earning Rewards` - `Pre-Rewards`) / `Pre-Rewards`]

wOUT[, mean(pctChange), brand == "ALL OTHERS"]

OUT[, brand_group := brand == "ALL OTHERS"]

pdata <- OUT[salesPerDay > 0]

p1 <- ggplot(pdata[brand_group == FALSE]) +
  geom_bar(aes(brand, salesPerDay, fill = reward_period), stat = "identity", position = "dodge") +
  facet_grid(. ~ category3, scales = "free", space = "free") +
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K", accuracy = 1)) +
  ylab("Total Sales") +
  xlab("") +
  guides(fill = guide_legend(title = "Time Period")) +
  ggtitle("Total Sales By Brand and Category",
          "Pre-Rewards Period vs. Rewards Earning Period") +
  scale_fill_tableau()

ggplot(pdata[brand_group == FALSE]) +
  geom_bar(aes(brand, pctOfSales, fill = reward_period), stat = "identity", position = "dodge") +
  facet_wrap(. ~ category3, scales = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Percent of Sales") +
  xlab("") +
  ggtitle("Percent of Total Sales in Period",
          "Pre-Rewards Period vs. Rewards Earning Period") +
  guides(fill = guide_legend(title = "Time Period")) +
  scale_fill_tableau()


ggplot(pdata[brand_group == TRUE]) +
  geom_bar(aes(brand, salesPerDay, fill = reward_period), stat = "identity", position = "dodge") +
  facet_wrap(. ~ category3) +
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K")) +
  ylab("Total Sales") +
  xlab("") +
  ggtitle("Total Sales in Period",
          "For Brands Not Offering Rewards") +
  guides(fill = guide_legend(title = "Time Period")) +
  scale_fill_tableau()





# highway -------------------------------------------------------------------------------------



HWPOP <- h[org_uuid == "f38cd5d0-f1e2-475c-ac14-d70d1595abb4" & facility == "marinadelrey"]


course_date <- as.Date("2023-01-08")

PRE <- HWPOP[ order_time_utc < course_date & order_time_utc > course_date - days(30)]
POST <- HWPOP[ order_time_utc > course_date & order_time_utc < course_date + days(30)]

PRE[str_detect(brand_name, "PAX")][, sum(item_total), as.Date(order_time_utc)][, sum(V1)]
POST[str_detect(brand_name, "PAX")][, sum(item_total), as.Date(order_time_utc)][, sum(V1)]



pdata[, .(
  first_course = min(date_completed),
  last_course = max(date_completed),
  total_courses = sum(N),
  courses_per_day = sum(N) / max(c(1, as.numeric(max(date_completed) - min(date_completed), units = "days")))
), .(brand_name, dispensary_name)][total_courses > 1][order(-courses_per_day)]


