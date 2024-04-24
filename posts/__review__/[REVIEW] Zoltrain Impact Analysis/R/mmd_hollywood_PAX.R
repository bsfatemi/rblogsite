library(data.table)
library(DBI)
library(stringr)
library(lubridate)
library(hcaconfig)

## get population
##
nam <- "mmd"
fac <- "hollywood"
brd <- "PAX"
dpath <- str_glue("posts/Zoltrain Impact Analysis/data/{nam}_{fac}_{brd}_pop.csv")
dpath

oid <- orgIndex()[short_name == "mmd", org_uuid]

qry <- str_glue("SELECT * FROM population2
                WHERE org_uuid = '{oid}'
                AND facility = '{fac}'
                AND brand_name = '{brd}';")

cn <- dbc("prod2", "integrated")
MDR <- dbGetQuery(cn, qry)

dbd(cn)

setDT(MDR)
fwrite(MDR, dpath)

MDR <- fread(dpath, data.table = TRUE)


## get zoltrain rewards data
##
zDT <- fread("posts/Zoltrain Course Completions/data/Result_39.csv")
setnames(zDT, c("reward_id", "reward_date", "reward_value",
                "brand_id", "brand_name", "disp_id", "disp_name",
                "disp_state", "user_id", "reward_type"))
zDT[, c("reward_id", "brand_id", "disp_id") := NULL]
zDT[, reward_date := as.Date(reward_date)]



# Marina Del Rey ------------------------------------------------------------------------------



mmdRewards <- zDT[disp_name == "MMD - Hollywood",
                  sum(reward_value),
                  keyby = .(reward_date, brand_name)]


## summarize sales by day
##
pdata <- MDR[, .(
  units_sold = sum(product_qty),
  total_sales = sum(item_list_price)
), keyby = .(
  date = as_date(order_time_utc)
)][year(date) >= 2022]

## capture all days, even when there were no sales (due to out of stock)
##
tmp <- data.table(date = seq(pdata[1, date], pdata[.N, date], by = "day"))
setkeyv(tmp, "date")

pdata2 <- pdata[tmp]
pdata2[is.na(pdata2)] <- 0

## calculate the cumulative sales velocities
pdata3 <- pdata2[, .(
  date,
  total_sales,
  units_sold,
  cumSalesVelocity = cumsum(total_sales) / (.I + 1),
  cumUnitVelocity = cumsum(units_sold) / (.I + 1)
)]

## summarize by week
pdata4 <- pdata3[, .(
  cum_dsvt = mean(cumSalesVelocity),
  cum_duvt = mean(cumUnitVelocity),
  tot_sales = sum(total_sales),
  tot_units = sum(units_sold)
), keyby = .(ym = floor_date(date, unit = "month"))]


## capture weeks where there was training
trainingDT <- mmdRewards[brand_name == "PAX", .N,
                         .(reward_date = floor_date(reward_date, "month"))][, .(reward_date)]
setkeyv(trainingDT, "reward_date")

pdata4[trainingDT, has_training_event := TRUE]
pdata4[is.na(has_training_event), has_training_event := FALSE]

## using the date of the first training, and date of the last, define the start and stop
## of the training period
##
# pdata4[which(has_training_event)[1]:.N, is_training_period := TRUE]
pdata4[which(has_training_event)[1]:which(has_training_event)[length(which(has_training_event))],
       is_training_period := TRUE]
pdata4[is.na(is_training_period), is_training_period := FALSE]

## keep data until the end of the training period only
pdata5 <- pdata4[(which(is_training_period)[1] - 4):rev(which(is_training_period))[1]]#[ym >= "2022-04-01"]

pdata5[is_training_period == FALSE, period := "Pre-Rewards Period"]
pdata5[is_training_period == TRUE, period := "Rewards Earning Period"]
pdata5[, period := factor(period, levels = c("Pre-Rewards Period", "Rewards Earning Period"))]

## get average for labelling plot lines
tmp <- pdata5[, .(ym = ym[1], aveMonthlySales = mean(tot_sales)), period]
tmp[, lab := scales::dollar(aveMonthlySales, accuracy = 1)]

p1 <- ggplot(pdata5, aes(ym, tot_sales)) +
  geom_line() +
  geom_point(aes(color = has_training_event), size = 5) +
  geom_hline(aes(yintercept = aveMonthlySales), tmp) +
  geom_text(aes(x = ym, y = aveMonthlySales, label = lab), tmp, vjust = -1, hjust = 0) +
  scale_x_date(date_breaks = "month", date_labels = "%b %y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("") +
  ylab("") +
  ggtitle("MMD | Hollywood | PAX",
          "Sales Per Month - Before vs During Rewards Earning Period") +
  facet_grid(. ~ period, scales = "free") +
  ggthemes::theme_igray() +
  theme(legend.position = "top",
        strip.text.x = element_text(
          size = 12, color = "red3", face = "bold.italic"
        )) +
  guides(color = guide_legend(title = "Redeemed Rewards:"))

ggsave(filename = str_glue("posts/Zoltrain Impact Analysis/plots/{nam}_{fac}_{brd}.png"),
       dpi = "print",
       scale = 5,
       height = 300,
       width = 700,
       units = "px",
       plot = p1)


