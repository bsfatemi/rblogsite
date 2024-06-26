library(data.table)
library(DBI)
library(stringr)
library(lubridate)
library(hcaconfig)

## get population
##
oid <- "f38cd5d0-f1e2-475c-ac14-d70d1595abb4"
nam <- orgShortName(oid)
fac <- "kindpeoplessoquel"
brd <- "PAX"
dpath <- str_glue("posts/Zoltrain Impact Analysis/data/{nam}_{fac}_{brd}_pop.csv")
dpath

# qry <- str_glue("SELECT * FROM population2
#                 WHERE org_uuid = '{oid}'
#                 AND facility = '{fac}'
#                 AND brand_name = '{brd}';")
#
# cn <- dbc("prod2", "integrated")
# MDR <- dbGetQuery(cn, qry)
#
# dbd(cn)
#
# setDT(MDR)
# fwrite(MDR, dpath)

MDR <- fread(dpath, data.table = TRUE)



## get zoltrain rewards data
##
zDT <- fread("posts/Impact of Budtender Rewards/data/Result_39.csv")
setnames(zDT, c("reward_id", "reward_date", "reward_value",
                "brand_id", "brand_name", "disp_id", "disp_name",
                "disp_state", "user_id", "reward_type"))
zDT[, c("reward_id", "brand_id", "disp_id") := NULL]
zDT[, reward_date := as.Date(reward_date)]



# Marina Del Rey ------------------------------------------------------------------------------

mmdRewards <- zDT[disp_name == "Kind Peoples - Soquel",
                  sum(reward_value),
                  keyby = .(reward_date, brand_name)]

## summarize sales by day
##
pdata <- MDR[brand_name == "PAX", .(
  units_sold = sum(product_qty),
  total_sales = sum(item_list_price),
  total_disc = -1 * sum(item_discount) / sum(item_subtotal)
), keyby = .(
  date = as_date(order_time_utc)
)]


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
pdata4[which(has_training_event)[1]:.N,
       is_training_period := TRUE]
pdata4[is.na(is_training_period), is_training_period := FALSE]

## keep data until the end of the training period only
pdata5 <- pdata4[ym < "2023-06-01"][year(ym) > 2019]

pdata5[is_training_period == FALSE, period := "Pre-Rewards Period"]
pdata5[is_training_period == TRUE, period := "Rewards Earning Period"]
pdata5[, period := factor(period, levels = c("Pre-Rewards Period", "Rewards Earning Period"))]

## get average for labelling plot lines
tmp <- pdata5[, .(ym = ym[1], aveMonthlySales = mean(tot_sales)), period]
tmp[, lab := scales::dollar(aveMonthlySales, accuracy = 1)]

p1 <- ggplot(pdata5, aes(ym, tot_sales)) +
  geom_line() +
  geom_point(aes(color = period), size = 5) +
  geom_hline(aes(yintercept = aveMonthlySales), tmp) +
  geom_text(aes(x = ym, y = aveMonthlySales, label = lab), tmp, vjust = -1, hjust = -.8) +
  scale_x_date(date_labels = "%b/%y") +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("") +
  ylab("") +
  ggtitle("Anonymized Retailer and Brand",
          "Sales Per Month - Before vs During Rewards Earning Period") +
  facet_grid(. ~ period, scales = "free") +
  ggthemes::theme_igray() +
  theme(legend.position = "top",
        strip.text.x = element_text(
          size = 12,
          color = "red3",
          face = "bold.italic"
        )) +
  guides(color = guide_legend(title = "")) +
  scale_color_manual(
    values = c("#e35148", "#479aed"),
    labels = c('No Engagement with Rewards', 'High Engagement with Rewards')
  )


ggsave(filename = str_glue("posts/Zoltrain Impact Analysis/plots/{nam}_{fac}_{brd}.png"),
       dpi = "print",
       scale = 5,
       height = 300,
       width = 700,
       units = "px",
       plot = p1)

# p1 <- ggplot(pdata5, aes(ym, tot_sales)) +
#   geom_line() +
#   geom_point(aes(color = has_training_event), size = 5) +
#   geom_hline(aes(yintercept = aveMonthlySales), tmp) +
#   geom_text(aes(x = ym, y = aveMonthlySales, label = lab), tmp, vjust = -1, hjust = -.8) +
#   scale_x_date(date_labels = "%b/%y") +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   xlab("") +
#   ylab("") +
#   ggtitle("Kind Peoples | Soquel | PAX",
#           "Sales Per Month - Before vs During Rewards Earning Period") +
#   facet_grid(. ~ period, scales = "free") +
#   ggthemes::theme_igray() +
#   theme(legend.position = "top",
#         strip.text.x = element_text(
#           size = 12,
#           color = "red3",
#           face = "bold.italic"
#         )) +
#   guides(color = guide_legend(title = "Redeemed Rewards:"))
#
# ggsave(filename = str_glue("posts/Zoltrain Impact Analysis/plots/{nam}_{fac}_{brd}.png"),
#        dpi = "print",
#        scale = 5,
#        height = 300,
#        width = 700,
#        units = "px",
#        plot = p1)

