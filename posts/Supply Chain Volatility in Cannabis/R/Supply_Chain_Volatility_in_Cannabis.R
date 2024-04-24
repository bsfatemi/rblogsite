# Script Path :
# posts/_draft/Supply Chain Volatility in Cannabis/R/Supply_Chain_Volatility_in_Cannabis.R


library(hcapipelines2)
library(data.table)
library(hcadatatools)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggpubr)

options(scipen = 1000)

wd <- fs::path(rstudioapi::getActiveProject(), "posts/_draft/Supply Chain Volatility in Cannabis")
setwd(wd)
rstudioapi::filesPaneNavigate(wd)

## Get and land one full year of stock snapshots for all leaflogix locations
##  - This takes about 20 minutes
##  - The resulting data is available (dev2) consolidated.leaflogix.stock_snapshot
##  - Only needs to be run once and the analysis can continue with a query to the database
##
# res <- run_stock_snapshot(24, days = 365, write = TRUE)

## Runtime period for the stock snapshot pipeline to store 1 years worth of data
# int_runtime <- res$logs[
#   !is.na(TimestampUTC),
#   lubridate::interval(min(TimestampUTC), max(TimestampUTC))
# ]

## This function is used to query the resulting database table from the pipeline run above
## and retrieves the needed columns for all historical stock snapshots
db_get_snapshot <- function() {
  cn <- dbc("dev2", "consolidated")
  on.exit(dbd(cn))
  qry <- "SELECT org,
            store,
            snapshotdate,
            productid,
            sku,
            room,
            quantity,
            totalcost,
            unit
        FROM leaflogix.stock_snapshot"
  DT <- hcadatatools::dbQueryFetch(cn, qry, n = 10^7)
  return(DT[])
}

## Query snapshots
DT <- db_get_snapshot()[, "snapshotdate" := lubridate::as_date(snapshotdate)]
#
# fst::write.fst(DT, "data/snapshot.fst")
#
# DT <- fst::read_fst("data/snapshot.fst", as.data.table = TRUE)

setkey(DT, org, store, sku)
setorder(DT, org, store, sku, snapshotdate)

## Set location id to anon org/store
DT[, "locationid" := as.factor(str_pad(string = .GRP, width = 5, side = "left", pad = 0)),
   keyby = .(org, store)]

## Set keys and order table
keyCols <- c("locationid", "sku", "snapshotdate")
setkeyv(DT, keyCols)

DT[, cost_per_unit := totalcost / quantity]



# GET PRODUCT SALES PERIOD --------------------------------------------------------------------


##
## Get time series and fill in missing dates with 0 quantity by sku and location
##
snaps <- DT[, .(
  total_qty = sum(quantity),
  total_cost = sum(totalcost)
  ), .(locationid, sku, snapshotdate)]

snaps[, sum(total_qty), .(locationid, snapshotdate)][, mean(V1), locationid][, mean(V1)]
snaps[, sum(total_cost, na.rm = TRUE), .(locationid, snapshotdate)][, mean(V1)][, mean(V1), locationid]

timeSeries <- snaps[, .(
  snapshotdate = seq(min(snapshotdate), max(snapshotdate), by = 1)
), .(locationid, sku)]

keyCols <- c("locationid", "sku", "snapshotdate")
setkeyv(timeSeries, keyCols)
setkeyv(snaps, keyCols)


snapSeries <- snaps[timeSeries]
snapSeries[is.na(total_qty),  total_qty := 0]
snapSeries[is.na(total_cost), total_cost := 0]

snapSeries[total_qty > 0, ave_unit_cost := total_cost / total_qty]
setkey(snapSeries, locationid, sku)

tmp <- snapSeries[snapSeries[total_qty == 0, .N, keyby = .(locationid, sku)]][
  total_qty > 0,
  .(ave_unit_cost = mean(ave_unit_cost)),
  keyby = .(locationid, sku)]

snapSeries <- tmp[snapSeries][is.na(i.ave_unit_cost), i.ave_unit_cost := ave_unit_cost][]
snapSeries[, ave_unit_cost := NULL]
setnames(snapSeries, "i.ave_unit_cost", "ave_unit_cost")


snapSeries[total_qty > 0, .(
  n_prods = .N,
  mu_days = mean(V1),
  sd_days = sd(V1)
), .(locationid)]


summStats <- snapSeries[, .(
  days_no_stock = sum(total_qty == 0),
  days_with_stock = sum(total_qty > 0),
  span_days = as.numeric(difftime(
    max(snapshotdate),
    min(snapshotdate),
    units = "days"
  )) + 1
), .(locationid, sku)][, .(
  n_prods = .N,
  ave_span = mean(span_days),
  ave_days_no_stock = mean(days_no_stock),
  ave_days_with_stock = mean(days_with_stock)
), .(locationid)]

summStats[, .(
  ave_tot_skus = mean(n_prods),
  ave_tot_span = as.numeric(mean(ave_span)),
  ave_days_nostock = mean(ave_days_no_stock),
  ave_days_withstock = mean(ave_days_with_stock),
  cfi_tot_skus = 1.96 * sd(n_prods) / sqrt(.N),
  cfi_tot_days = 1.96 * sd(ave_span) / sqrt(.N)
)]

pdt <- melt(summStats,
     id.vars = "locationid",
     measure.vars = c("ave_days_no_stock", "ave_days_with_stock"))

setkey(pdt, locationid)
pdt2 <- DT[, .(org = unique(org)), keyby = .(locationid)][pdt]

anonOrg <- function(x) {
  txt <- stringr::str_pad(stringr::str_trunc(x, 10, ellipsis = ""), 10, side = "right", pad = "x")
  stringr::str_trunc(sodium::bin2hex(charToRaw(txt)), 10, "right", "")
}
pdt3 <- pdt2[, mean(value), .(org, variable)]
pdt3[, orgid := paste0("Org ", .GRP), org]

levs <- pdt3[variable == "ave_days_no_stock", as.character(orgid[order(-V1)])]
pdt3[, orgid := factor(orgid, levels = levs, labels = 1:length(levs))]
levs <- c("ave_days_with_stock", "ave_days_no_stock")
pdt3[, variable := factor(variable, levels = levs)]

p <- ggplot(pdt3) +
  geom_bar(aes(orgid, V1, fill = variable), stat = "identity") +
  ggthemes::theme_tufte() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    legend.title = element_blank(),
    legend.position = "top"
    ) +
  # scale_fill_discrete(labels = c("Ave Days With Stock", "Ave Days Without Stock"),
  #                     values = c("#00a651", "#ffa412")) +
  scale_fill_manual(labels = c("Ave Days With Stock", "Ave Days Without Stock"),
                    values = c("#00a651", "#ffa412")) +
  xlab("Anonomized Organization ID") +
  ylab("Ave Sales Period (Days)") +
  ggtitle("Product Sales Period By Org", "Ave Days Between First and Last Sale for All Products")
p + scale_y_continuous(expand = c(0,0))

# p + scale_x_discrete(expand = c(-1,0))


# PLOT STOCK VARIABILITY ----------------------------------------------------------------------



## Summarize units across rooms by date
summQty <- DT[, .(cost = sum(totalcost), qty = sum(quantity)), keyby = keyCols]

## By SKU, Fill in missing dates between first and last snapshot with 0 units available
tmp <- summQty[, .(
  "snapshotdate" = seq.Date(min(snapshotdate), max(snapshotdate), by = 1)
  ), .(locationid, sku)]
summDT <- setkeyv(summQty, keyCols)[setkeyv(tmp, keyCols)][is.na(qty), qty := 0][]


###
###
### Summarize Sample Population in this Analysis
###
###
## Calc the following stats by product and location
##  - total days since product was first stocked
##  - total days in period with units in stock > 0
##  - total days in period with units in stock = 0
##  - Average units in stock in this period
##  - Stdev of units in stock in this period
##  - Filter products that have never had stock
##  - Filter products that have never been sold out
##  - Filter products that don't have at least 2wks snapshots
freqDT <- summDT[, .(
  total_snaps = .N,
  days_in_stk = sum(qty > 0),
  days_no_stk = sum(qty == 0),
  ave_stk_qty = mean(qty),
  std_stk_qty = sd(qty),
  cfi_stk_qty = 1.96 * sd(qty) / sqrt(.N),
  ave_stk_val = mean(cost, na.rm = TRUE),
  std_stk_val = sd(cost, na.rm = TRUE),
  cfi_stk_val = 1.96 * sd(cost, na.rm = TRUE) / sqrt(.N)
), .(locationid, sku)
]

## Stats for a given day
summDT[, .(
  total_skus = .N,
  total_qty = sum(qty),
  total_cost = sum(cost)
), .(locationid, snapshotdate)
][, .(
  uniq_skus = mean(total_skus),
  tot_units = mean(total_qty),
  tot_costs = mean(total_cost, na.rm = TRUE)
  )]

## Stats for the full time period
freqDT[, .(
  tot_uniq_products = .N,
  ave_days_in_stock = mean(days_in_stk, na.rm = TRUE),
  ave_days_no_stock = mean(days_no_stk, na.rm = TRUE),
  ave_cost_of_stock = mean(ave_stk_val, na.rm = TRUE)
), locationid][, .(
  ave_uniq_products = mean(tot_uniq_products),
  ave_days_in_stock = mean(ave_days_in_stock),
  ave_days_no_stock = mean(ave_days_no_stock),
  ave_cost_of_stock = mean(ave_cost_of_stock)
)]


###
###
### Build data for output visualizations
###
stock <- summDT[, .(
  total_qty = ceiling(sum(qty)),
  total_cost = sum(cost, na.rm = TRUE)
), .(locationid, snapshotdate)][total_qty > 0]


setkey(stock, locationid, snapshotdate)
stock[, qty_delta := c(0, diff(total_qty)), locationid]
stock[, pct_delta := c(0, diff(total_qty) / total_qty[-.N]), locationid]
stock[, stk_level := cumprod(1 + pct_delta), locationid]

pdata <- stock[, .(
  "ave_stock_level" = mean(stk_level),
  "ave_total_units" = mean(total_qty),
  "ave_total_costs" = mean(total_cost)
  ), keyby = .(
  locationid,
  week_date = lubridate::floor_date(snapshotdate, unit = "week")
)]

##
## Label identified outliers and filter out
##
pdata[locationid == "00005", is_outlier := ave_total_units < 10000]
pdata[locationid == "00066", is_outlier := week_date <= "2022-10-30"]
pdata[locationid == "00067", is_outlier := ave_total_costs > 125000]
pdata[locationid == "00069", is_outlier := ave_total_costs < 40000]
pdata[locationid == "00076", is_outlier := ave_total_costs < 15000]
pdata[locationid == "00031", is_outlier := ave_total_units < 1000]

pdata[is.na(is_outlier), is_outlier := FALSE]

pdata2 <- pdata[!(is_outlier)]

## Ensure the first observation for every location starts at 100%
setkey(pdata2, locationid, week_date)
pdata2[
  setkey(pdata2[, .SD[1], locationid], locationid, week_date),
  ave_stock_level := 1
]

## Function to create exploration plots
##
..plot <- function(x, y, by) {
  locId <- by$locationid
  dT <- data.table(x, y)
  p_delta <- dT[, (y[.N] - y[1]) / y[1]]
  ggplot(dT, aes(x, y)) +
    geom_point() +
    geom_line()
}

## Load previous classications based on earlier exploration
metaIndex <- fst::read_fst("data/metaIndex.fst", as.data.table = TRUE)
setkey(metaIndex, locationid)

plotIndex <- metaIndex[setkey(unique(DT[, .(org, store, locationid)]), locationid)[
  pdata2[, .(
    data = list(.SD),
    plot_1 = list(..plot(week_date, ave_stock_level, .BY)),
    plot_2 = list(..plot(week_date, ave_total_costs, .BY))
  ),keyby = locationid]
]]

# groupsDT <- plotIndex[, .(locationid, plot_1, plot_2, group_1 = grp, group_2 = NA_character_)]
# for (i in groupsDT[, .I]) {
#   lid <- groupsDT[i, locationid]
#   print(lid)
#   print(plotIndex[i, plot_2[[1]]])
#   groupsDT[i, group_2 := readline("1 (down)|2 (up)|3 (else) @> ")]
# }
# groupsDT[, group_1 := as.integer(group_1)]
# groupsDT[, group_2 := as.integer(group_2)]
# metaIndex <- groupsDT[, .(locationid, level_group = group_1, cost_group = group_2)]
# fst::write_fst(metaIndex, "data/metaIndex.fst")

## Anonomize org
anonOrg <- function(x) {
  txt <- stringr::str_pad(stringr::str_trunc(x, 8, ellipsis = ""), 8, side = "right", pad = "x")
  stringr::str_trunc(sodium::bin2hex(charToRaw(txt)), 8, "right", "")
}
plotIndex[, orgid := anonOrg(org), org]

## define custom median function
plot.median <- function(x) {
  m <- median(x)
  c(y = m, ymin = as.numeric(quantile(x, .15)), ymax = as.numeric(quantile(x, .85)))
}

## Function to create output plots
plot_stock_level_stores <- function(data, colr, method = NULL) {
  num_locs <- data[, length(unique(locationid))]
  org_anon <- data[1, orgid]

  ## build plot
  p <- ggplot(data, aes(week_date, ave_stock_level)) +
    geom_line(aes(color = locationid), linetype = 3) +
    geom_point(aes(color = locationid), size = .5) +
    stat_summary(
      fun.data = "plot.median",
      geom = "errorbar",
      colour = colr,
      width = 1,
      alpha = 1,
      linewidth = 1
    ) +
    theme_tufte() +
    theme(axis.title = element_blank(),
          legend.position = "none") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ylab("Stock Relative to Day One") +
    ggtitle(
      "Average Weekly % Change in Total Inventory Units",
      stringr::str_glue("85% Confidence Interval for {num_locs} Locations (Retailer: {org_anon})")
    )
  if (!is.null(method))
    p <- p + geom_smooth(color = "black", se = FALSE, method = method, linewidth = .75)
  p
}

plot_stock_level_orgs <- function(data, colr, method = NULL) {
  num_orgs <- data[, length(unique(orgid))]
  num_locs <- data[, max(total_locations), .(orgid)][, sum(V1)]

  ## build plot
  p <- ggplot(data, aes(week_date, ave_stock_level)) +
    geom_line(aes(color = orgid), linetype = 3) +
    geom_point(aes(color = orgid), size = .5) +
    stat_summary(
      fun.data = "plot.median",
      geom = "errorbar",
      colour = colr,
      width = 1,
      alpha = 1,
      linewidth = 1
    ) +
    theme_tufte() +
    theme(axis.title = element_blank()) +
    scale_y_log10(labels = scales::percent_format(accuracy = 1)) +
    ylab("Stock Relative to Day One") +
    ggtitle(
      "Average Weekly % Change in Total Inventory Units",
      stringr::str_glue(
        "85% Confidence for {num_locs} Locations Across {num_orgs} Retailers"
      )
    )
  if (!is.null(method))
    p <- p + geom_smooth(color = "black", se = FALSE, method = method, linewidth = .75)
  p
}


## Units Group 2 (Increasing Inventory Level)
tmp <- plotIndex[level_group == 2, rbindlist(data), keyby = .(orgid, locationid)][
  !locationid %in% c("00025", "00036", "00044", "00059")
]
tmp[, ave_stock_level := cumprod(1 + c(0, diff(ave_total_units) / ave_total_units[-.N])),
    locationid]

tmp2 <- tmp[, .(
  total_locations = .N,
  ave_stock_level = mean(ave_stock_level),
  ave_total_units = mean(ave_total_units),
  ave_total_costs = mean(ave_total_costs)
),
keyby = .(orgid, week_date)]

plot_stock_level_orgs(
  data = tmp2,
  colr = "#ef574e",
  method = "loess"
)

## Units Group 2 (Decreasing Inventory Level)
tmp <- plotIndex[level_group == 1, rbindlist(data), keyby = .(orgid, locationid)]
# [
#   !locationid %in% c("00025", "00036", "00044", "00059")
# ]
tmp[, ave_stock_level := cumprod(1 + c(0, diff(ave_total_units) / ave_total_units[-.N])),
    locationid]

tmp2 <- tmp[, .(
  total_locations = .N,
  ave_stock_level = mean(ave_stock_level),
  ave_total_units = mean(ave_total_units),
  ave_total_costs = mean(ave_total_costs)
),
keyby = .(orgid, week_date)]

plot_stock_level_orgs(
  data = tmp2[orgid != "6469656d"],
  colr = "#57cf7b",
  method = "loess"
)


## Units Group 3 (Decreasing Inventory Level)
tmp <- plotIndex[level_group == 3, rbindlist(data), keyby = .(orgid, locationid)]
# [
#   !locationid %in% c("00025", "00036", "00044", "00059")
# ]
tmp[, ave_stock_level := cumprod(1 + c(0, diff(ave_total_units) / ave_total_units[-.N])),
    locationid]

tmp2 <- tmp[, .(
  total_locations = .N,
  ave_stock_level = mean(ave_stock_level),
  ave_total_units = mean(ave_total_units),
  ave_total_costs = mean(ave_total_costs)
),
keyby = .(orgid, week_date)]

plot_stock_level_orgs(
  data = tmp2,
  colr = "#4ba4fb",
  method = "lm"
)



###
### Good Day Farms
###
data_2 <- plotIndex[org == "gooddayfarms" & grp == 2 & show_plot, rbindlist(data), .(orgid, locationid)][
  !locationid %in% c("00026", "00058", "00039", "00034", "00030", "00040")
]
plot_stock_level(data_2, "#ef574e", method = "loess")


###
### Verano
###
data_1 <- plotIndex[org == "verano" & grp == 1, rbindlist(data), .(grp, orgid, locationid, org)]
data_2 <- plotIndex[org == "verano" & grp == 2, rbindlist(data), .(grp, orgid, locationid, org)]
data_3 <- plotIndex[org == "verano" & grp == 3, rbindlist(data), .(grp, orgid, locationid, org)]
setorder(data_1, week_date)
setorder(data_2, week_date)
setorder(data_3, week_date)
p1 <- plot_stock_level(data_1[!locationid %in% c("00131", "00139")], "#57cf7b", method = "loess")
p2 <- plot_stock_level(data_2, "#ef574e", method = "loess")
p3 <- plot_stock_level(data_3[!locationid %in% c("00119", "00113")], "#4ba4fb", method = "lm")




plotIndex[, .N, grp]
