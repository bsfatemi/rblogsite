library(data.table)
library(stringr)
library(lubridate)
library(hcaconfig)
library(DBI)
library(fst)
library(ggplot2)
library(patchwork)
library(ggthemes)

options(scipen = 10000)
threads_fst(nr_of_threads = 24, reset_after_fork = NULL)
setDTthreads(threads = 18)



# GET POPULATION - CREATE CAT3 ----------------------------------------------------------------


h <- read_fst("~/PROJECTS/analysis/data/pop_5-22-23.fst", as.data.table = TRUE,
              c("org_uuid", "facility", "brand_name", "pos_category", "order_time_utc",
                "product_name", "item_list_price", "item_subtotal", "product_qty",
                "item_discount"))

POP2 <- read_fst("~/PROJECTS/analysis/data/pop_5-22-23.fst", as.data.table = TRUE,
                 c("pos_category", "product_name"))

# h[brand_name == "NO_VALUE", brand_name := NA]
# h[brand_name == "DRIVER TIP", brand_name := NA]
# h[product_name == "DRIVER TIP", product_name := NA]



t <- Sys.time()
P2CATS <- buildCategory3(POP2)
Sys.time() - t

# identify outliers in price ------------------------------------------------------------------


h[item_list_price < 0, item_subtotal := item_subtotal + -1*item_list_price]

h[item_list_price < 0, item_list_price := item_subtotal + item_discount]

h[, price_per_unit := item_subtotal / product_qty]


DT <- h[!is.na(brand_name)]

# DT[, .(
#   med = median(price_per_unit),
#   q05 = quantile(price_per_unit, .05),
#   q95 = quantile(price_per_unit, .95)
#   ), category3]


# cats <- c("EDIBLES", "VAPES", "EXTRACTS",
#           "PREROLLS", "FLOWER", "TOPICALS",
#           "TABLETS_CAPSULES", "DRINKS", "TINCTURES")
#
# priceSummary <- DT[category3 %in% cats, .(
#   items = .N,
#   orgs = length(unique(org_uuid)),
#   medPrice = median(item_subtotal),
#   avePrice = mean(item_subtotal),
#   sdPrice = sd(item_subtotal),
#   aveDisc = mean(item_discount)
# ), .(brand_name, category3, product_name)][orgs > 29][order(brand_name, category3)]
#
# priceSummary[order(-orgs)][orgs > 5]


# facs <- orgIndex()[, .(org_uuid, short_name)]
# setkeyv(facs, "org_uuid")


# KIVA MINTS -------------------------------------------------------------------------





# ##
# ## clean up data
# ##
# h[brand_name == "NO_VALUE", brand_name := NA]
# h[brand_name == "DRIVER TIP", brand_name := NA]
# h[product_name == "DRIVER TIP", product_name := NA]
#
# h[str_detect(brand_name, "VERANO"), brand_name := "VERANO"]
# h[str_detect(brand_name, "ENCORE EDIBLES"), brand_name := "VERANO"]
# h[str_detect(brand_name, "TRAVELERS \\| ESSENCE"), brand_name := "VERANO"]
# h[brand_name == "ENCORE", brand_name := "VERANO"]
#
# h[str_detect(brand_name, "^RYTHM \\|"), brand_name := "RYTHM"]



# BUILD SEGMENTS ------------------------------------------------------------------------------

buildSegment <- function(DT) {

  ## we know that this is calculating the days that the customer made an order
  custFreq <- DT[, .(orderTotal = sum(item_total)),
                 keyby = .(
                   customer_id,
                   phone,
                   order_date = as_date(order_time_utc)
                 )]

  tmp <- custFreq[, .(
    totalOrders = .N,
    aveDaysBetween = ceiling(as.numeric(mean(diff(order_date), na.rm = TRUE), units = "days")),
    sdDaysBetween = ceiling(as.numeric(sd(diff(order_date), na.rm = TRUE), units = "days")),
    aveOrderSize = mean(orderTotal)
  ), .(customer_id, phone)
  ][ !is.na(aveDaysBetween) & !is.na(sdDaysBetween) ]

  ##
  ## Get Quantile Boundaries
  ##
  qs <- c(.25, .75)

  # upper bounds for total orders
  lb_totalorders <- tmp[, quantile(totalOrders,   qs, na.rm = TRUE)[1]]
  ub_totalorders <- tmp[, quantile(totalOrders,   qs, na.rm = TRUE)[2]]

  # lower bounds for frequency and variability
  lb_daysbetween <- tmp[, quantile(aveDaysBetween, qs, na.rm = TRUE)[1]]
  ub_daysbetween <- tmp[, quantile(aveDaysBetween, qs, na.rm = TRUE)[2]]

  # lower bounds for frequency and variability
  lb_daysbetweenVar <- tmp[, quantile(sdDaysBetween, qs, na.rm = TRUE)[1]]
  ub_daysbetweenVar <- tmp[, quantile(sdDaysBetween, qs, na.rm = TRUE)[2]]

  # upper bounds for order size
  lb_ordersize <- tmp[, quantile(aveOrderSize,  qs, na.rm = TRUE)[1]]
  ub_ordersize <- tmp[, quantile(aveOrderSize,  qs, na.rm = TRUE)[2]]


  ###
  ### Long time repeat customers
  ###
  MAT1 <- tmp[totalOrders > ub_totalorders,
              .(customer_id, phone, is_long_time = TRUE)]

  ###
  ### Customers that purchase most frequently
  ###
  MAT2 <- tmp[aveDaysBetween < lb_daysbetween &
                sdDaysBetween < lb_daysbetweenVar,
              .(customer_id, phone, is_most_frequent = TRUE)]

  ###
  ### Customers that purchase least frequently
  ###
  MAT3 <- tmp[aveDaysBetween > ub_daysbetween &
                sdDaysBetween > ub_daysbetweenVar,
              .(customer_id, phone, is_least_frequent = TRUE)]

  ###
  ### High dollar spenders AND Low dollar spenders
  ###
  MAT4 <- tmp[aveOrderSize > ub_ordersize,
              .(customer_id, phone, is_high_spender = TRUE)]
  MAT5 <- tmp[aveOrderSize < lb_ordersize,
              .(customer_id, phone, is_low_spender = TRUE)]

  ##
  ## Get category preferences
  ##
  cats <- c("PREROLLS",
            "EDIBLES",
            "DRINKS",
            "EXTRACTS",
            "FLOWER",
            "VAPES",
            "TOPICALS",
            "TABLETS_CAPSULES",
            "TINCTURES")

  custPrefs <- DT[!is.na(phone) & category3 %in% cats, .(
    totalSpend = sum(item_subtotal),
    totalItems = .N
  ), .(customer_id, phone, category3)] #[totalItems > 1]

  custPrefs[, "catPreference" := totalSpend / sum(totalSpend), .(customer_id, phone)]

  cpShare <- dcast.data.table(custPrefs,
                              customer_id + phone ~ category3,
                              value.var = "catPreference",
                              fill = 0)
  setnames(cpShare, "TABLETS_CAPSULES", "TABLETS_CAPSULES", skip_absent = TRUE)

  ###
  ### ENSURE ALL DESIRED CATEGORY COLUMNS EXIST IN TABLE
  ###  - Note some clients have no extract sales for some reason. Look into this later
  ###
  if (!"PREROLLS" %in% names(cpShare)) {
    cpShare[, PREROLLS := 0]
  }
  if (!"EATS" %in% names(cpShare)) {
    cpShare[, EATS := 0]
  }
  if (!"DRINKS" %in% names(cpShare)) {
    cpShare[, DRINKS := 0]
  }
  if (!"EXTRACTS" %in% names(cpShare)) {
    cpShare[, EXTRACTS := 0]
  }
  if (!"FLOWER" %in% names(cpShare)) {
    cpShare[, FLOWER := 0]
  }
  if (!"VAPES" %in% names(cpShare)) {
    cpShare[, VAPES := 0]
  }
  if (!"TOPICALS" %in% names(cpShare)) {
    cpShare[, TOPICALS := 0]
  }
  if (!"TABLETS_CAPSULES" %in% names(cpShare)) {
    cpShare[, TABLETS_CAPSULES := 0]
  }
  if (!"TINCTURES" %in% names(cpShare)) {
    cpShare[, TINCTURES := 0]
  }

  ### EXCLUSIVE PRODUCT TYPE CUSTOMERS
  MAT6  <- cpShare[PREROLLS > .75,
                   .(customer_id, phone, is_prerolls_only = TRUE)]
  MAT7  <- cpShare[EATS > .75,
                   .(customer_id, phone, is_edibles_only = TRUE)]
  MAT8  <- cpShare[DRINKS > .75,
                   .(customer_id, phone, is_drinks_only = TRUE)]
  MAT9  <- cpShare[EXTRACTS > .75,
                   .(customer_id, phone, is_extracts_only = TRUE)]
  MAT10 <- cpShare[FLOWER > .75,
                   .(customer_id, phone, is_flower_only = TRUE)]
  MAT11 <- cpShare[VAPES > .75,
                   .(customer_id, phone, is_vapes_only = TRUE)]
  MAT12 <- cpShare[TOPICALS > .75,
                   .(customer_id, phone, is_topicals_only = TRUE)]
  MAT13 <- cpShare[TABLETS_CAPSULES > .75,
                   .(customer_id, phone, is_tablets_only = TRUE)]
  MAT14 <- cpShare[TINCTURES > .75,
                   .(customer_id, phone, is_tinctures_only = TRUE)]

  ## Percent of all customers that have a dominant preference
  pct <- sum(sapply(list(
    MAT6, MAT7, MAT8, MAT9, MAT10, MAT11, MAT12, MAT13, MAT14
  ), nrow)) / nrow(cpShare)
  print(pct)

  ###
  ### Discount motivated customers
  ###
  discDT <- DT[, .(
    totalOrders = .N,
    totalSpent = sum(item_subtotal),
    totalDiscount = -1 * sum(item_discount)
  ), .(order_id, customer_id, phone)]
  discDT[, avePctDisc := totalDiscount / totalSpent]

  discQS <- discDT[avePctDisc > 0, quantile(avePctDisc, qs)]

  MAT15 <- discDT[avePctDisc > discQS[2], .(
    customer_id, phone, is_discount_sensitive = TRUE
  )]

  ###
  ### Combine labelled customer segments
  ###
  keyvars <- c("customer_id", "phone")

  setkeyv(MAT1, keyvars)
  setkeyv(MAT2, keyvars)
  setkeyv(MAT3, keyvars)
  setkeyv(MAT4, keyvars)
  setkeyv(MAT5, keyvars)
  setkeyv(MAT6, keyvars)
  setkeyv(MAT7, keyvars)
  setkeyv(MAT8, keyvars)
  setkeyv(MAT9, keyvars)
  setkeyv(MAT10, keyvars)
  setkeyv(MAT11, keyvars)
  setkeyv(MAT12, keyvars)
  setkeyv(MAT13, keyvars)
  setkeyv(MAT14, keyvars)
  setkeyv(MAT15, keyvars)

  CSEGS <- Reduce(
    function(x, y) merge.data.table(x[!is.na(phone)], y[!is.na(phone)], all = TRUE),
    list(
      MAT1, MAT2, MAT3, MAT4, MAT5, MAT6, MAT7, MAT8,
      MAT9, MAT10, MAT11, MAT12, MAT13, MAT14, MAT15
    )
  )

  CSEGS[is.na(CSEGS), ] <- FALSE # replace all NAs with FALSE

  ## append org_uuid and return result
  OUT <- unique(cbind(org_uuid = DT[1, org_uuid], CSEGS))
  OUT[]
}
buildSegmentStats <- function(segment, DT) {
  getSegmentStats <- function(flag) {
    seg <- segment[get(flag), (keys), with = FALSE]

    tmp <- DT[seg, .(
      order_total = sum(item_total, na.rm =  TRUE),
      ave_item_price = mean(item_total, na.rm = TRUE)
    ), .(facility, order_id, customer_id, phone, birthday, order_time_utc, age)]

    setkeyv(tmp, "order_time_utc")

    OUT <- data.table(
      segment = flag,
      ave_orders_per_day = tmp[, .N, as.Date(order_time_utc)][, mean(N)],
      ave_age = tmp[, mean(age, na.rm = TRUE)],
      ave_order_total = tmp[, mean(order_total)],
      ave_item_price = tmp[, mean(ave_item_price)],
      ave_orders_per_cust = tmp[, .N, .(customer_id, phone)][, mean(N)],
      total_customers = tmp[, .N, .(customer_id, phone)][, .N]
    )
    OUT[]
  }
  getStats <- function() {
    tmp <- DT[, .(
      order_total = sum(item_total, na.rm =  TRUE),
      ave_item_price = mean(item_total, na.rm = TRUE)
    ), .(facility, order_id, customer_id, phone, birthday, order_time_utc, age)]

    setkeyv(tmp, "order_time_utc")

    OUT <- data.table(
      segment = "all customers",
      ave_orders_per_day = tmp[, .N, as.Date(order_time_utc)][, mean(N)],
      ave_age = tmp[, mean(age, na.rm = TRUE)],
      ave_order_total = tmp[, mean(order_total)],
      ave_item_price = tmp[, mean(ave_item_price)],
      ave_orders_per_cust = tmp[, .N, .(customer_id, phone)][, mean(N)],
      total_customers = tmp[, .N, .(customer_id, phone)][, .N]
    )
    OUT[]
  }

  ## define keys to order data and also keys
  ## used in the locally defined functions above
  keys <- c("customer_id", "phone")
  setkeyv(DT, keys)

  segment_stats <- cbind(
    org_uuid = DT[1, org_uuid],
    short_name = oname,
    rbindlist(list(
      getStats(),
      getSegmentStats("is_extracts_only"),
      getSegmentStats("is_vapes_only"),
      getSegmentStats("is_tinctures_only"),
      getSegmentStats("is_flower_only"),
      getSegmentStats("is_tablets_only"),
      getSegmentStats("is_topicals_only"),
      getSegmentStats("is_edibles_only"),
      getSegmentStats("is_drinks_only"),
      getSegmentStats("is_prerolls_only"),
      getSegmentStats("is_discount_sensitive"),
      getSegmentStats("is_most_frequent"),
      getSegmentStats("is_least_frequent"),
      getSegmentStats("is_high_spender"),
      getSegmentStats("is_low_spender")
    ))
  )

  segment_stats[, ave_orders_per_day := ceiling(ave_orders_per_day)]
  segment_stats[, ave_age := ceiling(ave_age)]
  segment_stats[, ave_order_total := round(ave_order_total, 0)]
  segment_stats[, ave_item_price := round(ave_item_price, 0)]
  segment_stats[, ave_orders_per_cust := ceiling(ave_orders_per_cust)]

  segment_stats
}

oid_vec <- h[, .N, org_uuid][order(-N)][1:5, org_uuid]

oid <- oid_vec[1]
oname <- hcaconfig::orgShortName(oid)
DT <- h[org_uuid == oid]
segment <- buildSegment(DT)
stats <- buildSegmentStats(segment, DT)


tot_custs <- stats[segment == "all customers", total_customers]
stats[, pct_of_customers := total_customers / tot_custs]





library(ggplot2)
library(ggthemes)

stats[, ave_order_total := as.numeric(str_remove(ave_order_total, "\\$"))]
stats[, total_customers := as.numeric(str_remove_all(total_customers, "\\,"))]

stats[str_detect(segment, "extra|top|vap|prer|edib|flow|drink|tab|tinct"),
      segment_type := "Product Preference"]
stats[!str_detect(segment, "extra|top|vap|prer|edib|flow|drink|tab|tinct"),
      segment_type := "Customer Behavior"]

stats[segment_type == "Product Preference",
      segment := paste0("Prefers ",
                        str_extract(segment, "(?<=is_).+(?=_only)"))]

stats[segment == "is_discount_senstive", segment := "Discount Sensitive"]
stats[segment == "is_high_spender", segment := "High Spender"]
stats[segment == "is_low_spender", segment := "Low Spender"]
stats[segment == "is_least_frequent", segment := "Orders Less Frequently"]
stats[segment == "is_most_frequent", segment := "Orders Most Frequently"]
stats[segment == "is_discount_sensitive", segment := "Is Discount Sensitive"]

stats[, segment := factor(segment,
                          levels = stats[order(-total_customers), segment])]

pdata <- stats[segment != "all customers"]

ggplot(pdata) +
  geom_bar(aes(segment, total_customers, fill = segment), stat = "identity") +
  scale_y_sqrt(
    labels = scales::comma_format(scale = .001, suffix = "K"),
    breaks = c(1000, 50000, 150000)
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  facet_grid(. ~ segment_type, scales = "free", space = "free") +
  xlab("") +
  ylab("") +
  ggtitle("Total Customers in Segments") +
  scale_fill_stata()







