oid <- orgIndex()[short_name == "unrivaled", org_uuid]
s <- as_date("2023-02-05")
e <- as_date("2023-02-22")
ndays <- difftime(e, s, units = "days")

getPopPeriod <- function(s0, s1) {
  cn <- dbc("prod2", "integrated")
  on.exit(dbd(cn))

  qry <- str_glue(
    "SELECT customer_id,
    phone,
    order_time_utc,
    item_subtotal,
    item_discount,
    product_qty
  FROM population2
  WHERE org_uuid = '{oid}'
  AND brand_name = 'KIVA'
  AND order_time_utc < '{s1}'
  AND order_time_utc > '{s0}'
  AND phone IS NOT NULL
  AND item_total > 5"
  )
  as.data.table(dbGetQuery(cn, qry))
}

fullDT <- getPopPeriod(as_date("2015-01-01"), today())

setkeyv(fullDT, c("customer_id", "phone"))

fullDT[, cid := .GRP, c("customer_id", "phone")]

orderSum <- fullDT[, .(
  order_sales = sum(item_subtotal),
  order_units = sum(product_qty),
  order_discount = -1 * sum(item_discount) / sum(item_subtotal)
), keyby = .(customer_id, phone, cid, order_date = as_date(order_time_utc))]

custList <- split(orderSum, orderSum$cid)

custSum <- rbindlist(lapply(custList, function(x) {
  setorderv(x, "order_date")
  x[, order_index := .I]
  x[]
}))

custSum[order_date >= s - ndays & order_date < s, period := "Pre-Promo"]
custSum[order_date >= s & order_date < e, period := "Promo Period"]
custSum[order_date >= e & order_date < e + ndays, period := "Post-Promo"]
custSum[order_date < s - ndays, period := "before"]
custSum[order_date >= e + ndays, period := "after"]

custSum[, period_days := as.integer(difftime(max(order_date), min(order_date), "days")), period]

levs <- c("before", "Pre-Promo", "Promo Period", "Post-Promo", "after")
custSum[, period := factor(period, levels = levs)]

custSum[, lost_kiva_customer := difftime(today(), max(order_date)) > 120, cid]

plotData <- custSum[period %in% c("Pre-Promo", "Promo Period"), .(
  totalCustomers = length(unique(cid)),
  totalCustPerDay = length(unique(cid)) / period_days[1],
  spendPerDay = sum(order_sales) / period_days[1],
  unitsPerDay = ceiling(sum(order_units) / period_days[1]),
  aveSpend = mean(order_sales),
  aveUnitsPerOrder = ceiling(mean(order_units)),
  aveDiscountRate = mean(order_discount)
), keyby = .(
  period,
  new_kiva_customers = order_index == 1,
  lost_kiva_customer
  )]

plotData[new_kiva_customers == TRUE, customers := "First Kiva Order"]
plotData[new_kiva_customers == FALSE, customers := "Repeat Kiva Buyer"]
plotData[lost_kiva_customer == FALSE, `Kiva Customer` := "Retained"]
plotData[lost_kiva_customer == TRUE, `Kiva Customer` := "Lost"]

ggplot(plotData, aes(period, totalCustomers, fill = `Kiva Customer`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = totalCustomers),
            colour = "white",
            size = 4,
            vjust = 1.5,
            position = position_dodge(.9)) +
  facet_grid( customers ~ period, scales = "free", switch = "y") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 10),
    strip.text.y.left = element_text(angle = 0)
  ) +
  ggtitle("Total Kiva Customers (by Period)") +
  xlab("") +
  ylab("")

plotData[, totalCustPerDay := round(totalCustPerDay, 1)]

ggplot(plotData, aes(period, totalCustPerDay, fill = `Kiva Customer`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = totalCustPerDay),
            colour = "white",
            size = 4,
            vjust = 1.5,
            position = position_dodge(.9)) +
  facet_grid( customers ~ period, scales = "free", switch = "y") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 10),
    strip.text.y.left = element_text(angle = 0)
  ) +
  ggtitle("Kiva Customers Per Day (by Period)") +
  xlab("") +
  ylab("")

ggplot(plotData, aes(period, unitsPerDay, fill = `Kiva Customer`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = unitsPerDay),
            colour = "white",
            size = 4,
            vjust = 1.5,
            position = position_dodge(.9)) +
  facet_grid( customers ~ period, scales = "free", switch = "y") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 10),
    strip.text.y.left = element_text(angle = 0)
  ) +
  ggtitle("Kiva Units Sold Per Day (by Period)") +
  xlab("") +
  ylab("")


plotData[, lab := scales::dollar(spendPerDay, accuracy = 1)]

ggplot(plotData, aes(period, spendPerDay, fill = `Kiva Customer`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = lab),
            colour = "white",
            size = 4,
            vjust = 1.5,
            position = position_dodge(.9)) +
  facet_grid( customers ~ period, scales = "free", switch = "y") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(size = 10),
    strip.text.y.left = element_text(angle = 0)
  ) +
  ggtitle("Kiva Sales Per Day (by Period)") +
  xlab("") +
  ylab("")

