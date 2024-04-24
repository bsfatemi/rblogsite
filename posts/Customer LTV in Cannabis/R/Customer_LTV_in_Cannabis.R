library(hcaconfig)
library(data.table)
library(hcadatatools)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(logr)
library(rstudioapi)
library(ggpubr)

setwd(paste0(getActiveProject(), "/posts/_draft/Customer LTV in Cannabis/"))

..get_pop <- function(org_ids, date_start, date_stop) {
  cn <- dbc("prod2", "integrated")
  on.exit(dbd(cn))
  qry <- str_glue(
    "SELECT org_uuid,
      customer_id,
      order_id,
      order_time_utc,
      order_line_total
   FROM population
   WHERE org_uuid IN ({str_flatten_comma(sapply(org_ids, function(x) str_c(\"'\", x, \"'\")))})
    AND '[{date_start}, {date_stop})'::daterange @> order_time_utc::date
    AND order_line_total > 5
    AND order_line_total < 1000
    AND customer_id IS NOT NULL"
  )
  dbQueryFetch(cn, qry, 10^5)
}

## Set inputs to analysis
t1 <- floor_date(today(), "quarter")
t0 <- floor_date(t1 - years(5) - days(1), "quarter")

## Get population data across a period of quarters and set quarter id
# org_ids <- orgIndex()[(curr_client), sample(org_uuid, 20, FALSE)]
orgShortName("9703059f-18da-4b4b-9b78-59142c99792e")
DT <- setkey(..get_pop(org_ids[1], t0, t1), org_uuid, order_time_utc)
DT[, qtr_id := .GRP, quarter(order_time_utc, type = "year.quarter")]

## Summarize customers by order then by quarter
sumDT <- DT[!str_detect(customer_id, "\\-NA$")][, .(
  order_total = sum(order_line_total)
), .(qtr_id, order_time_utc, customer_id, order_id)][, .(
  total_spend = sum(order_total),
  total_orders = .N
), keyby = .(qtr_id, customer_id)]

sumDT[, spend_per_order := total_spend / total_orders]

## split into subsets by quarter into list
ll <- split(setkey(sumDT, customer_id), by = "qtr_id", sorted = TRUE)

## these customers survived until the end
OG <- do.call("Reduce", list(function(l, r) r[l, .SD, nomatch = NULL], ll))

sumOG <- sumDT[OG[, .(customer_id)], .(
  customers = .N,
  sales = sum(total_spend),
  orders = sum(total_orders)
), qtr_id, on = "customer_id"][, c("sales_per_order", "orders_per_cust", "sales_per_cust") := .(
  sales / orders,
  orders / customers,
  sales / customers
)]

sumDT[OG[, .(customer_id)], is_og := TRUE, on = "customer_id"]
sumDT[is.na(is_og), is_og := FALSE]

ll <- split(sumDT, by = "qtr_id", sorted = TRUE)

pdata <- rbindlist(lapply(1:length(ll), function(x) do.call("Reduce", list(..RL, ll[1:x]))))[, .(
  customers = .N,
  sales = sum(total_spend),
  orders = sum(total_orders)
), keyby = .(qtr_id, is_og)]

pdata[
  setkey(DT[, .N, .(qtr_id, quarter(order_time_utc, type = "year.quarter"))][, !"N"], qtr_id),
  quarter := quarter
]
tmp <- pdata[, as.data.table(str_split(as.character(quarter), "\\.", 2, simplify = TRUE))]
pdata[, qtr := tmp[, str_c("Q", V2, " ", V1)]]

setkey(pdata, qtr_id)
pdata[, qtr := factor(qtr, levels = pdata[, .N, .(qtr_id, qtr)][, qtr])]

labs <- pdata[, .(
  customers = sum(customers),
  sales = sum(sales),
  orders = sum(orders)
  ), qtr]

labs[, customers_lab := scales::percent(customers / customers[1], accuracy = 1)]
labs[, sales_lab := scales::percent(sales / sales[1], accuracy = 1)]
labs[, orders_lab := scales::percent(orders / orders[1], accuracy = 1)]

ggplot(pdata) +
  geom_bar(aes(qtr, customers, fill = is_og), stat = "identity") +
  geom_text(aes(qtr, customers, label = customers_lab),
            data = labs,
            vjust = 1.5,
            color = "white") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_smooth(aes(x, y),
              data = pdata[, .(y = sum(customers)), by = .(x = qtr_id)],
              formula = y ~ log(x),
              method = "loess",
              se = FALSE) +
  ylab("Customers") +
  ggtitle("Total Customers with Orders by Quarter")

ggplot(pdata) +
  geom_bar(aes(qtr, sales, fill = is_og), stat = "identity") +
  geom_text(aes(qtr, sales, label = sales_lab),
            data = labs,
            vjust = 1.5,
            color = "white") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K")) +
  geom_smooth(aes(x, y),
              data = pdata[, .(y = sum(sales)), by = .(x = qtr_id)],
              formula = y ~ log(x),
              method = "loess",
              se = FALSE) +
  ylab("Sales") +
  ggtitle("Total Sales from Customers by Quarter")

ggplot(pdata) +
  geom_bar(aes(qtr, orders, fill = is_og), stat = "identity") +
  geom_text(aes(qtr, orders, label = orders_lab),
            data = labs,
            vjust = 1.5,
            color = "white") +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_smooth(aes(x, y),
              data = pdata[, .(y = sum(orders)), by = .(x = qtr_id)],
              formula = y ~ log(x),
              method = "loess",
              se = FALSE) +
  ylab("Orders") +
  ggtitle("Total Orders from Customers by Quarter")


setkey(sumDT, qtr_id, customer_id)
sumDT[qtr_id < 9, .(
  ave_qtrly_spend = mean(total_spend),
  ave_qtrly_orders = mean(total_orders)
), qtr_id][, .(lt_sales = sum(ave_qtrly_spend), lt_orders = sum(ave_qtrly_orders))]


# survDT <- labs[, .(qtr, Customers = customers_lab, Orders = orders_lab, Sales = sales_lab)]
# survDT <- pdata[, .(customers = sum(customers), sales = sum(sales), orders = sum(orders)), by = qtr]
#
# survDT[, Customers := customers / customers[1]]
# survDT[, Sales := sales / sales[1]]
# survDT[, Orders := orders / orders[1]]
#
# spDT <- melt(survDT,
#              id.vars = "qtr",
#              measure.vars = c("Customers", "Sales", "Orders"),
#              variable.name = "attrition",
#              value.name = "rate")
#
# ggplot(spDT, aes(qtr, rate, group = attrition)) +
#   geom_point(aes(color = attrition)) +
#   geom_smooth(aes(color = attrition), se = FALSE) +
#   ggthemes::scale_color_economist() +
#   ggtitle("Percent Attrition by Quarter") +
#   ylab("Attrition Rate") +
#   scale_y_continuous(labels = scales::percent_format()) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.position = "top",
#         legend.justification = c(0, 0),
#         legend.title = element_blank())
#
# ggarrange(p0, p1, p2, ncol = 1, align = "v")

