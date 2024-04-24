library(data.table)
library(DBI)
library(stringr)
library(lubridate)
library(hcaconfig)

oid <- orgIndex()[short_name == "unrivaled", org_uuid]

qry <- str_glue("SELECT *
                FROM population2
                WHERE org_uuid = '{oid}'
                AND phone IS NOT NULL
                AND item_total > 5")

cn <- dbc("prod2", "integrated")
DT <- dbGetQuery(cn, qry)
setDT(DT)


c_start <- as.Date("2023-02-05")
c_stop  <- as.Date("2023-02-21")

promo_days <- as.numeric(c_stop - c_start, units = "days")

prior_start <- c_start - days(promo_days) - days(1)
prior_stop <- prior_start + days(promo_days)

post_start <- c_stop + days(1)
post_stop <- c_stop + days(promo_days) + days(1)


# PROMOTION 1 ---------------------------------------------------------------------------------


C_ORDERS <- DT[, .(
  totalItems = .N,
  orderTotal = sum(item_total, na.rm = TRUE),
  aveItemTotal = sum(item_total, na.rm = TRUE) / sum(product_qty, na.rm = TRUE)
), keyby = .(
  org_uuid,
  facility,
  order_id,
  customer_id,
  phone,
  has_brand = str_detect(brand_name, "CAMINO|KIVA|LOST FARMS"),
  order_date = as.Date(order_time_utc)
)]


setkeyv(C_ORDERS, "order_date")
C_ORDERS[, orderIndex := .(1:.N), .(org_uuid, facility, customer_id)]


PRE <- C_ORDERS[order_date < prior_stop & order_date > prior_start]
PROMO <- C_ORDERS[order_date < c_stop   & order_date > c_start]
POST <- C_ORDERS[order_date < post_stop & order_date > post_start]

PRE[, period := "Pre-Promotion"]
PROMO[, period := "Promotional Period"]
POST[, period := "Post-Promotion"]

EDT <- rbindlist(list(PRE, PROMO, POST))
EDT[, period := factor(period, c("Pre-Promotion", "Promotional Period", "Post-Promotion"))]

pdata <- EDT[, sum(totalItems), keyby = .(order_date, has_brand, period)][c(-1, -83, -85)]
pdata2 <- pdata[!(period == "Pre-Promotion" & has_brand == TRUE & V1 > 1100)]

pdata2[has_brand == TRUE, facet := "Promoted Products"]
pdata2[has_brand == FALSE, facet := "No Promoted Products"]

aveDT <- pdata2[, .(startdate = min(order_date), mean(V1)), .(facet, period)]
aveDT[, label := ceiling(V2)]

ggplot(pdata2) +
  geom_point(aes(order_date, V1, color = period)) +
  facet_grid(facet ~ period, scales = "free", space = "free") +
  geom_hline(data = aveDT, aes(yintercept = V2)) +
  scale_y_log10() +
  geom_text(data = aveDT,
            aes(startdate, V2, label = label, vjust = -1, hjust = -.05),
            color = "black") +
  xlab("") +
  ylab("Total Units Sold") +
  ggthemes::theme_igray() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0)) +
  scale_color_economist() +
  ggtitle("Unrivaled - Kiva Promotion - Units Sold Per Day",
          "Promotion: Camino Sours Sales Incentive")



# PROMOTION 2 ---------------------------------------------------------------------------------

c_start <- as.Date("2022-09-20")
c_stop  <- as.Date("2022-10-22")

promo_days <- as.numeric(c_stop - c_start, units = "days")

prior_start <- c_start - days(promo_days) - days(1)
prior_stop <- prior_start + days(promo_days)

post_start <- c_stop + days(1)
post_stop <- c_stop + days(promo_days) + days(1)

OPOP <- h[org_uuid == oid]

brand <- "CREAM OF THE CROP|WEST COAST TRADING"

C_ORDERS <- OPOP[category3 == "FLOWER", .(
  totalItems = sum(product_qty, na.rm = TRUE),
  promoProductsTotal = sum(item_total, na.rm = TRUE),
  orderTotal = order_tot[1],
  aveItemTotal = sum(item_list_price, na.rm = TRUE) / sum(product_qty, na.rm = TRUE)
), keyby = .(
  org_uuid,
  facility,
  order_id,
  customer_id,
  phone,
  has_brand = str_detect(brand_name, brand),
  order_date = as.Date(order_time_utc)
)]

# OPOP[str_detect(brand_name, "CREAM OF THE CROP|COTC|WCTC|WEST COAST TRADING")]

setkeyv(C_ORDERS, "order_date")
C_ORDERS[, orderIndex := .(1:.N), .(org_uuid, facility, customer_id)]

PRE   <- C_ORDERS[order_date < prior_stop & order_date > prior_start]
PROMO <- C_ORDERS[order_date < c_stop & order_date > c_start]
POST  <- C_ORDERS[order_date < post_stop & order_date > post_start]

PRE[,   period := "Pre-Promotion"]
PROMO[, period := "Promotional Period"]
POST[,  period := "Post-Promotion"]


DT <- rbindlist(list(PRE, PROMO, POST))
DT[, period := factor(period, c("Pre-Promotion", "Promotional Period", "Post-Promotion"))]

DT[, sum(promoProductsTotal), keyby = .(has_brand, period)]
DT[, sum(orderTotal), keyby = .(has_brand, period)]
DT[, sum(totalItems), keyby = .(has_brand, period)]

pdata <- DT[, sum(totalItems), .(has_brand, order_date, period)]

pdata[has_brand == TRUE, facet := "Promoted Products"]
pdata[has_brand == FALSE, facet := "No Promoted Products"]

pdata <- pdata[!(period == "Promotional Period" & has_brand == TRUE & V1 < 50) &
                 !(period %in% c("Pre-Promotion", "Post-Promotion") & has_brand == TRUE & V1 > 100) &
                 !(period %in% c("Pre-Promotion", "Post-Promotion") & has_brand == FALSE & V1 > 700)]

aveDT <- pdata[, .(startdate = min(order_date), mean(V1)), .(facet, period)]
aveDT[, label := ceiling(V2)]

ggplot(pdata) +
  geom_point(aes(order_date, V1, color = period)) +
  facet_grid(facet ~ period, scales = "free") +
  geom_hline(data = aveDT, aes(yintercept = V2)) +
  scale_y_log10() +
  geom_text(data = aveDT,
            aes(startdate, V2, label = label, vjust = -1, hjust = -.05),
            color = "black") +
  xlab("") +
  ylab("Total Units Sold") +
  ggthemes::theme_igray() +
  theme(legend.position = "none",
        strip.text.y = element_text(angle = 0)) +
  scale_color_economist() +
  ggtitle("Unrivaled - Flower Promotion - Units Sold Per Day", "Promotion: Commissions on WCTC and COTC")







