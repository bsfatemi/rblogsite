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

K <- h[brand_name == "KIVA" & category3 == "EDIBLES"]

pat_terra <- "TERRA|BITES"
pat_gummy <- "GUMMIES|GUMMY|CHEWS|MIDNIGHT BLUEBERRY|WATERMELON|LOST FARM(S)?|CAMINO"
pat_cbars <- "\\bBAR\\b|MIDNIGHT MINT DARK CHOCOLATE|TOFFEE CRUNCH|CHURRO MILK CHOCOLATE"
pat_mints <- "\\bMINTS\\b|PETRA|MOROCCAN MINT"


shareSumm <- K[, .(
  totalSales = scales::dollar(sum(item_subtotal)),
  totalUnits = sum(product_qty),
  aveUnitPrice = scales::dollar(mean(price_per_unit)),
  aveUnitDisc = scales::percent(mean(-1 * item_discount) / mean(price_per_unit))
), .(
  is_terrabites = str_detect(product_name, pat_terra),
  is_gummies = str_detect(product_name, pat_gummy),
  is_chocobar = str_detect(product_name, pat_cbars),
  is_mints = str_detect(product_name, pat_mints)
)]
shareSumm[, market_share := scales::percent(totalUnits / sum(totalUnits))]

# shareSumm2 <- shareSumm[!!(is_terrabites|is_gummies|is_chocobar|is_mints)]

shareSumm[is_terrabites == TRUE, product := "Kiva Terra Bites"]
shareSumm[is_gummies == TRUE,    product := "Kiva Camino Gummies"]
shareSumm[is_chocobar == TRUE,   product := "Kiva Chocolate Bars"]
shareSumm[is_mints == TRUE,      product := "Kiva Mints"]
shareSumm[is.na(product),        product := "Kiva Other Products"]

shareSumm[, c("is_terrabites", "is_gummies", "is_chocobar", "is_mints") := NULL]
setcolorder(shareSumm, "product")

levs <- c("Kiva Camino Gummies",
          "Kiva Chocolate Bars",
          "Kiva Terra Bites",
          "Kiva Mints",
          "Kiva Other Products")
shareSumm[, product := factor(product, levels = levs)]
fwrite(shareSumm[order(product)], file = "kiva_product_share.csv")

shareTotal <- DT[category3 == "EDIBLES", .(
  totalSales = sum(item_subtotal),
  totalUnits = sum(product_qty)
), .(is_kiva = str_detect(product_name, "KIVA") | brand_name == "KIVA")]

shareTotal[, market_share := scales::percent(totalSales / sum(totalSales))]
shareTotal



# pat <- paste0(c(pat_terra, pat_gummy, pat_cbars, pat_mints), collapse = "|")
# K[!str_detect(product_name, pat)][, .N, product_name][order(-N)]


# KIVA CHOCOLATE BARS -------------------------------------------------------------------------


KCB <- DT[brand_name == "KIVA" & category3 == "EDIBLES" & str_detect(product_name, "BAR")]

KcbByDay <- KCB[, .(units = sum(product_qty),
                    price = mean(price_per_unit),
                    disc = -1*mean(item_discount) / mean(price_per_unit)),
                keyby = .(week = floor_date(order_time_utc, "week"))]
setkeyv(KcbByDay, "org_uuid")
KcbByDay[facs, short_name := short_name]


pdata <- KcbByDay[units > 300]
pdata[, demand := units / max(units)]

pdata[demand == 1, is_outlier := TRUE]
pdata[demand > .6 & demand < .8 & price < 20, is_outlier := TRUE]
pdata[demand > .4 & demand < .6 & price < 20.5, is_outlier := TRUE]
pdata[is.na(is_outlier), is_outlier := FALSE]
pdata2 <- pdata[demand < .75 & demand > .2][is_outlier == FALSE]

pdata2[, demand := units / max(units)]


demand <- c("80% or greater", "60% - 80%", "40% - 60%", "40% or lower")
discDT <- data.table(
  "demand" = factor(demand, levels = rev(demand)),
  "aveDiscount" = c(
    pdata2[demand > .8, mean(disc)],
    pdata2[demand > .6 & demand < .8, mean(disc)],
    pdata2[demand > .4 & demand < .6, mean(disc)],
    pdata2[demand < .4, mean(disc)]
  ),
  "avePrice" = c(
    pdata2[demand > .8, mean(price)],
    pdata2[demand > .6 & demand < .8, mean(price)],
    pdata2[demand > .4 & demand < .6, mean(price)],
    pdata2[demand < .4, mean(price)]
  )
)

discDT[, avePrice := paste0("Ave Price: ", scales::dollar(avePrice))]

ggplot(discDT, aes(demand, aveDiscount)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(.03, .06, .09, .12, .15, .18)) +
  xlab("Demand Level") +
  ylab("Discount") +
  geom_text(aes(label = avePrice), vjust = 2, color = "white") +
  theme_economist_white()

ggplot(pdata2, aes(demand, price)) +
  geom_point(aes(color = disc)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("Demand (Units Per Week)") +
  ylab("Unit Price") +
  ggtitle("Optimal Price Model for Kiva Chocolate Bars", "Demand Estimated as Units Per Week") +
  geom_smooth(formula = y ~ log(x)) +
  scale_color_continuous_tableau() +
  theme_calc()



l1 <- paste0("Ave Demand at this Price: ",
             pdata2[price > 24, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata2[price > 24, scales::percent(mean(disc))])

lab1 <- data.table(x = .78, y = 24.3, label = l1)

l2 <- paste0("Ave Demand at this Price: ",
             pdata2[price < 21, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata2[price < 21, scales::percent(mean(disc))])


lab2 <- data.table(x = .55, y = 20.3, label = l2)

ggplot(pdata2, aes(demand, price)) +
  geom_point(aes(color = disc)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("Demand (Units Per Week)") +
  ylab("Unit Price") +
  ggtitle("Optimal Price Model for Kiva Chocolate Bars", "Demand Estimated as Units Per Week") +
  geom_smooth(formula = y ~ log(x)) +
  geom_hline(data = data.table(y = 20), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 21), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 24), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_hline(data = data.table(y = 25), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_text(data = lab2, aes(x, y, label = label), color = "green4") +
  geom_text(data = lab1, aes(x, y, label = label), color = "red4") +
  scale_color_continuous_tableau() +
  theme_calc() +
  theme(legend.position = "none")


# KIVA CAMINO GUMMIES -------------------------------------------------------------------------


KCG <- DT[brand_name == "KIVA" & category3 == "EDIBLES" & str_detect(product_name, "CAMINO")]

KcgByDay <- KCG[, .(units = sum(product_qty),
                    price = mean(price_per_unit),
                    disc = -1*mean(item_discount) / mean(price_per_unit)),
                keyby = .(week = floor_date(order_time_utc, "week"))]

pdata <- KcgByDay[units > 300]
pdata[, demand := units / max(units)]


pdata2 <- pdata[year(week) > 2019 & demand > .3 & demand < .8]

pdata3 <- pdata2[!(price < 18.5 & demand < .55)]

pdata3[, demand := (units / max(units))^2]

l1 <- paste0("Ave Demand at this Price: ",
             pdata3[price > 20.3, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata3[price > 20.3, scales::percent(mean(disc))])

lab1 <- data.table(x = .78, y = 20.7, label = l1)

l2 <- paste0("Ave Demand at this Price: ",
             pdata3[price < 18.4, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata3[price < 18.4, scales::percent(mean(disc))])


lab2 <- data.table(x = .5, y = 17.8, label = l2)

ggplot(pdata3[!(demand < .8 & price < 18.1)], aes(demand, price)) +
  geom_point(aes(color = disc)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("Demand (Units Per Week)") +
  ylab("Unit Price") +
  ggtitle("Optimal Price Model for Kiva Camino Gummies", "Demand Estimated as Units Per Week") +
  geom_smooth(formula = y ~ log(x) * log(x + 1), method = "gam") +
  # scale_color_continuous_tableau() +
  geom_hline(data = data.table(y = 17.4), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 18.4), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 20.3), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_hline(data = data.table(y = 21.5), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_text(data = lab2, aes(x, y, label = label), color = "green4") +
  geom_text(data = lab1, aes(x, y, label = label), color = "red4") +
  scale_color_continuous_tableau() +
  theme_clean() +
  theme(legend.position = "none")



demand <- c("80% or greater", "60% - 80%", "40% - 60%", "40% or lower")
discDT <- data.table(
  "demand" = factor(demand, levels = rev(demand)),
  "aveDiscount" = c(
    pdata3[demand > .9, mean(disc)],
    pdata3[demand > .6 & demand < .9, mean(disc)],
    pdata3[demand > .3 & demand < .6, mean(disc)],
    pdata3[demand < .3, mean(disc)]
  ),
  "avePrice" = c(
    pdata3[demand > .9, mean(price)],
    pdata3[demand > .6 & demand < .9, mean(price)],
    pdata3[demand > .3 & demand < .6, mean(price)],
    pdata3[demand < .3, mean(price)]
  )
)

discDT[, avePrice := paste0("Ave Price: ", scales::dollar(avePrice))]

ggplot(discDT, aes(demand, aveDiscount)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(.03, .06, .09, .12, .15, .18, .21)) +
  xlab("Demand Level") +
  ylab("Discount") +
  geom_text(aes(label = avePrice), vjust = 2, color = "white") +
  theme_economist_white()




# KIVA TERRA BITES -------------------------------------------------------------------------


KTB <- DT[brand_name == "KIVA" & category3 == "EDIBLES" & str_detect(product_name, "TERRA BITES")]

KtbByDay <- KTB[, .(units = sum(product_qty),
                    price = mean(price_per_unit, trim = .2),
                    disc = -1*mean(item_discount) / mean(price_per_unit)),
                keyby = .(week = floor_date(order_time_utc, "week"))]

pdata <- KtbByDay[units > 300]
pdata[, demand := units / max(units)]


pdata2 <- pdata[!(demand < .5 & price < 21.5)]#[year(week) > 2019 & demand > .3 & demand < .8]

pdata3 <- pdata2[!(price < 22.1 & demand < .4)]

pdata4 <- pdata3[!(price < 20 & demand > .6 & demand < .8)]

pdata5 <- pdata4[!(price < 21 & demand < .6)]

pdata6 <- pdata5[!(price > 26 & demand < .5 & demand > .25)]

pdata7 <- pdata6#[!(demand > .5)]

# pdata7[, demand := (units / max(units))^2]



l1 <- paste0("Ave Demand at this Price: ",
             pdata7[price > 25, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata7[price > 25, scales::percent(mean(disc))])

lab1 <- data.table(x = .78, y = 25.5, label = l1)

l2 <- paste0("Ave Demand at this Price: ",
             pdata7[price < 21.8, scales::percent(mean(demand))],
             " <> Ave Discount at this Price: ",
             pdata7[price < 21.8, scales::percent(mean(disc))])


lab2 <- data.table(x = .55, y = 19.6, label = l2)


ggplot(pdata7, aes(demand, price)) +
  geom_point(aes(color = disc)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab("Demand (Units Per Week)") +
  ylab("Unit Price") +
  ggtitle("Optimal Price Model for Kiva Terra Bites", "Demand Estimated as Units Per Week") +
  geom_smooth(formula = y ~ log(x) * log(x + 1), method = "gam") +
  geom_hline(data = data.table(y = 19.1), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 21.8), aes(yintercept = y), linetype = 1, color = "green4", linewidth = 1) +
  geom_hline(data = data.table(y = 25), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_hline(data = data.table(y = 27), aes(yintercept = y), linetype = 1, color = "red4", linewidth = 1) +
  geom_text(data = lab2, aes(x, y, label = label), color = "green4") +
  geom_text(data = lab1, aes(x, y, label = label), color = "red4") +
  scale_color_continuous_tableau() +
  theme_clean() +
  theme(legend.position = "none")


demand <- c("80% or greater", "60% - 80%", "40% - 60%", "40% or lower")
discDT <- data.table(
  "demand" = factor(demand, levels = rev(demand)),
  "aveDiscount" = c(
    pdata3[demand > .9, mean(disc)],
    pdata3[demand > .6 & demand < .9, mean(disc)],
    pdata3[demand > .3 & demand < .6, mean(disc)],
    pdata3[demand < .3, mean(disc)]
  ),
  "avePrice" = c(
    pdata3[demand > .9, mean(price)],
    pdata3[demand > .6 & demand < .9, mean(price)],
    pdata3[demand > .3 & demand < .6, mean(price)],
    pdata3[demand < .3, mean(price)]
  )
)

discDT[, avePrice := paste0("Ave Price: ", scales::dollar(avePrice))]

ggplot(discDT, aes(demand, aveDiscount)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(.03, .06, .09, .12, .15, .18, .21)) +
  xlab("Demand Level") +
  ylab("Discount") +
  geom_text(aes(label = avePrice), vjust = 2, color = "white") +
  theme_economist_white()
