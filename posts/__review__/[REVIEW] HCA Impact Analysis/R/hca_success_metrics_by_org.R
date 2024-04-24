library(data.table)
library(stringr)
library(lubridate)
library(hcaconfig)
library(DBI)
library(fst)
library(fs)
library(hcadatatools)
library(logr)

options(scipen = 1000)

log_open()

# Script Functions ----------------------------------------------------------------------------

dbGetOrgPop2 <- function(oid, FUN = NULL, ...) {

  # get shortname for logging
  sn <- orgShortName(oid)

  ## if no fun given for each org's subset, set as identity function
  if (is.null(FUN)) FUN <- identity

  ## internal function to fetch query by rows
  ..fetch <- function(cn, qry) {
    res <- dbSendQuery(cn, qry)
    on.exit(dbClearResult(res))
    h <- as.data.table(dbFetch(res, 1))
    iter <- 0
    while (!dbHasCompleted(res)) {
      cat("Query iteration:", iter <- iter + 1, "\n")
      h <- rbindlist(list(h, as.data.table(dbFetch(res, 10^5))))
    }
    return(h)
  }
  cn <- dbc("prod2", "integrated")
  on.exit(dbd(cn))

  qry <- str_glue(
    "SELECT *
     FROM population2
     WHERE order_time_utc IS NOT NULL
      AND org_uuid = '{oid}'
      AND item_subtotal < 1000
      AND item_subtotal > 5"
  )
  OUT <- ..fetch(cn, qry)

  ## log statement
  put(paste0("@> Query complete: ", sn))

  ## apply fun to the orgs population subset and return
  return(do.call(FUN, list(OUT, ...)))
}

buildAppData <- function(POP) {
  cats <- c(
    "FLOWER", "VAPES", "EXTRACTS", "PREROLLS", "EDIBLES", "DRINKS",
    "TOPICALS", "TINCTURES", "TABLETS_CAPSULES"
  )

  CAT <- POP[cats, {
    print_prog(100)
    st <- sum(item_subtotal)
    to <- length(unique(order_time_utc))
    tu <- sum(product_qty)
    list(
      ave_disc_rate = -1 * sum(item_discount) / sum(item_list_price),
      n_uniq_products = length(unique(product_name)),
      n_uniq_brands = length(unique(brand_name)),
      n_uniq_customers = length(unique(phone)),
      pct_retail_sales = sum(item_subtotal[order_type == "RETAIL"]) / st,
      pct_retail_units = sum(product_qty[order_type == "RETAIL"]) / tu,
      total_orders = to,
      total_units = tu,
      total_sales = st,
      sales_per_order = st / to
    )},
    keyby = .(
      org_uuid,
      order_facility,
      is_hca_customer,
      mon_date = floor_date(order_time_utc, "month")
    ),
    on = .(category3)
  ][, month_id := rowid(org_uuid, order_facility)][]

  ## Add org shortname to the table for presentation
  DT <- setkeyv(orgIndex()[, .(short_name), key = org_uuid][CAT], "short_name")

  ## align month_id so month 0 is when each org/store started with HCA
  keyCols <- c("short_name", "order_facility")
  setkeyv(DT, keyCols)

  DT[DT[, month_id[which(is_hca_customer)[1]] + 2, keyby = keyCols], month_id := month_id - V1]

  setkeyv(DT, "short_name")

  return(DT[])
}

write_out <- function(DT) {
  dir_out <- fs::path(getActiveProject(), "posts", "_draft", "HCA Impact Analysis", "app/data")
  put(paste0("Writing appdata.fst to: ", dir_out))

  new_path <- paste0(dir_out, "/appdata.fst")
  if (file_exists(new_path)) {
    old_path <- paste0(dir_out, "/old_appdata.fst")
    if (file_exists(old_path))
      file_delete(old_path)
    file_move(new_path, old_path)
  }
  stopifnot(!file_exists(new_path))
  fst::write_fst(DT, new_path)
  invisible(TRUE)
}

# Script Inputs -------------------------------------------------------------------------------

orgList <- list(
  shannonsbest       = list(oid = "9244cc3c-5ac1-47aa-b0b0-d6a4423b0547", since = "2022-11-01"),
  highwaycannabis    = list(oid = "f38cd5d0-f1e2-475c-ac14-d70d1595abb4", since = "2022-09-01"),
  treehousecraft     = list(oid = "d0bd6c5d-19e6-4fa6-99ad-860681c92c63", since = "2022-09-01"),
  exhalebrands       = list(oid = "3f6043b2-b7f7-4337-a866-f1d823d38fa9", since = "2022-12-01"),
  nug                = list(oid = "a5c9adc3-b042-4aa0-bdae-56e4a5417092", since = "2022-07-01"),
  wheelhouse         = list(oid = "184adb7c-bcb7-4e6f-89a6-6de9709ade7f", since = "2022-10-01"),
  pineappleexpress   = list(oid = "d3dde012-73b9-483b-814a-3971867e5a0e", since = "2022-04-01"),
  medithrive         = list(oid = "0ec7399e-395c-4b17-b287-5abd20e957ee", since = "2020-02-01"),
  mmd                = list(oid = "de65e6a2-2bec-4608-8d9f-74f4a2015851", since = "2022-01-01"),
  sundial_collective = list(oid = "b0214a42-292b-48e7-8a04-4a0d57d23ba7", since = "2023-01-01"),
  hightimes          = list(oid = "579c39f7-fe97-4803-ab70-6fe45dfe0e77", since = "2021-06-01"),
  apothecarium       = list(oid = "b6c6ad09-62dc-43a5-9f88-c3c6f5a53eda", since = "2021-10-01"),
  lionheart          = list(oid = "04cf5be8-5855-44ef-a40e-98145517dfd8", since = "2023-02-01"),
  thebrightspot      = list(oid = "468f6d46-f932-49d7-a0b2-45be9d0be774", since = "2022-02-01")
)

sep("Script Inputs")

put(orgList)

# Run Script ----------------------------------------------------------------------------------

sep("Getting Population Data")

POP <- rbindlist(lapply(seq_along(orgList), function(i) {
  o_id <- orgList[[i]]$oid
  s_ts <- orgList[[i]]$since
  f <- function(x, since) x[, is_hca_customer := order_time_utc > since][]
  setkey(dbGetOrgPop2(o_id, f, since = s_ts), order_facility, order_time_utc)[]
}))

sep("Building App Data")

DT <- buildAppData(POP)

# Write Out & Close Log -----------------------------------------------------------------------

sep("Writing Out Data")

write_out(DT)

log_close()




# Get Medithrive's Usage Trends ---------------------------------------------------------------


library(DBI)
library(data.table)
library(hcaconfig)
library(ggplot2)

cn <- hcaconfig::dbc("prod2", "appdata")

mscDT <- setDT(dbGetQuery(
  cn,
  "SELECT first_text as campaign_utc,
    customers,
    texts,
    pct_delivered,
    x24hr_roi
  FROM mstudio_sms_campaigns
  WHERE org = 'medithrive';"
), key = "campaign_utc")

pdata <- mscDT[, .(
  campaigns = .N,
  customers = sum(customers),
  texts = sum(texts),
  ave_delivery = mean(pct_delivered),
  total_roi = sum(x24hr_roi, na.rm = TRUE)
), keyby = .(
  date = floor_date(as_date(campaign_utc), "month")
)][-(.N-1):-.N]


ggplot(pdata) +
  geom_point(aes(date, campaigns)) +
  geom_smooth(aes(date, campaigns), method = "lm", se = FALSE) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Total Campaigns By Month", "Medithrive")
