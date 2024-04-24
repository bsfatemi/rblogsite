# #
# library(data.table)
# library(stringr)
# library(DBI)
# library(fst)
# library(hcaconfig)
# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(jsonlite)
# library(purrr)
# library(logr)
#
# options(scipen = 1000)
#
#
#
# log_open()
#
#
# # Get campaigns raw data ----------------------------------------------------------------------
#
#
#
# cn <- dbc("prod2", "appdata")
#
# put("Query mstudio_user_campaigns")
# qry <- "SELECT org, campaign_id, campaign_source, filters_applied FROM mstudio_user_campaigns;"
# muc <- setkeyv(setDT(dbGetQuery(cn, qry)), c("org", "campaign_id"))
#
# put("Query mstudio_sms_campaigns")
# qry <- "SELECT * FROM mstudio_sms_campaigns;"
# msc <- setkeyv(setDT(dbGetQuery(cn, qry)), c("org", "campaign_id"))
#
# dbd(cn)
#
# put("Join mstudio_* tables, add and drop columns")
# raw <- muc[msc][!is_many_days & !is.na(org)][, (c(
#   "is_campaign_am",
#   "campaign_hour",
#   "campaign_date",
#   "campaign_age_days",
#   "campaign_dur_mins"
# )) := .(
#   am(first_text),
#   hour(first_text),
#   as_date(first_text),
#   as.numeric(today() - as_date(first_text), units = "days"),
#   as.numeric(last_text - first_text, units = "mins")
# )][, (c(
#   "is_many_days",
#   "days",
#   "run_date_utc",
#   "updated_at",
#   "first_text",
#   "last_text"
# )) := NULL][]
#
# put("Parse filter json into columns and bind by campaigns")
# DT <- raw[, rbindlist(lapply(apply(.SD, 1, function(r) {
#   x <- r[["filters_applied"]]
#   if (!is.na(x)) {
#     tmp <- fromJSON(x)
#     if (length(tmp$filters) > 0) {
#       tmp$filters <- lapply(tmp$filters, function(i) {
#         if (length(i) > 0)
#           return(paste0(unique(i), collapse = ","))
#         return(NA)
#       })
#     } else {
#       tmp$filters <- NULL
#     }
#
#     if (!is.null(names(tmp$filters)))
#       tmp$filter <- setNames(tmp$filter, paste0("filt_", names(tmp$filter)))
#
#     OUT <- c(source = tmp$source, title = tmp$title, tmp$filter)
#   } else {
#     OUT <- NULL
#   }
#   return(c(as.list(r[names(r) != "filters_applied"]), OUT))
# }), as.data.table), fill = TRUE)]
#
#
# put("Query campaign logs <mstudio_sms_logs>")
# cn <- dbc("prod2", "cabbage_patch")
# qry <- "SELECT org,
#             campaign_id,
#             data_to_1_carrier AS carrier,
#             mstudio_sms_logs.to as phone_to,
#             http_code,
#             data_from_phone_number AS phone_from
#         FROM mstudio_sms_logs
#         WHERE mstudio_sms_logs.to IS NOT NULL"
# logs <- setkeyv(setDT(dbGetQuery(cn, qry)), "carrier")
# dbd(cn)
#
# put("Query subcarriers <normalized_phone_carriers>")
# cn <- dbc("prod2", "integrated")
# subs <- setkeyv(setDT(dbReadTable(cn, "normalized_phone_carriers")), "carrier")
# dbd(cn)
#
# put("Join subcarrier info into logs data and clean rows")
# logs[subs, norm_carrier := normalized_carrier]
# logs[!is.na(norm_carrier), carrier := str_to_upper(norm_carrier)]
# logs[, norm_carrier := NULL]
# logs[carrier == "", carrier := NA]
# logs[, carrier := str_trim(str_to_upper(carrier), "both")]
# logs[, carrier := str_squish(carrier)]
#
#
#
# put("building campaigns raw")
#
# ## Tables below share the same keys
# keyCols <- c("org", "campaign_id")
#
# ## get ROI details into one table
# roi_dt <- unique(melt(
#   data = data,
#   id.vars = keyCols,
#   measure.vars = patterns(
#     roi_s = "^x.+roi$",
#     roi_c = "^x.+customers$",
#     roi_recap_s = "^x.+sales$",
#     roi_recap_c = "^x.+recapture$"
#   ),
#   variable.name = "roi_period",
#   variable.factor = TRUE
# ))
# roi_dt[, roi_period := factor(roi_period, labels = c("24hr", "72hr", "7day", "2wk", "30day"))]
#
# ## get delivery details into one table
# deliver_cnams <- c(
#   "customers_targeted",
#   "customers",
#   "texts",
#   "texts_sent",
#   "billed_texts",
#   "pct_delivered",
#   "pct_reached",
#   "price",
#   "cost_per_customer",
#   "unsubscribes",
#   "opt_out_rate"
# )
# deliver_dt <- unique(data[, c(keyCols, deliver_cnams), with = FALSE])
#
# ## get basic info into one table
# campaign_cnams <- c(
#   "campaign_date",
#   "campaign_hour",
#   "campaign_dur_mins",
#   "campaign_age_days",
#   "text_length",
#   "is_campaign_am",
#   "is_mms",
#   "is_single_text"
# )
# info_dt <- unique(data[, c(keyCols, campaign_cnams), with = FALSE])
#
# ## get campaign meta data into one table
# meta_cnams <- c(
#   "campaign_source",
#   "body",
#   "media_url",
#   "title",
#   "filters_applied"
# )
# meta_dt <- data[, c(keyCols, meta_cnams), with = FALSE][, .SD[1], .(org, campaign_id)]
#
# ## clean up body
# body_pat <- "\\(?(text|txt|reply)? ?stop to (unsubscribe|cancel)\\)?"
# meta_dt[, body := str_trim(str_remove(str_to_lower(body), body_pat), "both")]
# meta_dt[body == "", body := NA]
#
# ## extract attributes of campaigns
# url_pat <- "(http(s)?\\:)|\\.(org|com|net|online|delivery)\\b|\\.(ly|at|us|gy|de)\\/"
# disc_pat <- "[0-9][0-9]\\%|(percent off)"
# free_pat <- "\\bfree\\b"
# price_pat <- "\\$[0-9]+\\.?"
#
# setkeyv(info_dt, keyCols)
# setkeyv(meta_dt, keyCols)
#
# info_dt[meta_dt, `:=`(
#   has_image = !is.na(media_url),
#   has_discount = str_detect(body, disc_pat),
#   has_freebie = str_detect(body, free_pat),
#   has_price = str_detect(body, price_pat),
#   has_url = str_detect(body, url_pat),
#   has_emoji = emoji::emoji_detect(body)
# )]
#
# setkeyv(roi_dt, keyCols)
# setkeyv(deliver_dt, keyCols)
#
# ll_raw <- list(
#   "details" = info_dt[],
#   "meta" = meta_dt[],
#   "delivery" = deliver_dt[],
#   "roi" = roi_dt[]
# )
#
# # Build campaigns stats -----------------------------------------------------------------------
#
# put("building campaigns stats")
#
# logs[is.na(carrier), carrier := "UNKNOWN"]
# logs[, is_success := http_code < 300]
#
# stats <- logs[, {
#   ind_tmb <- str_detect(carrier, "T\\-MOBILE")
#   ind_att <- str_detect(carrier, "AT\\&T")
#   ind_vzn <- str_detect(carrier, "VERIZON")
#   ind_unk <- str_detect(carrier, "UNKOWN")
#
#   list(
#     n_phone_to   = .N,
#     n_phone_from = length(unique(phone_from)),
#     n_carriers   = length(unique(carrier)),
#     pct_success  = sum(is_success) / .N,
#     pct_fails    = sum(!is_success) / .N,
#     pct_tmobile  = sum(ind_tmb) / .N,
#     pct_att      = sum(ind_att) / .N,
#     pct_verizon  = sum(ind_vzn) / .N,
#     pct_others   = sum(!ind_tmb & !ind_att & !ind_vzn & !ind_unk) / .N,
#     has_tmobile  = any(ind_tmb),
#     has_att      = any(ind_att),
#     has_verizon  = any(ind_vzn)
#   )
# }, keyby = .(org, campaign_id)]
#
#
# log_close()
#
#
# # Analysis ------------------------------------------------------------------------------------
#
#
#
# # THRPT <- logs[!is.na(phone_from), .(
# #   n_phone_to = .N,
# #   tot_success = sum(http_code < 300),
# #   tot_failure = sum(http_code > 399)
# #   ), .(campaign_id, phone_from)]
# # THRPT[, success_rate := tot_success / n_phone_to]
# # THRPT[, mean(success_rate), phone_from]
#
#
# sep("Finalizing campaigns data")
#
# keyCols <- c("org", "campaign_id")
# setkeyv(ll_raw$delivery, keyCols)
# setkeyv(ll_raw$details, keyCols)
# setkeyv(stats, keyCols)
#
# ll <- list(
#   summ = stats[ll_raw$delivery[ll_raw$details]],
#   meta = ll_raw$meta,
#   roi = ll_raw$roi
# )
#
# SUMM <- ll$summ
#
#
# SUMM[, is_july_23 := year(campaign_date) == 2023 & month(campaign_date) == 7]
#
#
#
#
# summary(lm(
#   pct_reached ~
#     # is_july_23* pct_att+
#     # is_july_23* pct_verizon+
#     # is_july_23* pct_tmobile+
#     # has_tmobile +
#     # has_att +
#     # has_verizon +
#     # is_campaign_am +
#     log(n_phone_to) +
#     # log(n_phone_to)
#     # pct_att +
#     # pct_verizon +
#
#     is_mms * has_emoji +
#     is_mms * has_url +
#     is_mms * has_price +
#     # is_mms * has_freebie +
#     # is_mms * has_discount +
#     # year(campaign_date) +
#     # log(text_length) +
#     # log(campaign_dur_mins) +
#     log(billed_texts)  +
#     log(n_carriers) +
#     log(n_phone_from),
#   SUMM[campaign_date > today() - days(90)][billed_texts > 0 & campaign_dur_mins > 0]
# ))
#
# # rstudioapi::documentOpen(lf)
#
# ll$summ[, .N, month(campaign_date)]
#
# SUMM <- ll$summ
# ROI  <- ll$roi
# META <- ll$meta
#
# setkeyv(ROI, "org")
# setkeyv(SUMM, "org")
#
# orgs <- SUMM[, max(campaign_date), keyby = org][year(V1) == 2023, .(org)]
#
# sDT <- SUMM[orgs]
# rDT <- ROI[orgs]
#
# roiDT <- rDT[roi_period == "7day", .(
#   ave_7day_recap_s = mean(roi_recap_s, na.rm = TRUE)
# ), keyby = .(org)]
#
#
#
# ## Summarize by org
# mdata <- pdata[, .(
#   total_campaigns = .N,
#   ave_target_count = mean(customers_targeted),
#   ave_pct_delivered = mean(pct_delivered, trim = .1),
#   pct_has_image = sum(has_image) / .N,
#   pct_has_disc = sum(has_discount) / .N,
#   pct_has_price = sum(has_price) / .N,
#   pct_has_free = sum(has_freebie) / .N,
#   pct_has_url = sum(has_url) / .N,
#   pct_has_emoji = sum(has_emoji) / .N,
#   ave_body_len = mean(text_length, trim = .1)
# ), .(org, month_date = floor_date(campaign_date, unit = "month"))]
#
#
# mdata[pct_has_disc > 0 & pct_has_image > 0 & pct_has_price > 0]
#
# write_fst(mdata, "posts/Maximizing SMS Deliverability/mdata.fst")


library(ggplot2)
library(plotly)
library(scales)
library(fst)
library(data.table)
library(scales)

mdata <- read_fst("mdata.fst", as.data.table = TRUE)

pdt <- mdata[ave_target_count < 5000 & ave_target_count > 100]
p0 <- ggplot(pdt, aes(ave_target_count, ave_pct_delivered)) +
  geom_point(aes(color = org, size = total_campaigns)) +
  geom_smooth(formula = y ~ poly(x, 2), method = "glm", level=0.80) +
  scale_x_log10(labels = comma_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_size(guide = "none") +
  xlab("Average Targets Per Campaign") +
  ylab("Average Deliverability Rate")

pp0 <- plotly::ggplotly(p0)
saveRDS(pp0, "posts/Maximizing SMS Deliverability/widgets/sms_target_size.rds")


pdt <- mdata[ave_pct_delivered > .55 & pct_has_image > .02]
p1 <- ggplot(pdt, aes(pct_has_image, ave_pct_delivered)) +
  geom_point(aes(color = org, size = total_campaigns)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_x_log10(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_size(guide = "none") +
  xlab("Percent of Campaigns that included an Image") +
  ylab("Average Deliverability Rate")

pp1 <- plotly::ggplotly(p1)
saveRDS(pp1, "posts/Maximizing SMS Deliverability/widgets/sms_include_image.rds")


pdt <- mdata[pct_has_price > .04 & total_campaigns > 10]
p2 <- ggplot(pdt, aes(pct_has_price, ave_pct_delivered)) +
  geom_point(aes(color = org, size = total_campaigns)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_x_log10(label = scales::percent_format()) +
  scale_y_continuous(label = scales::percent_format()) +
  xlab("Percent of Campaign Messages that mentioned Prices") +
  ylab("Average Deliverability Rate") +
  scale_size(guide = "none")

pp2 <- plotly::ggplotly(p2)
saveRDS(pp2, "posts/Maximizing SMS Deliverability/widgets/sms_include_price.rds")


pdt <- mdata[ave_pct_delivered > .5 & pct_has_free > .005]
p3 <- ggplot(pdt, aes(pct_has_free, ave_pct_delivered)) +
  geom_point(aes(color = org, size = total_campaigns)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_x_log10(label = scales::percent_format()) +
  scale_y_continuous(label = scales::percent_format()) +
  xlab("Percent of Campaigns that mention Free") +
  ylab("Average Deliverability Rate") +
  scale_size(guide = "none")

pp3 <- plotly::ggplotly(p3)
saveRDS(pp3, "posts/Maximizing SMS Deliverability/widgets/sms_mention_free.rds")


pdt <- mdata[pct_has_emoji > .05 & ave_pct_delivered > .75]
p4 <- ggplot(pdt, aes(pct_has_emoji, ave_pct_delivered)) +
  geom_point(aes(color = org, size = total_campaigns)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_x_log10(label = scales::percent_format()) +
  scale_y_continuous(label = scales::percent_format()) +
  xlab("Percent of Campaigns that use Emojis") +
  ylab("Average Deliverability Rate") +
  scale_size(guide = "none")

pp4 <- plotly::ggplotly(p4)
saveRDS(pp4, "posts/Maximizing SMS Deliverability/widgets/sms_include_emoji.rds")



summary(lm(pct_delivered  ~
             log(n_phone_to) +
             log(n_phone_from) +
             log(n_carriers) +
             log(customers) +
             log(customers_targeted) +
             # log(texts) +
             # log(texts_sent) +
             # log(billed_texts) +
             log(text_length) +
             log(campaign_dur_mins) +
             # is_mms +
             has_image +
             has_tmobile +
             has_url +
             has_price +
             # has_discount +
             has_emoji,
           pdata[campaign_dur_mins > 0 & customers > 0 & texts > 0 & billed_texts > 0]))

# cn <- dbc("prod2", "cabbage_patch")
# qry <- "SELECT
#           SUM(CASE WHEN errors_1_code IS NULL THEN 1 ELSE 0 END) delivered,
#           SUM(CASE WHEN errors_1_code IS NOT NULL THEN 1 ELSE 0 END) as undelivered,
#           to_1_carrier
#         FROM tel_sms
#         WHERE orguuid = 'de65e6a2-2bec-4608-8d9f-74f4a2015851'
#           AND date(sent_at) > '2023-01-25'
#           AND direction = 'outbound'
#         GROUP BY to_1_carrier"
# DT <- as.data.table(dbGetQuery(cn, qry))
# DT
#
#
# cn <- dbc("prod2", "cabbage_patch")
# qry <- "SELECT
#           SUM(CASE WHEN errors_1_code IS NULL THEN 1 ELSE 0 END) delivered,
#           SUM(CASE WHEN errors_1_code IS NOT NULL THEN 1 ELSE 0 END) as undelivered,
#           to_1_carrier
#         FROM tel_sms
#         WHERE orguuid = 'de65e6a2-2bec-4608-8d9f-74f4a2015851'
#           AND date(sent_at) > '2023-01-25'
#           AND direction = 'outbound'
#         GROUP BY to_1_carrier"
# DT <- as.data.table(dbGetQuery(cn, qry))
# DT
#
# oid <- "de65e6a2-2bec-4608-8d9f-74f4a2015851"
# qry <- str_glue("
#         SELECT to_1_carrier AS carrier,
#           from_phone_number AS phone_from,
#           to_1_phone_number AS phone_to,
#           (CASE WHEN type = 'MMS' THEN TRUE ELSE FALSE END) AS is_mms,
#           (CASE WHEN media_1_url IS NULL THEN FALSE ELSE TRUE END) AS has_image,
#           (CASE WHEN errors_1_code IS NULL THEN TRUE ELSE FALSE END) AS is_delivered
#         FROM tel_sms
#         WHERE orguuid = '{oid}'
#           AND direction = 'outbound'
#           AND date(sent_at) > '{today() - months(6)}';")
#
# DT[, sent_at := as_datetime(sent_at)]
# DT[carrier == "", carrier := NA]
# DT[, carrier := str_squish(str_trim(str_to_upper(carrier), "both"))]
# DT[, carrier_state := str_extract(carrier, "(?<=\\- )[A-Z][A-Z](?=$)")]
# DT[, carrier2 := str_remove(carrier, " - (?<=\\- )[A-Z][A-Z](?=$)")]
#
#
# f <- function(i) {
#   st <- state.abb[which(str_detect(i, str_to_upper(state.name)))]
#   st[1]
# }
# DT[is.na(carrier_state), carrier_state := unlist(sapply(carrier, f))]
#
# DT[str_detect(carrier2, "VERIZON"), carrier2 := "VERIZON"]
# DT[str_detect(carrier2, "T\\-MOBILE"), carrier2 := "T-MOBILE"]
# DT[str_detect(carrier2, "AT\\&T"), carrier2 := "AT&T"]
# DT[str_detect(carrier2, "METROPCS"), carrier2 := "METROPCS"]
# DT[str_detect(carrier2, "CABLEVISION LIGHTPATH"), carrier2 := "CABLEVISION LIGHTPATH"]
# DT[str_detect(carrier2, "BELLSOUTH TELECOMM"), carrier2 := "BELLSOUTH TELECOMM"]
# DT[str_detect(carrier2, "CHARTER FIBERLINK"), carrier2 := "CHARTER FIBERLINK"]
# DT[str_detect(carrier2, "CENTURYLINK"), carrier2 := "CENTURYLINK"]
# DT[str_detect(carrier2, "FRONTIER"), carrier2 := "FRONTIER"]
# DT[str_detect(carrier2, "LEVEL 3"), carrier2 := "LEVEL 3"]
# DT[str_detect(carrier2, "OMNIPOINT"), carrier2 := "OMNIPOINT"]
# DT[str_detect(carrier2, "ONVOY"), carrier2 := "ONVOY"]
# DT[str_detect(carrier2, "PEERLESS NETWORK"), carrier2 := "PEERLESS NETWORK"]
# DT[str_detect(carrier2, "POWERTEL"), carrier2 := "POWERTEL"]
# DT[str_detect(carrier2, "TELEPORT COMMUNICATIONS"), carrier2 := "TELEPORT COMMUNICATIONS"]
# DT[str_detect(carrier2, "TIME WARNER"), carrier2 := "TIME WARNER"]
# DT[str_detect(carrier2, "WINDSTREAM"), carrier2 := "WINDSTREAM"]
# DT[str_detect(carrier2, "XO"), carrier2 := "XO"]
#
#
# DT[, .N, keyby = carrier2] |> View()

# ..collapse_carriers <- function(logs) {
#   cars <- c(
#     "AIRUS, INC",
#     "AT&T",
#     "ACS",
#     "ARMSTRONG TEL",
#     "BRESNAN BROADBAND",
#     "BROADVIEW NETWORKS",
#     "CABLEVISION LIGHTPATH",
#     "CAVALIER",
#     "CEBRIDGE TELECOM",
#     "CENTURYLINK",
#     "CENTURYTEL",
#     "CHARTER FIBERLINK",
#     "CITIZENS TEL",
#     "CONSOLIDATED COMM",
#     "CORETEL",
#     "FRONTIER",
#     "GLOBAL CROSSING",
#     "LEVEL 3",
#     "MCIMETRO ACCESS",
#     "PEERLESS NETWORK",
#     "TELEPORT COMM",
#     "UNITED TEL",
#     "US LEC",
#     "VERIZON",
#     "WINDSTREAM",
#     "XO",
#     "YMAX COMM",
#     "MCLEODUSA",
#     "PAETEC",
#     "RCLEC",
#     "ZIPLY FIBER",
#     "ASTOUND",
#     "DELTACOM",
#     "FIRST COMM",
#     "IDT AMERICA",
#     "INTEGRATED PATH COMM",
#     "KNOLOGY",
#     "LOCAL ACCESS",
#     "LUMOS",
#     "MATRIX TELECOM",
#     "MCC TELEPHONY",
#     "MIDCONTINENT COMM",
#     "MOSAIC",
#     "MPOWER",
#     "PIONEER TEL",
#     "RCN TELECOM",
#     "NEX-TECH",
#     "TC SYSTEMS",
#     "UTILITY TELECOM GROUP",
#     "US XCHANGE",
#     "VANTAGE TELECOM",
#     "VOXBEAM TEL",
#     "LOGIX COMM",
#     "WIDE VOICE",
#     "NETWORK TELEPHONE",
#     "BARR TEL",
#     "BRIGHT HOUSE NTWS",
#     "BUSINESS TELECOM",
#     "CHOICE ONE",
#     "COMCAST PHONE",
#     "COMMNET WIRELESS",
#     "CONVERSENT",
#     "COX",
#     "DIGICEL",
#     "FARMERS",
#     "HAWAIIAN TELCOM",
#     "PINE BELT",
#     "RANGE TEL",
#     "GOLDEN WEST",
#     "TDS METROCOM",
#     "TELEPACIFIC CORP",
#     "CORE COMMUNICATIONS",
#     "ATX",
#     "CTC"
#   )
#
#   iter <- 0
#   for (x in cars) {
#     cat("progress", iter <- iter + 1, "\n")
#     logs[str_detect(carrier, x), carrier := x]
#   }
#   return(logs[])
# }



#
# mldata <- clogSumm[n_phone_to > 100, .(
#   total_campaigns = .N,
#   ave_campaign_size = mean(n_phone_to, trim = .1),
#   ave_phones_from = mean(n_phone_from, trim = .1),
#   ave_num_carriers = mean(n_carriers, trim = .1),
#   ave_pct_success = mean(pct_success, trim = .1),
#   ave_pct_failure = mean(pct_fails, trim = .1),
#   ave_pct_tmobile = mean(pct_tmobile, trim = .1),
#   ave_pct_verizon = mean(pct_verizon, trim = .1),
#   ave_pct_att = mean(pct_att, trim = .1),
#   ave_pct_others = mean(pct_others, trim = .1),
#   pct_with_verizon = sum(has_verizon) / .N,
#   pct_with_att = sum(has_att) / .N,
#   pct_with_tmobile = sum(has_tmobile) / .N
# ), org]
#
# summary(lm(ave_pct_failure ~
#              ave_phones_from +
#              ave_num_carriers +
#              pct_with_verizon +
#              ave_pct_others +
#              ave_pct_att,
#            mldata[total_campaigns > 50]))
#
# ggplot(clogSumm[n_phone_to > 100 & pct_tmobile > 0], aes(pct_tmobile, pct_fails)) +
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10()
#
#
# clogSumm[has_tmobile & has_att & has_tmobile]
#
# stats <- logDT[, .(
#   n_phone_to = .N,
#   n_phone_from = length(unique(phone_from)),
#   n_carriers = length(unique(carrier)),
#   tot_success = sum(http_code == 200 | http_code == 201, na.rm = TRUE),
#   tot_fails = sum(http_code > 201, na.rm = TRUE),
#   has_tmobile = any(str_detect(carrier, "T\\-MOBILE"), na.rm = TRUE),
#   has_att = any(str_detect(carrier, "T\\-MOBILE"), na.rm = TRUE),
#   has_verizon = any(str_detect(carrier, "T\\-MOBILE"), na.rm = TRUE),
#   pct_tmobile = sum(str_detect(carrier, "T\\-MOBILE")) / .N
# ), keyby = .(org, campaign_id)]


# logDT[, .N, keyby = carrier] |> View()
