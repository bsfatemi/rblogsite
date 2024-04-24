
# Read population data ------------------------------------------------------------------------


popdir <- "/home/bobbyf/PROJECTS/analysis/data/pop_2023-06-22_1687455808"
vec <- list.files(popdir, full.names = TRUE)
fps <- vec[order(as.numeric(str_extract(vec, "(?<=iter_).+(?=\\.fst)")))]

f <- function(i) {
  cols <- c("org_uuid",
            "order_facility",
            "order_type",
            "sold_by",
            "phone",
            "birthday",
            "customer_city",
            "customer_state",
            "order_time_utc",
            "brand_name",
            "product_name",
            "raw_category_name",
            "product_qty",
            "order_line_subtotal",
            "order_line_list_price",
            "order_line_discount")
  dt <- read_fst(i, columns = cols, as.data.table = TRUE)[year(order_time_utc) == 2023]
  dt[, brand_name := str_remove(brand_name, "^D ")]
  dt[brand_name == "" | brand_name == "NO_VALUE", brand_name := NA]
  dt[, brand_name := str_trim(str_to_upper(brand_name), "both")]
  dt[, product_name := str_trim(str_to_upper(product_name), "both")]
  DT <- cbind(dt[!is.na(brand_name)], category3 = build_category3(dt[!is.na(brand_name)], NULL))
  cleanPop(DT)
}

t <- Sys.time()
P <- rbindlist(mclapply(fps, f, mc.cores = 10))
Sys.time() - t

setkeyv(P, c("org_uuid", "order_facility"))







# DT[, brand2 := str_split(brand_name, " ?\\| ?", simplify = TRUE)[, 1]]
#
#
#
# ## clean up other brands
# brands <- c("ALVAREZ",
#             "APPALACHIAN PHARM",
#             "ANCIENT ROOTS",
#             "ARTET",
#             "AVENGIVA",
#             "AZULANO",
#             "BANDOLYN",
#             "BAYAVEC",
#             "BANIVA",
#             "BENELEAVES",
#             "BLANCAVEC",
#             "BLUEBEREN",
#             "BULLET TRAIN",
#             "CALAVEC",
#             "CANNABIDIOL",
#             "CARPENTER MEDICAL GROUP",
#             "CATALYST",
#             "CEREZAVEC",
#             "CERTIFIED CULTIVATORS",
#             "CHURLIVA",
#             "CLASSIX",
#             "COLUMBIA CARE",
#             "CROPS",
#             "CRUDE BOYS",
#             "DAZE OFF",
#             "DOGHOUSE",
#             "FARKAS FARMS",
#             "FARMACEUTICAL RX",
#             "FIRELANDS SCIENTIFIC")
#
# for (i in brands) {
#   print(i)
#   DT[str_detect(brand_name, str_glue("^\\*?{i}\\b")), brand_name := i]
# }
#
#
# brands <- c("AGL",
#             "EMPERIDEX",
#             "FIORADEN",
#             "GLACEDEN",
#             "GRAPEDEN",
#             "GRETOLEX",
#             "INDOTI",
#             "JAVADEN",
#             "JAZRICA",
#             "JEEMODEX",
#             "LEXIKAN",
#             "LUNAVEN",
#             "MACBRID",
#             "MANTEVEC",
#             "MANZADEN",
#             "MARMADEN",
#             "CURALEAF",
#             "NUVADEN",
#             "OBAKAN",
#             "OCHOVEC",
#             "PURPALEX",
#             "SAVOTI",
#             "SCOOPADEN",
#             "SOULBRID",
#             "SUMMIT",
#             "SUNMED",
#             "TORTITEX",
#             "TULSAVEC",
#             "TUSSADEN",
#             "WHIZBRID")
# for(i in brands) {
#   print(i)
#   DT[str_detect(brand2, str_glue("^\\*?{i}")), brand2 := i]
# }
#
#
# pat <- ".+(?= (FLOWER|PRE\\-ROLLS|SHORTIES T[0-9]))"
# DT[str_detect(brand2, pat), brand2 := str_extract(brand2, pat)]
#
#
# brands <- c("GUAIVEN",
#             "HARACEPT",
#             "CURALEAF",
#             "MPX",
#             "RATINDICA",
#             "RELEAF",
#             "ROSICA",
#             "STILLETOVEX",
#             "SUNDICA",
#             "THAILIVA",
#             "TORTADEN",
#             "TRUTHICA",
#             "ANIVA",
#             "BALINICA",
#             "CREPELEX",
#             "EGBRID",
#             "AVENGIVA",
#             "DEEZIVA",
#             "ELIORACA",
#             "FANTADEX",
#             "HAUNTRICA",
#             "HELADICA",
#             "JEDIVA",
#             "JEHANOSE",
#             "JELLIVEX",
#             "LECHELEX",
#             "MAYFLOWER",
#             "MELENAX")
# for(i in brands) {
#   print(i)
#   DT[str_detect(brand2, str_glue("{i}")), brand2 := i]
# }
#
#
# DT[str_detect(brand_name, "\\bLIT\\b"), brand2 := "LIT"]
# DT[str_detect(brand2, "MEND"), brand2 := "MEND"]
# DT[str_detect(brand2, "TRAVELER"), brand2 := "TRAVELERS"]
# DT[str_detect(brand2, "^(T|C)(10|100|16|5|50|25|40) "), brand2 := "CTPHARMA"]
# DT[str_detect(brand2, "(?<=THC).*(?=DROPS)"), brand2 := "THC DROPS"]
# DT[str_detect(brand2, "CALAVEC"), brand2 := "CALAVEC"]
# DT[str_detect(brand2, "CANNABIDIOL"), brand2 := "CANNABIDIOL"]
# DT[str_detect(brand2, "CEREZAVEC"), brand2 := "CEREZAVEC"]
# DT[str_detect(brand2, "CHURLIVA"), brand2 := "CHURLIVA"]
# DT[str_detect(brand2, "DELATAVEC"), brand2 := "DELATAVEC"]
# DT[str_detect(brand2, "DHARMADEX"), brand2 := "DHARMADEX"]
# DT[str_detect(brand2, "DETROIT EDIBLE"), brand2 := "DETROIT EDIBLES"]
# DT[str_detect(brand2, "DIP DEVICES"), brand2 := "DIP DEVICES"]
# DT[str_detect(brand2, "FRESALYN"), brand2 := "FRESALYN"]
# DT[str_detect(brand2, "GELATOVEN"), brand2 := "GELATOVEN"]
# DT[str_detect(brand2, "GHANICA"), brand2 := "GHANICA"]
# DT[str_detect(brand2, "GRATICA"), brand2 := "GRATICA"]
# DT[str_detect(brand2, "GRUTALEX"), brand2 := "GRUTALEX"]
# DT[str_detect(brand2, "GWYNIVA"), brand2 := "GWYNIVA"]
# DT[str_detect(brand2, "HAZEVEC"), brand2 := "HAZEVEC"]
# DT[str_detect(brand2, "INFUSION AID"), brand2 := "INFUSION AID"]
# DT[str_detect(brand2, "MODIVEC"), brand2 := "MODIVEC"]
# DT[str_detect(brand2, "PINTSICA"), brand2 := "PINTSICA"]
# DT[str_detect(brand2, "SONGRICA"), brand2 := "SONGRICA"]
# DT[str_detect(brand2, "STOMPEDEX"), brand2 := "STOMPEDEX"]
# DT[str_detect(brand2, "TAYTENATE"), brand2 := "TAYTENATE"]
# DT[str_detect(brand2, "BANDOLYN"), brand2 := "BANDOLYN"]
# DT[str_detect(brand2, "BAYAVEC"), brand2 := "BAYAVEC"]
# DT[str_detect(brand2, "BENTOBRID"), brand2 := "BENTOBRID"]
# DT[str_detect(brand2, "BISCAYPURE"), brand2 := "BISCAYPURE"]
# DT[str_detect(brand2, "BLANCAVEC"), brand2 := "BLANCAVEC"]
# DT[str_detect(brand2, "BLUEBEREN"), brand2 := "BLUEBEREN"]
# DT[str_detect(brand2, "BUBBYS BAKED"), brand2 := "BUBBYS BAKED GOODS"]
# DT[str_detect(brand2, "COBBLADEX"), brand2 := "COBBLADEX"]
# DT[str_detect(brand2, "DORMADEN"), brand2 := "DORMADEN"]
# DT[str_detect(brand2, "EVOLVE"), brand2 := "EVOLVE"]
# DT[str_detect(brand2, "FRAISEDEN"), brand2 := "FRAISEDEN"]
# DT[str_detect(brand2, "HH RED"), brand2 := "HH RED"]
# DT[str_detect(brand2, "KNOBRID"), brand2 := "KNOBRID"]
# DT[str_detect(brand2, "LEGEND"), brand2 := "LEGENDS"]
# DT[str_detect(brand2, "NOVA"), brand2 := "NOVA"]
# DT[str_detect(brand2, "PASTEL"), brand2 := "PASTEL"]
# DT[str_detect(brand2, "PHARMACANN"), brand2 := "PHARMACANN"]
# DT[str_detect(brand2, "RESERVE"), brand2 := "RESERVE"]
# DT[str_detect(brand2, "RICKAVEX"), brand2 := "RICKAVEX"]
# DT[str_detect(brand2, "SUPREME INFUSED"), brand2 := "SUPREME INFUSED"]
# DT[str_detect(brand2, "WARRANTEX"), brand2 := "WARRANTEX"]
# DT[str_detect(brand2, "TUCANA"), brand2 := "TUCANA"]
# DT[str_detect(brand2, "TROPEX"), brand2 := "TROPEX"]
# DT[str_detect(brand2, "TRILLAVEX"), brand2 := "TRILLAVEX"]
# DT[str_detect(brand2, "TRANSDERMAL CREAM"), brand2 := "TRANSDERMAL CREAM"]
# DT[str_detect(brand2, "TIGERBRID"), brand2 := "TIGERBRID"]
# DT[str_detect(brand2, "TETRADEN"), brand2 := "TETRADEN"]
# DT[str_detect(brand2, "TEXIBRID"), brand2 := "TEXIBRID"]
# DT[str_detect(brand2, "TUCANA"), brand2 := "TUCANA"]
#
#
#
# brand_summary <- DT[, {
#   units_sold <- sum(ceiling(product_qty))
#   first_sold <- as_date(min(order_time_utc))
#   last_sold <- as_date(max(order_time_utc))
#
#   list(
#     units_sold = units_sold,
#     first_sold = first_sold,
#     last_sold = last_sold,
#     last_sold_days = as.integer(difftime(today(), last_sold)),
#     units_per_day = ceiling(units_sold / as.integer(difftime(last_sold, first_sold)))
#   )}, keyby = .(brand2, category3)]
#
# brand_summary_wkly <- DT[, list(
#   units_per_day = sum(ceiling(product_qty)) / 7
# ), keyby = .(
#   brand2,
#   category3,
#   week_date = floor_date(as_date(order_time_utc), unit = "weeks")
# )]
# library(ggplot2)
#
# pdata <- brand_summary_wkly[
#   brand_summary_wkly[category3 == "VAPES", .N, keyby = brand2][N > 24, .(brand2)]
# ][category3 == "VAPES"]
#
# pdata <- brand_summary_wkly[week_date != max(week_date), .(
#   unitVd = ceiling(sum(units_per_day))
# ), .(category3, week_date)]
#
# ggplot(pdata) +
#   geom_point(aes(week_date, unitVd, color = category3)) +
#   scale_y_log10() +
#   facet_grid(. ~ category3, scales = "free")




# expr <- substitute(
#   as.integer(difftime(floor_date(today(), unit = "weeks"), max(week_date), units = "weeks"))
# )
# keycols <- c("brand2", "category3")
# setkeyv(brand_summary_wkly, keycols)
#
# bsw <- brand_summary_wkly[
#   brand_summary_wkly[, eval(expr) <= 3, keycols][(V1), keycols, with = FALSE]
# ]
# # bsw[, .N, .(brand2, category3)][N > 10]
#
# keycols <- c("category3", "week_date")
# setkeyv(bsw, keycols)
# bsw[
#   bsw[, .(total_units_per_day = sum(units_per_day)), keyby = keycols],
#   share_velocity := units_per_day / total_units_per_day
# ]
#
# bsw[, .(
#   ave_share_vt = mean(share_velocity, trim = .2)
# ), keyby = .(brand2, category3)][ave_share_vt > .01][
#   order(category3, -ave_share_vt)
# ]
#
#
# bs <- brand_summary[!is.infinite(units_per_day) & last_sold_days <= 21]
#
# bs[]
#
# brand_velocity <- dcast(
#   data = ,
#   formula = brand2 ~ category3,
#   value.var = "units_per_day",
#   fill = 0
# )[order(brand2)]
#
# brand_velocity[, UNITS_PER_DAY := apply(brand_velocity[,-1], 1, sum)]
# brand_velocity[UNITS_PER_DAY > 10][order(-FLOWER)]





