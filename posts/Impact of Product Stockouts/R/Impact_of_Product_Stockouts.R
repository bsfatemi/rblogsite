library(data.table)
library(hcapipelines2)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggtext)
library(magick)

setwd("posts/_draft/Impact of Product Stockouts/")
source("/home/bobbyf/PROJECTS/cabbageBlog/posts/Impact of Product Stockouts/app/R/plot_funs.R")

# inputs --------------------------------------------------------------------------------------


org <- ""
store <- ""
oid <- lookupOrgGuid(org)
sid <- orgIndexFull()[org_short == org & store_short == store, store_uuid]
sku <- ""

salesDT <- dbGetVtDaily(oid, sid)

oid <- read_org_index()[org == "", org_uuid[1]]

# oid <- ""
# sid <- ""
# sku <- ""






# plot ----------------------------------------------------------------------------------------

b0 <- plot_base_layer(salesDT)

p_sales <- b0 +
  labs(
    title = "<b>Cumulative Total Product Sales in Prior 90 Day Period</b><br>
    <span style= 'font-size:10pt;'>*With Stockouts (Actual) vs No Stockouts (Estimate)*</span>",
    caption = "*x-axis is % of period completed (i.e. 50% = 45 days of sales)*"
  ) +
  theme(
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ),
    plot.subtitle = element_textbox_simple(
      size = 10,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ),
    plot.caption = element_markdown(
      size = 10,
      lineheight = 1,
      hjust = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "#cae1ff",
      color = "#3d4c57"
    ),
    plot.caption.position = "plot"
  )

p_guide_1 <- plot_mid_period(b0, salesDT)
p_guide_2 <- plot_end_period(b0, salesDT)



ggsave(filename = "images/sales_but-for_stockouts.png",
       plot = p_sales,
       width = 2800,
       height = 2400,
       units = "px",
       dpi = "retina")
ggsave(filename = "images/guide_1_sales_but-for_stockouts.png",
       plot = p_guide_1,
       width = 2800,
       height = 2400,
       units = "px",
       dpi = "retina")
ggsave(filename = "images/guide_2_sales_but-for_stockouts.png",
       plot = p_guide_2,
       width = 2800,
       height = 2400,
       units = "px",
       dpi = "retina")






