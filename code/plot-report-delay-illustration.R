library(tidyquant)
library(here)
library(data.table)
library(ggplot2)

dt <- readRDS(here("data", "cases", "national", "merged.rds"))
dt <- data.table(dt)
dt[, cases := cumsum(cases), by=specimen_date]

tx <- unique(dt$report_date)[seq(90, 200, length.out = 8)]

dt[report_date %in% tx & specimen_date < max(tx)
   ][, ggplot(.SD, aes(x=specimen_date, y=cases, size = factor(report_date), col=factor(report_date))) +
       geom_ma(ma_fun = SMA, n = 7, lty = 1) +
       geom_vline(data = data.frame(report_date = tx[2:7]),
                  aes(xintercept = report_date-1, col=factor(report_date)), lty=2, lwd = 1) +
       scale_size_manual(values = c(rep(1.5,5), 1.5))+
       scale_color_manual(values = 7:1) +
       scale_y_continuous("New cases (7-day moving average)",
                          labels = scales::label_number(suffix = "K", scale = 1e-3)) +
       scale_x_date("Specimen date", 
                    limits = c(as.Date("2021-08-01"), as.Date("2021-12-01"))) +
       theme_bw() + 
       theme(legend.position = 'none')] 


