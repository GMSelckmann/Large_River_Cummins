library(tidyverse)
library(dataRetrieval)
library(toolbox)



# pull_nwis ---------------------------------------------------------------
pull_nwis <- function(gage) {
  nwis.df <- dataRetrieval::readNWISdata(service = "dv",
                                         site = gage,
                                         startDate = "1800-10-01",
                                         endDate = "2014-09-30",
                                         asDateTime = FALSE,
                                         #tz = "America/New_York",
                                         # Dischrage Code.
                                         parameterCd = "00060") %>% 
    toolbox::prep_df()
}

# prep_gage ---------------------------------------------------------------
prep_gage <- function(x) {
  final.df <- x %>% 
    rename(date = datetime,
           flow = x_00060_00003) %>% 
    filter(date >= as.Date("1965-10-01") & date < as.Date("1966-10-01") |
             date >= as.Date("1995-10-01") & date < as.Date("1996-10-01") |
             date >= as.Date("2011-10-01") & date < as.Date("2014-10-01")
    ) %>% 
    mutate(year = lubridate::year(date),
           month = lubridate::month(date, label = TRUE, abbr = TRUE),
           month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan",
                                            "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep")),
           water_year = case_when(
             date >= as.Date("1965-10-01") & date < as.Date("1966-10-01") ~ "1966",
             date >= as.Date("1995-10-01") & date < as.Date("1996-10-01") ~ "1996",
             date >= as.Date("2011-10-01") & date < as.Date("2012-10-01") ~ "2012",
             date >= as.Date("2012-10-01") & date < as.Date("2013-10-01") ~ "2013",
             date >= as.Date("2013-10-01") & date < as.Date("2014-10-01") ~ "2014",
             TRUE ~ "ERROR"
           ),
           log_flow = log(flow)) %>% 
    group_by(water_year, month) %>% 
    mutate(
      quant_50 = quantile(log_flow, 0.50)
    ) %>% 
    ungroup()
}


# quant_summary -----------------------------------------------------------
quant_summary <- function(x) {
  final.df <- x %>% 
    prep_df() %>% 
    rename(date = datetime,
           flow = x_00060_00003) %>% 
    mutate(log_flow = log(flow),
           month = lubridate::month(date, label = TRUE, abbr = TRUE),
           month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan",
                                            "Feb", "Mar", "Apr", "May",
                                            "Jun", "Jul", "Aug", "Sep"))) %>% 
    group_by(month) %>% 
    summarize(quant_10 = quantile(log_flow, 0.10),
              quant_25 = quantile(log_flow, 0.25),
              quant_50 = quantile(log_flow, 0.50),
              quant_75 = quantile(log_flow, 0.75),
              quant_90 = quantile(log_flow, 0.90)
    ) %>% 
    ungroup() %>% 
    mutate(month_num = as.numeric(month))
}


# fplot -------------------------------------------------------------------
fplot <- function(gage.df, quant.df) {
  ggplot() + 
    # geom_point(data = gage.df, aes(month, log_flow)) +
    geom_line(data = gage.df, aes(month, y = quant_50,
                                group = water_year, color = water_year), size = 1.1) +
    geom_ribbon(data = quant.df, aes(x = month_num,
                                         ymin = quant_10, ymax = quant_25,
                                         fill = "cadetblue3"), alpha = 0.5) +
    geom_ribbon(data = quant.df, aes(x = month_num, 
                                         ymin = quant_25, ymax = quant_75,
                                         fill = "orange"),alpha = 0.5) +
    geom_ribbon(data = quant.df, aes(x = month_num,
                                         ymin = quant_75, ymax = quant_90,
                                         fill = "cadetblue3"), alpha = 0.5) +
    scale_fill_manual(values = c("cadetblue3" = "cadetblue3",  "orange" = "orange"),
                      name = "Quartile Range",
                      labels = c("Outer Quartile Range: 0.10-0.25 and 0.75-0.90",
                                 "Inner Quartile Range: 0.25-0.75")) +
    geom_line(data = gage.df, aes(month, y = quant_50,
                                group = water_year, color = water_year), size = 1.1) +
    scale_color_manual(values = c("1966" = "red",
                                  "1996" = "blue",
                                  "2012" = "black",
                                  "2013" = "darkorchid4",
                                  "2014" = "sienna3"),
                       name = "Water Year") +
    guides(
      color = guide_legend(order = 0),
      fill = guide_legend(order = 1)
    ) +
    # scale_x_discrete(expand= c(0,0)) +
    xlab("Month") +
    ylab("Log Flow (CFS)") +
    theme(axis.title.x = element_text(face = "bold", colour = "black", size = 15),
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 15),
          axis.title.y = element_text(face = "bold", colour = "black", size = 15),
          axis.text.y = element_text(angle = 0, vjust = 0.5, size = 15),
          legend.text = element_text(size = 13),
          legend.title = element_text(face = "bold", size = 15)
          
    )
  
}

# flow_plot ---------------------------------------------------------------
flow_plot <- function(gage) {
  nwis.df <- pull_nwis(gage)
  prep.gage <- prep_gage(nwis.df)
  quant.df <- quant_summary(nwis.df)
  fplot(prep.gage, quant.df)
}