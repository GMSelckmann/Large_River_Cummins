
source("flow/flow_plot_func.R")
por <- "01638500"
lf <- "01646500"


por.plot <- flow_plot(por)
lf.plot <- flow_plot(lf)

por.lf.plot <- cowplot::plot_grid(por.plot, lf.plot, ncol = 1, labels = c( "A)", "B)"))

ggsave("por_flow.png", por.plot,
       path = "flow/output",
       width = 20, height = 5, units = "in")

ggsave("lf_flow.png", por.plot,
       path = "flow/output",
       width = 20, height = 5, units = "in")

ggsave("por_lf_flow.png", por.lf.plot,
       path = "flow/output",
       width = 20, height = 10, units = "in")
