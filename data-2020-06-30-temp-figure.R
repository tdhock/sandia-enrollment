library(data.table)
library(ggplot2)
temp.dt <- fread("data-2020-06-30-temp.csv")

table(temp.dt$degrees.C)
temp.colors <- c(
  "0"="#FFF5F0", "#FEE0D2", "#FCBBA1",
  "#FC9272", "#FB6A4A", "#EF3B2C", 
  "23"="#CB181D", "#A50F15", "#67000D",
  "60"="red")
ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_wrap("cell")+
  geom_point(aes(
    log(-current.nA), log10(resistance.ohms), fill=degrees.C),
    shape=21,
    data=temp.dt)+
  ##scale_fill_manual(values=temp.colors)
  scale_fill_gradient(low="white", high="red")

gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_wrap("cell", scales="free", labeller=label_both, ncol=4)+
  geom_point(aes(
    -current.nA, resistance.ohms, fill=degrees.C),
    shape=21,
    data=temp.dt)+
  scale_fill_gradient(low="white", high="red")+
  scale_x_log10(
    "current (nA)",
    breaks=c(100, 400, 800)
  )+
  scale_y_log10()
png("data-2020-06-30-temp-figure.png", width=8, height=12, units="in", res=100)
print(gg)
dev.off()

cell.id.vec <- sort(unique(temp.dt$cell))
for(cell.id in cell.id.vec){
  out.png <- file.path(
    "data-2020-06-30-temp-figure",
    paste0(cell.id, ".png"))
  dir.create(dirname(out.png), showWarnings=FALSE)
  cat(sprintf("%4d / %4d %s\n", cell.id, length(cell.id.vec), out.png))
  cell.dt <- temp.dt[cell==cell.id]
  cell.stats <- cell.dt[, .(
    min=min(resistance.ohms),
    median=median(resistance.ohms),
    max=max(resistance.ohms)
  ), by=.(C=degrees.C, current.nA)]
  gg <- ggplot()+
    ggtitle(paste("cell", cell.id))+
    theme_bw()+
    theme(panel.spacing=grid::unit(0, "lines"))+
    geom_ribbon(aes(
      -current.nA, ymin=min, ymax=max, fill=C, group=C),
      alpha=0.5,
      color="grey",
      data=cell.stats)+
    directlabels::geom_dl(aes(
      -current.nA, (min+max)/2, label=C, color=C),
      data=cell.stats,
      method="right.polygons")+
    geom_point(aes(
      -current.nA, resistance.ohms, fill=degrees.C),
      shape=21,
      data=cell.dt)+
    scale_x_log10(
      "current (nA)",
      breaks=c(100, 400, 800),
      limits=c(100, 1000)
    )+
    scale_y_log10(
      "resistance (ohms)")+
    scale_color_gradient(low="white", high="red")+
    scale_fill_gradient(low="white", high="red")+
    facet_grid(. ~ degrees.C, labeller=label_both)
  png(out.png, width=8, height=3, units="in", res=100)
  print(gg)
  dev.off()
}
