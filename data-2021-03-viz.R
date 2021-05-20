library(data.table)
library(ggplot2)
data.dt <- data.table::fread("data-2021-03.csv")

some.cells <- 1:10
some.data <- data.dt[cell %in% some.cells]

ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ cell, labeller=label_both)

ggplot()+
  geom_point(aes(
    sqrt(current.nA), log(resistance.ohms)),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ cell, labeller=label_both)

ggplot()+
  geom_point(aes(
    sqrt(current.nA), log(resistance.ohms), fill=degrees.C),
    shape=21,
    data=some.data)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ cell, labeller=label_both)

ggplot()+
  scale_color_gradient(low="white", high="red")+
  geom_boxplot(aes(
    sqrt(current.nA), log(resistance.ohms),
    ##color=degrees.C),
    group=paste(current.nA, degrees.C)),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ cell, labeller=label_both)

ggplot()+
  scale_fill_gradient(low="white", high="red")+
  geom_violin(aes(
    sqrt(current.nA), log(resistance.ohms),
    fill=degrees.C,
    group=paste(current.nA, degrees.C)),
    color="black",
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(cell ~ ., scales="free")

some.stats <- some.data[, .(
  q75=quantile(resistance.ohms, 0.75),
  median=median(resistance.ohms),
  q25=quantile(resistance.ohms, 0.25)
), by=.(cell, current.nA, degrees.C)]
gg <- ggplot()+
  scale_fill_gradient(low="white", high="red")+
  scale_fill_manual(values=c("0"="blue", "23"="white", "80"="red"))+
  geom_segment(aes(
    sqrt(current.nA), log(q25),
    xend=sqrt(current.nA), yend=log(q75)),
    size=1,
    data=some.stats)+
  geom_point(aes(
    sqrt(current.nA), log(median),
    fill=factor(degrees.C)),
    color="black",
    shape=21,
    data=some.stats)+
  ylab("log10(Resistance in ohms)")+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ cell, labeller=label_both)
png("data-2021-03-viz-10-cells.png", width=10, height=5, res=200, units="in")
print(gg)
dev.off()
