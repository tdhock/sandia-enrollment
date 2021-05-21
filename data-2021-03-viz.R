library(data.table)
library(ggplot2)
data.dt <- data.table::fread("data-2021-03.csv")

some.cells <- 1:10
some.data <- data.dt[cell %in% some.cells]

gg <- ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ cell, labeller=label_both)

gg <- ggplot()+
  geom_point(aes(
    sqrt(current.nA), log(resistance.ohms)),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ cell, labeller=label_both)

gg <- ggplot()+
  geom_point(aes(
    sqrt(current.nA), log(resistance.ohms), fill=degrees.C),
    shape=21,
    data=some.data)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ cell, labeller=label_both)

gg <- ggplot()+
  scale_color_gradient(low="white", high="red")+
  geom_boxplot(aes(
    sqrt(current.nA), log(resistance.ohms),
    ##color=degrees.C),
    group=paste(current.nA, degrees.C)),
    data=some.data)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ cell, labeller=label_both)

gg <- ggplot()+
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

data.stats <- data.dt[, .(
  q75=quantile(resistance.ohms, 0.75),
  median=median(resistance.ohms),
  q25=quantile(resistance.ohms, 0.25),
  measurements=.N
), by=.(cell, current.nA, degrees.C)]
data.stats[, iqr := q75-q25]
table(data.stats$measurements)
data.stats[measurements < max(measurements)]#same as in missing fig.

zeros <- data.table(count=nrow(data.stats[iqr==0]))
gg <- ggplot()+
  geom_point(aes(
    0, count),
    data=zeros)+
  geom_histogram(aes(
    iqr),
    data=data.stats)+
  scale_x_log10("Inter-quartile range over measurements for a (cell,current,temperature)")+
  ggtitle("Range of measurements is bimodal, with some zeros")
png("data-2021-03-viz-iqr-hist.png", width=6, height=5, res=200, units="in")
print(gg)
dev.off()

lim <- 10^c(0, 7)
gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_abline(aes(
    slope=slope,intercept=intercept),
    data=data.table(slope=1,intercept=0),
    color="grey")+
  coord_equal(xlim=lim, ylim=lim)+
  scale_fill_manual(values=c("0"="blue", "23"="white", "80"="red"))+
  geom_point(aes(
    iqr, median, fill=factor(degrees.C)),
    shape=21,
    data=data.stats)+
  scale_x_log10("Inter-quartile range of measurements for a (cell,current,temperature)")+
  scale_y_log10("Median of measurements for a (cell,current,temperature)")+
  ggtitle("Two clusters of (cell,current,temperature) data")
png("data-2021-03-viz-iqr-median-scatter.png", width=6, height=6, res=200, units="in")
print(gg)
dev.off()

gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_abline(aes(
    slope=slope,intercept=intercept),
    data=data.table(slope=1,intercept=0),
    color="grey")+
  coord_equal(xlim=lim, ylim=lim)+
  facet_grid(. ~ degrees.C, labeller=label_both)+
  geom_point(aes(
    iqr, median),
    shape=21,
    data=data.stats)+
  scale_x_log10("Inter-quartile range of measurements for a (cell,current,temperature)")+
  scale_y_log10("Median of measurements for a (cell,current,temperature)")+
  ggtitle("Two clusters of (cell,current,temperature) data")
png("data-2021-03-viz-iqr-median-scatter-panels.png", width=10, height=4, res=200, units="in")
print(gg)
dev.off()

cell.temp.ranges <- data.stats[, .(
  max=max(median),
  min=min(median),
  measurements=sum(measurements)
), by=.(cell, degrees.C)]
gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ ., labeller=label_both)+
  geom_segment(aes(
    cell, min,
    color=factor(measurements),
    xend=cell, yend=max),
    data=cell.temp.ranges)+
  geom_point(aes(
    cell, (min+max)/2,
    color=factor(measurements)),
    shape=1,
    data=cell.temp.ranges[min==max])+
  scale_y_log10("Median measurement values for each (cell,temperature)
Segment = range over all currents, Point = only value when min=max")+
  scale_color_manual("N",values=c("400"="grey50", "384"="green", "8"="violet"))
png("data-2021-03-viz-iqr-cell-ranges-N.png", width=10, height=5.2, res=200, units="in")
print(gg)
dev.off()

cell.temp.ranges[, smallest.upper := min(max), by=cell]
cell.temp.ranges[, class := ifelse(smallest.upper < 1e5, "small", "large")]
gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(degrees.C ~ ., labeller=label_both)+
  geom_segment(aes(
    cell, min,
    color=class,
    xend=cell, yend=max),
    data=cell.temp.ranges)+
  geom_point(aes(
    cell, (min+max)/2,
    color=class),
    shape=1,
    data=cell.temp.ranges[min==max])+
  scale_y_log10("Median measurement values for each (cell,temperature)
Segment = range over all currents, Point = only value when min=max")
png("data-2021-03-viz-iqr-cell-ranges-class.png", width=10, height=5.2, res=200, units="in")
print(gg)
dev.off()
