works_with_R('3.6.0', animint2="2019.7.3", data.table="1.12.8")
r46 <- fread("Updated-R46Sandia.csv")
ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    shape=1,
    data=r46)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("cell", labeller=label_both)+
  scale_y_log10()

ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    shape=1,
    data=r46)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("cell", labeller=label_both, scales="free")+
  scale_y_log10()

ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    shape=1,
    data=r46)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("cell", labeller=label_both, scales="free")

cell.stats <- r46[, .(
  min=min(resistance.ohms),
  max=max(resistance.ohms)
), by=.(cell)][order(min)]

r46[, group := ifelse(resistance.ohms < 120000, "low", "high")]
r46[, Cell := factor(cell, cell.stats$cell)]
ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    shape=1,
    data=r46[group=="low"])+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("Cell", labeller=label_both, scales="fixed")+
  scale_y_log10()

ggplot()+
  geom_point(aes(
    current.nA, resistance.ohms),
    shape=1,
    data=r46[group=="high"])+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_wrap("Cell", labeller=label_both, scales="fixed")+
  scale_y_log10()

r46[, log10.ohms := log10(resistance.ohms)]
r46[, sqrt.nA := sqrt(-current.nA)]
lm.dt <- r46[, {
  fit <- lm(log10.ohms ~ sqrt.nA, data=.SD)
  res.vec <- fit$residuals
  data.table(
    t(coef(fit)),
    root.mean.square.error=sqrt(mean(res.vec^2)))
}, by=.(cell)]
ggplot()+
  geom_point(aes(
    `(Intercept)`, sqrt.nA, fill=root.mean.square.error),
    shape=21,
    data=lm.dt)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()

lm.dt[, RMSE := root.mean.square.error]
viz <- animint(
  coefs=ggplot()+
    ggtitle("Linear regression parameters, select cell")+
    theme_bw()+
    theme_animint(width=500)+
    geom_point(aes(
      `(Intercept)`, sqrt.nA, key=1),
      showSelected="cell",
      shape=21,
      size=5,
      fill="grey",
      data=lm.dt)+
    geom_point(aes(
      `(Intercept)`, sqrt.nA, fill=RMSE),
      clickSelects="cell",
      shape=21,
      size=4,
      alpha=1,
      data=lm.dt)+
    scale_fill_gradient(low="white", high="red")+
    ylab("slope"),
  regression=ggplot()+
    ggtitle("Regression plot for selected cell")+
    theme_bw()+
    geom_abline(aes(
      slope=sqrt.nA, intercept=`(Intercept)`),
      clickSelects="cell",
      size=4,
      color="orange",
      alpha=0.8,
      data=lm.dt)+
    geom_point(aes(
      sqrt.nA, log10.ohms, key=paste(sqrt.nA, read)),
      showSelected="cell",
      data=r46)+
    geom_abline(aes(
      slope=sqrt.nA, intercept=`(Intercept)`, key=1),
      showSelected="cell",
      data=lm.dt),
  regressionUp=ggplot()+
    ggtitle("Regression plot axis updates")+
    theme_bw()+
    theme_animint(update_axes="y")+
    geom_point(aes(
      sqrt.nA, log10.ohms, key=paste(sqrt.nA, read)),
      showSelected="cell",
      data=r46)+
    geom_abline(aes(
      slope=sqrt.nA, intercept=`(Intercept)`, key=1),
      showSelected="cell",
      data=lm.dt),
  duration=list(
    cell=2000),
  title="Sandia cell current resistance measurements"
)
animint2dir(viz, "Updated-R46Sandia-figure", open.browser = FALSE)
