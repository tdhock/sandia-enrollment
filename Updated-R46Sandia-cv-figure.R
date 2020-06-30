library(data.table)
library(animint2)
r46cv <- readRDS("Updated-R46Sandia-cv.rds")
for(data.type in names(r46cv)){
  r46cv[[data.type]][, test.nA := factor(test.current)]
  r46cv[[data.type]][, Model := ifelse(model.name=="baseline", "baseline", "glmnet")]
}
test.loss <- r46cv$loss[set=="test"]
ggplot()+
  geom_tile(aes(
    train.cell, test.nA, fill=log(rmse)),
    data=test.loss)+
  coord_equal()+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(Model ~ ., labeller=label_both)

test.sum.tall <- test.loss[, .(
  sum.rmse=sum(rmse)
), by=.(train.cell, model.name)]
test.sum.wide <- dcast(
  test.sum.tall,
  train.cell ~ model.name,
  value.var="sum.rmse")[order(-glmnet)]
test.sum.wide[, rmse.diff := glmnet - baseline]

loss.wide <- dcast(
  test.loss,
  train.cell + test.nA ~ Model,
  value.var="rmse")
loss.wide[, rmse.diff := glmnet - baseline]
thresh <- 0.1
loss.wide[, rmse.delta := ifelse(rmse.diff>thresh, thresh, rmse.diff)]
ggplot()+
  geom_tile(aes(
    train.cell, test.nA, fill=rmse.delta),
    data=loss.wide)+
  coord_equal()+
  scale_fill_gradient2(low="blue", high="red")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))

loss.wide[, better := ifelse(glmnet>baseline, "baseline", "glmnet")]
ggplot()+
  geom_tile(aes(
    train.cell, test.nA, fill=better),
    data=loss.wide)+
  coord_equal()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))

ggplot()+
  geom_abline(slope=1, intercept=0, color="grey")+
  geom_text(aes(
    baseline, glmnet, label=train.cell),
    shape=1,
    data=loss.wide)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ test.nA, labeller=label_both)+
  scale_x_log10()+
  scale_y_log10()+
  coord_equal()

weight.stats <- r46cv$weight[, .(
  mean.weight=mean(weight),
  nonzero.weights=sum(weight!=0),
  sum.sign=sum(sign(weight))
), by=.(Model, train.cell, variable)]
ggplot()+
  geom_tile(aes(
    train.cell, variable, fill=nonzero.weights),
    data=weight.stats)+
  coord_equal()+
  scale_fill_gradient(low="white", high="violet")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(Model ~ ., labeller=label_both)

ggplot()+
  geom_tile(aes(
    train.cell, variable, fill=sum.sign),
    data=weight.stats)+
  coord_equal()+
  scale_fill_gradient2(low="orange", high="blue")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(Model ~ ., labeller=label_both)

dir.create("Updated-R46Sandia-cv-figure", showWarnings=FALSE)
cell.id.vec <- unique(r46cv$pred$train.cell)
for(cell.i in seq_along(cell.id.vec)){
  train.cell <- cell.id.vec[[cell.i]]
  out.png <- file.path(
    "Updated-R46Sandia-cv-figure",
    paste0(train.cell, ".png"))
  cat(sprintf("%d / %d %s\n", cell.i, length(cell.id.vec), out.png))
  select.dt <- data.table(train.cell)
  cell.pred <- r46cv$pred[select.dt, on="train.cell"]
  cell.line <- r46cv$line[select.dt, on="train.cell"]
  gg <- ggplot()+
    ggtitle(paste("cell", train.cell))+
    scale_size_manual(values=c(train=3, test=1))+
    geom_point(aes(
      neg.current, log.resistance, size=set),
      shape=21,
      data=cell.pred)+
    geom_line(aes(
      current, pred.y, color=Model),
      data=cell.line)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ test.nA, labeller=label_both)+
    scale_x_continuous(
      "current (nA)",
      limits=c(150, 450))
  png(out.png, width=8, height=2, res=100, units="in")
  print(gg)
  dev.off()
}

viz <- list(
  data=ggplot()+
    scale_size_manual(values=c(train=3, test=1))+
    geom_point(aes(
      neg.current, log.resistance, size=set),
      shape=21,
      showSelected="train.cell",
      data=r46cv$pred)+
    geom_line(aes(
      current, pred.y, color=Model, group=Model),
      showSelected="train.cell",
      data=r46cv$line)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(
      width=1000,
      height=300)+
    facet_grid(. ~ test.nA, labeller=label_both)+
    scale_x_continuous(
      "current (nA)",
      limits=c(150, 450)),
  weights=ggplot()+
    geom_tile(aes(
      train.cell, variable, fill=sum.sign),
      clickSelects="train.cell",
      data=weight.stats)+
    scale_fill_gradient2(low="orange", high="blue")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000, height=200)+
    facet_grid(Model ~ ., scales="free", space="free"),
  diff=ggplot()+
    geom_tile(aes(
      train.cell, test.nA, fill=rmse.delta),
      clickSelects="train.cell",
      data=loss.wide)+
    scale_fill_gradient2(low="blue", high="red")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000, height=200)
)
animint2dir(viz, "Updated-R46Sandia-cv-figure-interactive", open.browser=FALSE)
