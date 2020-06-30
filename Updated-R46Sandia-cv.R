library(data.table)
library(splines)
library(glmnet)
library(ggplot2)

r46 <- fread("Updated-R46Sandia.csv")
r46[, neg.current := -current.nA]
u.current <- unique(r46$neg.current)
x.mat.list <- list()
y <- log(r46$resistance.ohms)
for(fun.name in c("identity", "sqrt", "log", "bs", "ns")){
  fun <- get(fun.name)
  result <- fun(r46$neg.current)
  if(!is.matrix(result))result <- cbind(result)
  for(col.i in 1:ncol(result)){
    x.mat.list[[paste0(fun.name, col.i)]] <- result[, col.i]
  }
}
x.mat <- do.call(cbind, x.mat.list)
u.mat <- unique(x.mat)

weight.dt.list <- list()
line.dt.list <- list()
loss.dt.list <- list()
pred.dt.list <- list()
cell.id.vec <- unique(r46$cell)
for(train.cell.i in seq_along(cell.id.vec)){
  train.cell <- cell.id.vec[[train.cell.i]]
  cat(sprintf("%4d / %4d cell=%d\n", train.cell.i, length(cell.id.vec), train.cell))
  is.cell <- r46$cell==train.cell
  cell.x.mat <- x.mat[is.cell, ]
  cell.y <- y[is.cell]
  cell.dt <- data.table(
    r46[is.cell],
    log.resistance=cell.y,
    sqrt1=cell.x.mat[, "sqrt1"])
  u.dt <- unique(cell.dt[, .(sqrt1)])
  for(test.current in u.current){
    is.test <- cell.dt$neg.current == test.current
    is.train <- !is.test
    train.x.mat <- cell.x.mat[is.train,]
    train.y <- cell.y[is.train]
    set.seed(1)
    fit.list <- list(
      glmnet=cv.glmnet(train.x.mat, train.y),
      baseline=lm(log.resistance ~ sqrt1, cell.dt))
    for(model.name in names(fit.list)){
      fit <- fit.list[[model.name]]
      w <- coef(fit)
      weight.dt.list[[paste(train.cell, test.current, model.name)]] <- data.table(
        train.cell, test.current, model.name,
        variable=if(model.name=="glmnet")rownames(w) else names(w),
        weight=as.numeric(w))
      line.dt.list[[paste(train.cell, test.current, model.name)]] <- data.table(
        train.cell, test.current, model.name,
        current=u.mat[, "identity1"],
        pred.y=as.numeric(predict(fit, if(model.name=="glmnet")u.mat else u.dt)))
      cell.pred.dt <- data.table(
        cell.y,
        pred.y=as.numeric(predict(fit, if(model.name=="glmnet")cell.x.mat else cell.dt)),
        set=ifelse(is.test, "test", "train"))
      cell.pred.dt[, residual := pred.y - cell.y]
      pred.dt.list[[paste(train.cell, test.current, model.name)]] <- data.table(
        train.cell, test.current, model.name,
        cell.dt,
        cell.pred.dt)
      cell.loss.dt <- cell.pred.dt[, .(
        rmse=sqrt(mean(residual^2)),
        sd.residual=sd(residual)
      ), by=set]
      loss.dt.list[[paste(train.cell, test.current, model.name)]] <- data.table(
        train.cell, test.current, model.name,
        cell.loss.dt)
    }
  }
}

r46cv <- list(
  weight=do.call(rbind, weight.dt.list),
  pred=do.call(rbind, pred.dt.list),
  loss=do.call(rbind, loss.dt.list),
  line=do.call(rbind, line.dt.list))
saveRDS(r46cv, file="Updated-R46Sandia-cv.rds")

