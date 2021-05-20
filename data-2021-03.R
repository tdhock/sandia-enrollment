library(readxl)
library(ggplot2)
library(data.table)
data.xlsx.vec <- Sys.glob("data-2021-03/*")
data.dt.list <- list()
for(data.xlsx in data.xlsx.vec){
  sheet.vec <- readxl::excel_sheets(data.xlsx)
  some.sheet.vec <- sheet.vec[-1]
  print(some.sheet.vec)
  for(sheet in some.sheet.vec){
    data.df <- readxl::read_xlsx(data.xlsx, sheet, .name_repair="minimal")
    cat(sprintf(
      "%s %s nrow=%d ncol=%d\n",
      data.xlsx, sheet, nrow(data.df), ncol(data.df)))
    tall.dt <- nc::capture_melt_single(
      data.df,
      nc::field("cell", "_", "[0-9]+", as.integer),
      value.name="resistance.ohms",
      na.rm=FALSE)
    temp.row <- nc::capture_first_vec(
      data.xlsx, degrees.C="[0-9]+", as.integer, "C_")
    data.dt.list[[paste(data.xlsx, sheet)]] <- data.table(
      temp.row,
      current.nA=as.integer(sub("nA", "", sheet)),
      tall.dt)
  }
}
(data.dt <- do.call(rbind, data.dt.list))

missing.dt <- data.dt[is.na(resistance.ohms), .(
  count=.N
), by=.(degrees.C, current.nA, cell)]

gg <- ggplot()+
  geom_tile(aes(
    factor(cell), current.nA, fill=count),
    data=missing.dt)+
  facet_grid(degrees.C ~ ., labeller=label_both)
png("data-2021-03-missing-count.png", width=8, height=3, res=200, units="in")
print(gg)
dev.off()

not.missing <- data.dt[!is.na(resistance.ohms)]
not.missing[, table(degrees.C)]
not.missing[, table(current.nA)]
not.missing[, table(table(cell))]
cell.counts <- not.missing[, .(
  count=.N
), by=cell]
cell.counts[count < max(count)]

data.table::fwrite(not.missing, "data-2021-03.csv")
