library(data.table)
R.dt.list <- list()
xlsx.vec <- Sys.glob("data-2020-06-30/Thermal Testing/*.xlsx")
for(xlsx.i in seq_along(xlsx.vec)){
  file.xlsx <- xlsx.vec[[xlsx.i]]
  degrees.C <- as.integer(gsub("[^0-9]", "", basename(file.xlsx)))
  sheet.name.vec <- readxl::excel_sheets(file.xlsx)
  match.dt <- nc::capture_first_vec(
    sheet.name.vec,
    current.nA="[-0-9]+", as.integer,
    "nA", nomatch.error=FALSE)
  match.dt[, sheet := sheet.name.vec]
  nA.sheet.dt <- match.dt[is.finite(current.nA)]
  for(sheet.i in 1:nrow(nA.sheet.dt)){
    one.row <- nA.sheet.dt[sheet.i]
    one.df <- readxl::read_excel(file.xlsx, sheet=one.row$sheet)
    one.df$read <- 1:nrow(one.df)
    one.tall <- nc::capture_melt_single(
      one.df,
      cell="[0-9]+", as.integer,
      "$",
      value.name="resistance.ohms")
    R.dt.list[[paste(xlsx.i, sheet.i)]] <- one.row[, data.table(
      degrees.C, current.nA, one.tall)]
  }
}
R.dt <- do.call(rbind, R.dt.list)

fwrite(R.dt, "data-2020-06-30-temp.csv")
