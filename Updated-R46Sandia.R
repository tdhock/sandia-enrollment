library(data.table)
file.xlsx <- "Updated-R46Sandia.xlsx"
sheet.name.vec <- readxl::excel_sheets(file.xlsx)
## Yes, we inject current increments on reverse bias, hence the
## negative current. So, in our case @ -200nA, ... -350nA and -400nA.
match.dt <- nc::capture_first_vec(
  sheet.name.vec,
  current.nA="[-0-9]+", as.integer,
  "nA", nomatch.error=FALSE)
match.dt[, sheet := sheet.name.vec]
nA.sheet.dt <- match.dt[is.finite(current.nA)]
## Testing equipment in lab is at ambient temperature, ~23C.


## For the 51 reads, that is equated to the average(reads) we see on
## the first notebook: R46 Averages. We can ignore it but if we want
## to see the Standard Deviations for those averages we can refer to
## each different notebook's current (i.e, -200nA, -250nA,
## etc.). There is no significance to the row number, it is something
## we can use to help analyze a cell's (or read) "intra-PUF variation"

## But to clarify, for the other excel notebooks from the first, I
## simply extract the averages of each current injection (46 reads for
## each current) and put it into the first excel notebook. I plotted
## those averages against column (I) to get my fit.



## Yes, RXX is a column identifier for when I call the variable into RStudio for plotting. So, 46 Reads total.
R.dt.list <- list()
for(sheet.i in 1:nrow(nA.sheet.dt)){
  one.row <- nA.sheet.dt[sheet.i]
  one.df <- readxl::read_excel(file.xlsx, sheet=one.row$sheet)
  ## Yes, within each column of RXX, there is atleast 5 resistance
  ## averages because we have 5 current injections. And yes,
  ## Resistance is in Ohms.
  one.df$read <- 1:nrow(one.df)
  one.tall <- nc::capture_melt_single(
    one.df,
    "R",
    cell="[0-9]+", as.integer,
    value.name="resistance.ohms")
  R.dt.list[[sheet.i]] <- data.table(
    one.row, one.tall)
}
R.dt <- do.call(rbind, R.dt.list)

fwrite(R.dt, "Updated-R46Sandia.csv")
