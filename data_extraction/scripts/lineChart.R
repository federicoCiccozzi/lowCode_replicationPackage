# Line chart

setwd("/Users/federico/Dropbox/SMS_low_code/data_extraction/analysis")


# the file name of the pdf being produced
fileName <- "./output/years.pdf"

data <- read.delim("./input/data_extraction_form_all.txt", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "N/A")

# name of the X and Y axis labels
xLabel <- "Year"
yLabel <- "# of primary studies"

# name of the column of interest
columnName <- "Year"


# one element for each possible year
yearsInt <- c(2016:2021)
years <- as.factor(yearsInt)

#############################################################################

# twin array of possibleValue storing the values of each possible element
counts <- vector("integer", length(years))
for (i in 1:length(years)) {
  counts[i] <- 0
}

# the labels to be used in the line chart
labels <- ""

# counting the elements for each possible value
for (el in data[[columnName]]) {
  for (i in 1:length(years)) {
    if(grepl(years[i], el)) {
      counts[i] <- as.integer(counts[i]) + 1
    }
  }
}

# lastYear = counts[length(counts)]
# lastYearPercentage = round(lastYear/sum(counts) * 100, 2)

# compute the factor to which elements values must be scaled
#pct <- round(counts/sum(counts) * 100, 1)
#pct[length(pct)] <- pct[length(pct)] + 0.1
# we add the percentages to the labels
#labels <- paste(counts, "\n", pct, "%", sep="") # add percents to labels 
labels <- counts
#labels <- sub("0\n0%", "", labels)

# load the library ggplot2
library(ggplot2)

xSize <- 3
ySize <- 3

marginLeft <- 0
marginBottom <- 0
marginTop <- 0
marginRight <- 0


pdf(fileName, width=xSize, height=ySize)
par(mar=c(marginBottom, marginLeft, marginTop, marginRight))
par(mfrow=c(1, 1))
par(las=1)

# create the dataframe
df.papers <- data.frame(years,counts)

library(cowplot)
# in p now we have the final plot
p =ggplot(data=df.papers, aes(x=years, y=counts, group=1)) +
  geom_text(data=df.papers,aes(x=years,y=counts,label=labels),size=3, alpha=1, lineheight=1, vjust=-1.2,hjust=.5) + 
  labs(x=xLabel, y=yLabel) +
  theme_cowplot() +
  geom_point(size=3) + 
  ylim(0, max(counts)+1) + 
  #coord_fixed(ratio=.1) + 
  geom_line(data=subset(df.papers, years != "2021")) + 
  geom_line(linetype="dashed", data=subset(df.papers, years = "2021")) +
  theme(axis.line = element_line(color = 'black'), axis.text.y = element_text(size=10), axis.text.x = element_text(angle=90, size=10))
print(p)
dev.off()
