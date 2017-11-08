# For the first few plots, use the mpg dataset
library(ggplot2)
library(ggthemes)
# head(mpg)

# Histogram of hwy mpg values:
ggplot(data=mpg, aes(x=hwy)) + geom_histogram(bins=20, fill='red', alpha=0.5)

# Barplot of car counts per manufacturer with color fill defined by cyl count
ggplot(data=mpg, aes(x=manufacturer)) + geom_bar(aes(fill=factor(cyl)))

# Switch now to use the txhousing dataset that comes with ggplot2
# head(txhousing)

# Create a scatterplot of volume versus sales. 
# Afterwards play around with alpha and color arguments to clarify information.
ggplot(data=txhousing, aes(x=sales, y=volume)) + geom_point(color='blue',alpha=0.5)

# Add a smooth fit line to the scatterplot from above.
plt1 <- ggplot(data=txhousing, aes(x=sales, y=volume)) 
plt2 <- plt1 + geom_point(color='blue',alpha=0.5)
plt3 <- plt2 + geom_smooth(color='red')
Print(plt3)




