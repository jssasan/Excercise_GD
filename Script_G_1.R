
# Goal: Please use the data set to prepare a summary of important trends, changes, and events in the appropriate marketing channels 
# and products. Your summary should be useful to both a senior marketing executive, who has just a few seconds to spend on the 
# report, and to a marketing product manager, who will be looking for more detail and greater depth of analysis.


require(ggplot2)
require(reshape2)
require(dplyr)
require(scales)
  
## data import
filePath <- "C:\\Users\\jasmeet\\Desktop\\GoDaddy\\input.csv"
df_1 <- read.csv(file= filePath, header = TRUE)

head(df_1)
str(df_1)

## Changing to date format
df_1$Date <- as.Date(df_1$Date,format= "%m/%d/%Y")

## index for x-axis
df_1$index <- c(1:nrow(df_1))

df <- df_1   
str(df)




#### Multiple Linear Regression Model   #### 

## Additional Variables-- Strategy-Change Variables 

# 1/14/2008	Hats price change: $15 to $18
df$hats_Price_Change <- ifelse(df$Product == "Hats" & 
                              as.numeric(format(as.Date(df$Date,format="%m/%d/%Y"), "%d")) > 13, 1, 0 )

df$hats_Price_Change <- as.factor(df$hats_Price_Change)

# 1/6/2008	New website promotion for hats
df$website_hats_promotion <- ifelse(df$Channel == "Website" & 
                              df$Product == "Hats" & 
                             as.numeric(format(as.Date(df$Date,format="%m/%d/%Y"), "%d")) > 6, 1, 0 )

df$website_hats_promotion <- as.factor(df$website_hats_promotion)

# 1/18/2008	Product launch: Shoe 2.0
df$product_launch <- ifelse(as.numeric(format(as.Date(df$Date,format="%m/%d/%Y"), "%d")) == 18, 1, 0 )

df$product_launch <- as.factor(df$product_launch)


# 1/22/2008	Sandals price change: $28 to $20
df$sandals_Price_Change <- ifelse(df$Product == "Sandals" & 
                                 as.numeric(format(as.Date(df$Date,format="%m/%d/%Y"), "%d")) > 21, 1, 0 )

df$sandals_Price_Change <- as.factor(df$sandals_Price_Change)

## some more variables

# Day of The Week
df$dayOfWeek <- weekdays(df$Date)
df$dayOfWeek <- as.factor(df$dayOfWeek)

# weekend or weekday
df$weekend <- ifelse(df$dayOfWeek == "Saturday" | df$dayOfWeek == "Sunday"  , 1, 0)
df$weekend <- as.factor(df$weekend)

# category - footwear or headgear
df$category <- ifelse(df$Product == "Sandals" | df$Product == "Shoes" , "Footwear", "Headgear")
df$category <- as.factor(df$category)

str(df)

## converted everything to numeric except dates
# df[-1]  <- lapply(df[,-1], as.numeric)




## Scatter Plot
df_1_scatter <- melt(df_1[,c(4:6)], id.vars = "index")

p1 <- ggplot(data=df_1_scatter,aes(x = index,y = value))+
  geom_point(aes(color= variable))+
  facet_wrap(~variable, scales = "free", ncol = 3)   +
  geom_smooth(method = "lm") 

p1
ggsave("df_1_scatter.pdf",p1)

## Box Plot -- check outliers 

b1 <- ggplot(data=df_1_scatter,aes(x = index,y = value))+
  geom_boxplot(aes(color= variable)) +
  facet_wrap(~variable, scales = "free", ncol = 3) 
b1
ggsave("df_1_box.pdf",b1)

### Linear Regression Model

lm.model_1<- lm(data = df, Units~ . -Date -weekend -dayOfWeek -category -index  )  ## dropping variables which were insignifcant in a separate run of LM (weekend dayOfWeek category) 
summary(lm.model_1)

## anova  -- to check the significance of categorical variables with response variables i.e. revenue
anova_op <- anova(lm.model_1)

# Regression Diagnostic Plots 
pdf("lm.model_1.pdf")
par(mfrow= c(2,2))
plot(lm.model_1, col= c("dodgerblue3", "darkorange"))
dev.off()

## actual vs Fitted
actpred_df <- data.frame(index= df$index, actual_Units = df$Units, predicted_Units= lm.model_1$fitted.values)
actpred_df_melted<- melt(actpred_df, id.vars = "index", value.name = "Units")


ggplot(actpred_df_melted) +
  geom_line(aes(x=index, y=Units, color=variable),linetype="solid", size=1)+
  geom_point(aes(x=index, y=Units, color=variable))+
  scale_color_manual(values = c("red", "green"))+
  scale_x_continuous(breaks = pretty( actpred_df_melted$index,n=26))+
  scale_y_continuous(breaks = pretty_breaks(n=12)) +
  labs(title="Actual Vs Predicted Units")

ggsave(filename = "actual_vs_pred_1.pdf", plot = last_plot(), width = 10, height = 10)




