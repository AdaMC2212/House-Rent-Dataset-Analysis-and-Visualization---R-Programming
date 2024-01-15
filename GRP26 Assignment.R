# Chan Chun Ming, TP068720
# Chia Yong Xiang, TP068455
# Chin Yu Shyang, TP068794
# Chooi Yao Feng, TP068872



#packages
{
  install.packages("corrplot")
  install.packages("dplyr")
  install.packages("ggplot2")
  install.packages("reshape2")
  install.packages("tidyr")
  install.packages("ggpubr")
  install.packages("plotrix")
  library(plotrix)
  library(corrplot)
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(tidyr)
  library(ggpubr)
  library(plotrix)
}

#Data import
{
#read file
houseRent = read.csv("C:\\Users\\adamc\\OneDrive\\Desktop\\APU stuff\\YEAR 2 SEM 1\\PFDA\\assignment\\House_Rent_Dataset.csv")

#remove missing values
houseRent = na.omit(houseRent)

#extract unique value
houseRent = unique(houseRent)
}

#group by city
{
bangaloreHR = subset(houseRent,City == "Bangalore",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))
chennaiHR = subset(houseRent,City == "Chennai",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))
delhiHR = subset(houseRent,City == "Delhi",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))                
hyderabadHR = subset(houseRent,City == "Hyderabad",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))
kolkataHR = subset(houseRent,City == "Kolkata",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))
mumbaiHR = subset(houseRent,City == "Mumbai",select=c(Rent, Size, Furnishing.Status, Tenant.Preferred, Point.of.Contact))
}

# Data Cleaning
{
  # clean the Rent column
  ggplot(subset(houseRent,Rent<=1000000), aes(City,Rent,color = City)) + geom_boxplot()
  
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=250000 
                                           & houseRent$City == "Bangalore"),quantile(bangaloreHR$Rent,c(0.75)))
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=200000 
                                           & houseRent$City == "Chennai"),quantile(chennaiHR$Rent,c(0.75)))
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=200000 
                                           & houseRent$City == "Delhi"),quantile(delhiHR$Rent,c(0.75)))
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=175000 
                                           & houseRent$City == "Hyderabad"),quantile(hyderabadHR$Rent,c(0.75)))
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=175000 
                                           & houseRent$City == "Kolkata"),quantile(kolkataHR$Rent,c(0.75)))
  houseRent$Rent = replace(houseRent$Rent,(houseRent$Rent >=175000 
                                           & houseRent$City == "Mumbai"),quantile(mumbaiHR$Rent,c(0.75)))
  
  ggplot(subset(houseRent,Rent<=1000000), aes(City,Rent,color = City)) + geom_boxplot()
  
  #Clean the Size column
  ggplot(subset(houseRent,Size<=10000), aes(City,Size,color = City)) + geom_boxplot()
  
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >5000 
                                           & houseRent$City == "Bangalore"),quantile(bangaloreHR$Size,c(0.75)))
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >5000 
                                           & houseRent$City == "Chennai"),quantile(chennaiHR$Size,c(0.75)))
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >5000 
                                           & houseRent$City == "Delhi"),quantile(delhiHR$Size,c(0.75)))
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >5000 
                                           & houseRent$City == "Hyderabad"),quantile(hyderabadHR$Size,c(0.75)))
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >3000 
                                           & houseRent$City == "Kolkata"),quantile(kolkataHR$Size,c(0.75)))
  houseRent$Size = replace(houseRent$Size,(houseRent$Size >4000 
                                           & houseRent$City == "Mumbai"),quantile(mumbaiHR$Size,c(0.75)))
  
  
  houseRent = subset(houseRent,Point.of.Contact != "Contact Builder")
  
  
  
  ggplot(subset(houseRent,Size<=10000), aes(City,Size,color = City)) + geom_boxplot()
  
  temp_data = houseRent
}

# Data Pre-processing
{
  
  houseRent$SizeCategory <- ifelse(houseRent$Size > 800, "Size > 800", "Size <= 800")
  houseRent$RentCategory <- ifelse(houseRent$Rent > 5000, "Rent > 5000", "Rent <= 5000")

}

# Data Exploration
{
  #structure
  str(houseRent)
  
  #no. of rows and cols
  dim(houseRent)
  
  # cols name
  names(houseRent)
  
  #summary
  summary(houseRent)
}

# Analysis

# Chan Chun Ming
{
  ###Question 1: Question 1: How tenant preferred affect other aspects of the hypothesis?
  ###Analysis 1.1: Find the distribution of tenant preferred. 
  #Count the rows for every every category
  BFC = nrow(subset(houseRent,Tenant.Preferred == "Bachelors/Family"))
  BC = nrow(subset(houseRent,Tenant.Preferred == "Bachelors"))
  FC = nrow(subset(houseRent,Tenant.Preferred == "Family"))
  
  #
  tpLabel = c("Bachelors/Family","Bachelors","Family")
  tpValue = c(BFC,BC,FC)
  tpPerc = round(100*tpValue/sum(tpValue),1)
  pie(tpValue, labels=tpPerc, main = "Tenant Preferred Distribution", col = rainbow(length(tpValue)))
  legend("topright",tpLabel,cex = 0.5, fill = rainbow(length(tpValue)))
  
  ###Analysis 1.2: Find rent distribution according to tenant preferred. (Boxplot)
  ggplot(houseRent, aes(x = Tenant.Preferred, y = Rent, fill = Tenant.Preferred)) +
    geom_boxplot() +
    geom_hline(yintercept = 5000) + 
    labs(title = "Distribution of Rent categorised by Tenant Preferred", 
         x = "Tenant Preferred",
         y = "Rent") 
  
  #Analysis 1.3: Find size distribution accordin to tenant preferred. (Density Plot)
  ggplot(houseRent, aes(x = Size, color = Tenant.Preferred)) + geom_density() +
    geom_vline(xintercept = 800) +
    labs(title = "Distribution of Size categorised by Tenant Preferred", 
         x = "Size",
         y = "Density") 
  
  #Analysis 1.4: Find the distribution of tenant preferred according to furnished status.
  ggplot(houseRent, aes(x = Tenant.Preferred, y = Furnishing.Status, fill)) +
    geom_tile(stat = "bin2d") +
    stat_bin2d(geom = "text", aes(label = ..count..), vjust = -1) +
    scale_fill_continuous(low = "yellow",high = "red") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust =1)) +
    labs(title = "Distribution of Tenant Preferred and Furnishing Status", 
         x = "Tenant Preferred",
         y = "Rent") 
  
  #Analysis 1.5: Find the distribution for point of contact by tenant preferred 
  count_PCTP = houseRent%>%
    group_by(Tenant.Preferred,Point.of.Contact)%>%
    summarise(count = n())
  ggplot(count_PCTP, aes(x = Tenant.Preferred, y = count, fill = Point.of.Contact)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    labs(title = "Distribution for Point of Contact by Tenant Preferred", 
         x = "Tenant Preferred",
         y = "Count") 
  
  ###Question 2: How tenant preferred, and other attributes affect the correlation between rent price and furnishing status
  #Analysis 2.1：Find the distribution of rent and furnishing status if the house has no preferred tenant.
  ggplot(houseRent, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
    geom_violin() +
    geom_hline(yintercept = 5000) +
    labs(title = "Distribution of Rent categorised by Furnishing Status and Tenant Preffered", 
         x = "Tenant Preferred",
         y = "Rent") +
    facet_wrap(~Tenant.Preferred)
  
  #Analysis 2.2：Find the distribution of rent and furnishing status if the house has no preferred tenant.
  ggplot(houseRent, aes(x = Size, y = Rent)) +
    geom_point(aes(shape = Furnishing.Status, col = Tenant.Preferred)) +
    stat_smooth(method = lm) +
    labs(title = "Distribution of Rent and Sized according to Furnishing Status and Tenant Preferred", 
         x = "Size",
         y = "Rent") +
    facet_wrap(~Tenant.Preferred)
  
  #Analysis 2.3: Find the distribution of rent and furnishing status if the owner’s house has no preferred tenant. 
  OW = subset(houseRent, houseRent$Point.of.Contact == "Contact Owner")
  ggplot(OW, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
    geom_violin() +
    geom_hline(yintercept = 5000) +
    labs(title = "Distribution of Rent where the owner is the one to contact categorised by Furnishing Status and Tenant Preffered ", 
         x = "Tenant Preferred",
         y = "Rent") +
    facet_wrap(~Tenant.Preferred)
  
  
  ###Question 3:
  #Analysis 3:
  RS = mutate(houseRent,RentStatus = ifelse(Rent>5000,"Higher than 5000","Lower than 5000"))
  BF_CO_S_RS = subset(RS,((RS$Size>800)&
                            (RS$Tenant.Preferred=="Bachelors/Family")&
                            (RS$Point.of.Contact=="Contact Owner")))
  ggplot(BF_CO_S_RS, aes(x = Furnishing.Status, fill = Furnishing.Status)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(title = "Count of owner with house size bigger than 800 regardless of tenant",
         x = "Furnishing Status",
         y = "Count") +
    facet_wrap(~RentStatus)
  
}

# Chia Yong Xiang
{
#Q1 Which categories have more people that are willing to rent an unfurnished house for more than 5000?
# Prove Hypo
#Q1A1 Find the distribution of unfurnished houses
{
#count the number of occurrences of each furnishing status
UF = nrow(houseRent[houseRent$Furnishing.Status=="Unfurnished",])
SF = nrow(houseRent[houseRent$Furnishing.Status=="Semi-Furnished",])
FF = nrow(houseRent[houseRent$Furnishing.Status=="Furnished",])

# Calculate the percentages
total <- UF + SF + FF
percentage_UF <- round((UF / total) * 100, 2)
percentage_SF <- round((SF / total) * 100, 2)
percentage_FF <- round((FF / total) * 100, 2)

# Create a vector of percentages and labels
FS <- c(UF,SF,FF)
percentages <- c(percentage_UF, percentage_SF, percentage_FF)
name <- c("Unfurnished","Semi-furnished","Furnished")
#plot in pie chart
pie(FS, label = paste(name,FS,percentages,"%"), main = "Distribution of Furnishing Status")
}

#Q1A2 Find the distribution between the point of contact by furnishing status
{
  # Create a heatmap graph
  plot_heatmap1 <- houseRent %>%
    ggplot(aes(x = Furnishing.Status, y = Point.of.Contact)) +
    geom_tile(stat = "bin2d") +
    scale_fill_continuous(low = "yellow", high = "red") +  # Set colors for the heatmap
    labs(x = "Furnishing Status", y = "Point of Contact", fill = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    stat_bin_2d(geom = "text", aes(label = ..count..), vjust = -1)+  # Display frequency labels
    ggtitle("Distribution of Contact Point and Furnishing Status")  # Add the title
  
  print(plot_heatmap1)
}

#Q1A3 Find the distribution between rental fee and owners by furnishing status
{
  # Filter the data to include only Contact Owner
  CO_dataset = subset(houseRent, houseRent$Point.of.Contact =="Contact Owner")
  
  #Divide the rent by range
  houseRent <- houseRent %>%
    mutate(RentRange = as.integer(Rent / 5000))
  
  # Group by RentRange and summarize the count
  summary_stats <- CO_dataset %>%
    group_by(Furnishing.Status, RentRange) %>%
    summarise(count = n())
  
  # Create a histogram
  ggplot(summary_stats, aes(x = RentRange, y = count)) +
    geom_bar(stat = "identity", fill = "green") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed")+
    labs(title = "Rent Ranges of Owners sorted by Furnishing Status",
         x = "Rent Range (5 x 10e3)",
         y = "Count") +
    theme_bw()+
    facet_wrap(~Furnishing.Status, ncol = 1)  # Facet by Furnishing Status
}

#Q1A4 Find the distribution between tenant and owners who owns houses more than 5000 by furnishing status
{
  # Filter the data to include only "Contact Owner", Rent higher than 5000
  CO_LR_dataset=subset(houseRent, Point.of.Contact =="Contact Owner" 
                          & Rent > 5000)
  
  # Create a bar chart
  ggplot(CO_LR_dataset, aes(x = Tenant.Preferred, fill = Tenant.Preferred)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    labs(title = "Tenant of Owners' Properties that costs more than 5000 sorted by Furnishing Status",
         x = "Tenant Preferred",
         y = "Count") +
    theme_classic()+
    theme(legend.position = "bottom")+
    facet_wrap(~Furnishing.Status)
}

#Q1A5 Find the distribution between the house size and owners who owns houses to family and bachelors more than 5000 by furnishing status
{
  # Filter the data to include only "Contact Owner", Rent higher than 5000, "Family and Bachelor"
  BF_CO_HR_dataset=subset(houseRent,Tenant.Preferred == "Bachelors/Family"
                             & Point.of.Contact =="Contact Owner" 
                             & Rent >5000)
  
  # Divide the size by range
  houseRent <- houseRent %>%
    mutate(SizeRange = as.integer(Size / 100))
  
  # Group by SizeRange and summarize the count
  summary_stats_2 <- BF_CO_HR_dataset %>%
    group_by(Furnishing.Status,SizeRange) %>%
    summarise(count = n())
  
  # Create a scatterplot
  ggplot(summary_stats_2, aes(x = SizeRange, y = count)) +
    geom_point(color = "yellow") +  # Set the dot color to yellow
    geom_vline(xintercept = 8, color = "white", linetype = "dashed") +  # Add vertical line
    labs(title = "Size of Owners' Properties for Family and Bachelors that costs more than 5000",
         x = "Size (1 x 10e2)",
         y = "Count") +
    facet_wrap(~Furnishing.Status)+
    theme_dark()
}

#Q1A6 Find the distribution of owners who owns houses bigger than 800 to family and bachelors more than 5000 by furnishing status
{
  BF_CO_HR_LS_dataset=subset(houseRent,Tenant.Preferred == "Bachelors/Family"
                          & Point.of.Contact =="Contact Owner" 
                          & Rent >5000
                          & Size >800)
  
  ggplot(BF_CO_HR_LS_dataset, aes(x = Furnishing.Status, fill = Furnishing.Status)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5)) +
    labs(title = "Owners' Properties larger than 800 for Family and Bachelors that costs more than 5000",
         x = "Furnishing Status",
         y = "Count") +
    scale_color_brewer(palette = "Set3")+
    scale_fill_brewer(palette = "Set3")+
    theme_minimal()+
    theme(legend.position = "bottom")
  
}


#Question 2: How others factor influence furnishing status and rent? 
# Testing
#Q2A1 Find the relationship between Furnishing Status and Rent.
{
  # Anova Test
  anova_model1 <- aov(Rent ~ Furnishing.Status, data = houseRent)
  anova_result1 <- summary(anova_model1)
  print(anova_result1)
  
  #Create a boxplot Graph
  ggplot(houseRent, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
    geom_boxplot() +  # Boxplot
    labs(title = "Boxplot of Rent by Furnishing Status",
         x = "Furnishing Status",
         y = "Rent") +
    theme_minimal()
}

#Q2A2 Find the relationship between Furnishing Status and Rent based on Point of Contact.
{
  # Subset the data with relevant variables
  anova_model2 <- houseRent %>%
    select(Furnishing.Status, Rent, Point.of.Contact)
  anova_result2 <- aov(Rent ~ Furnishing.Status * Point.of.Contact, data = anova_model2)
  summary(anova_result2)
  
  ggplot(houseRent, aes(x = Point.of.Contact, y = Rent, color = Furnishing.Status)) +
    geom_jitter(position = position_jitter(width = 0.2), alpha = 0.7) +
    labs(title = "Rent by Furnishing.Status and Point.of.Contact",
         x = "Point.of.Contact",
         y = "Rent") +
    scale_color_brewer(palette = "Set2")+
    scale_fill_brewer(palette = "Set2")+
    theme_minimal()
}

#Q2A3 Find the relationship between Furnishing Status and Rent based on Tenant.
{
  
anova_model3 <- houseRent %>%
  select(Furnishing.Status, Rent, Tenant.Preferred)
anova_result3 <- aov(Rent ~ Furnishing.Status * Tenant.Preferred, data = anova_model3)
summary(anova_result3)

ggplot(houseRent, aes(x = Tenant.Preferred, y = Rent, fill = Furnishing.Status)) +
  geom_violin(scale = "count", position = "dodge") +
  labs(title = "Rent by Furnishing.Status and Tenant.Preferred",
       x = "Tenant Preferred",
       y = "Rent") +
  theme_bw()
}

#Q2A4 Find the relationship between Furnishing Status and Rent based on Size.
{
  anova_model4 <- houseRent %>%
    select(Furnishing.Status, Rent, Size)
  anova_result4 <- aov(Rent ~ Furnishing.Status * Size, data = anova_model4)
  summary(anova_result4)
  
  model1 =subset(houseRent,select=c(Rent,Size,Furnishing.Status))
  regression_results <- houseRent %>%
    group_by(Furnishing.Status) %>%
    do(model = lm(Rent ~ Size, data = .))
  
  regression_coefficients <- regression_results %>%
    mutate(intercept = coef(model)[1], slope = coef(model)[2]) %>%
    select(Furnishing.Status, intercept, slope)
  
  ggplot(houseRent, aes(x = Size, y = Rent, color = Furnishing.Status)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") +  # Add linear regression lines
    labs(title = "Relationship Between Furnishing Status, Rent, and Size",
         x = "Size",
         y = "Rent") +
    theme_dark() +
    theme(legend.position = "bottom") +
    facet_wrap(~ Furnishing.Status)
}

}

# Chin Yu Shyang
{
#3.1
point_of_contact_summary <- houseRent %>%
  group_by(Point.of.Contact) %>%
  summarise(count = n())

donut_chart <- ggplot(point_of_contact_summary, aes(x = 1, y = count, fill = Point.of.Contact)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0.8), color = "white", fill = "white") +
  geom_bar(stat = "identity", width = 1, position = "stack", color = "white") +
  coord_polar(theta = "y") +
  theme_void() +  # Remove background and gridlines
  scale_fill_manual(values = c("skyblue", "pink")) +  
  labs(fill = "Point of Contact") +
  labs(title = "Distribution of Point of Contact") +  
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) 

print(donut_chart)
#3.2
ggplot(point_of_contact_summary_size800, aes(x = Point.of.Contact, y = count, fill = Point.of.Contact)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +  # Add labels within stacked segments
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "pink")) + 
  labs(x = "Point of Contact", y = "Count", title = "Distribution of Point of Contact for Properties > 800 sq.ft")
#3.3
point_of_contact_summary_rent5000_size800 <- houseRent %>%
  filter(Rent > 5000) %>%
  mutate(SizeGroup = ifelse(Size > 800, ">800 sq.ft", "<=800 sq.ft")) %>%
  group_by(Point.of.Contact, SizeGroup) %>%
  summarise(count = n())

# Create a stacked bar chart with labels on top
bar_chart_rent5000_size800 <- ggplot(point_of_contact_summary_rent5000_size800, aes(x = Point.of.Contact, y = count, fill = SizeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add labels on top
  theme_minimal() +
  scale_fill_manual(values = c(">800 sq.ft" = "skyblue", "<=800 sq.ft" = "pink")) + 
  labs(x = "Point of Contact", y = "Count", title = "Distribution of Point of Contact for Properties with Rent > 5000 and Size > 800 sq.ft")

print(bar_chart_rent5000_size800)
#3.4
point_of_contact_summary_rent5000_size800 <- houseRent %>%
  filter(Rent > 5000) %>%
  filter(Size > 800) %>%
  group_by(Point.of.Contact, Furnishing.Status) %>%
  summarise(count = n())

bar_chart_rent5000_size800_furnishing <- ggplot(point_of_contact_summary_rent5000_size800, aes(x = Point.of.Contact, y = count, fill = Furnishing.Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add labels on top
  theme_minimal() +
  scale_fill_manual(values = c("Furnished" = "cyan", "Semi-Furnished" = "pink", "Unfurnished" = "aquamarine")) +
  labs(x = "Point of Contact", y = "Count", title = "Distribution of Point of Contact for Properties with Rent > 5000 and Size > 800 sq.ft by Furnishing Status")

print(bar_chart_rent5000_size800_furnishing)
3.5.1
point_of_contact_summary_rent5000_size800 <- houseRent %>%
  filter(Rent > 5000) %>%
  filter(Size > 800) %>%
  group_by(Point.of.Contact, Furnishing.Status, Tenant.Preferred) %>%
  summarise(count = n())

bar_chart_rent5000_size800_furnishing_tenant <- ggplot(point_of_contact_summary_rent5000_size800, aes(x = Point.of.Contact, y = count, fill = Furnishing.Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add labels on top
  facet_wrap(~ Tenant.Preferred) +  # Separate by Tenant Preferred
  theme_minimal() +
  scale_fill_manual(values = c("Furnished" = "cyan", "Semi-Furnished" = "pink", "Unfurnished" = "aquamarine")) +
  labs(x = "Point of Contact", y = "Count",
       title = "Distribution of Point of Contact for Properties with Rent > 5000 and Size > 800 sq.ft by Furnishing Status and Tenant Preferred")

print(bar_chart_rent5000_size800_furnishing_tenant)

point_of_contact_summary_rent5000_size800 <- houseRent %>%
  filter(Rent > 5000) %>%
  filter(Size > 800) %>%
  group_by(Point.of.Contact, Furnishing.Status, Tenant.Preferred) %>%
  summarise(count = n())



#3.5.2
heatmap_rent5000_size800_furnishing_tenant <- ggplot(point_of_contact_summary_rent5000_size800, aes(x = Point.of.Contact, y = Furnishing.Status, fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), color = "black", vjust = 0.5) +
  facet_wrap(~ Tenant.Preferred) +  # Separate by Tenant Preferred
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "sienna4") +  # Set color gradient
  labs(x = "Point of Contact", y = "Furnishing Status",
       title = "Heatmap of Point of Contact for Properties with Rent > 5000 and Size > 800 sq.ft by Furnishing Status and Tenant Preferred")

print(heatmap_rent5000_size800_furnishing_tenant)
#3.6
boxplot_rent50000 <- ggplot(houseRent, aes(x = Point.of.Contact, y = Rent, fill = Point.of.Contact)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Contact Agent" = "cyan", "Contact Owner" = "pink")) +
  labs(x = "Point of Contact", y = "Rent", title = "Comparison of Point of Contact and Rent") +
  theme_minimal() +
  theme(legend.position = 'none')

print(boxplot_rent50000)
#3.7
scatter_plot_tenant_rent <- ggplot(houseRent, aes(x = Tenant.Preferred, y = Rent, fill = Point.of.Contact)) +
  geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.2)) +
  labs(x = "Tenant Preferred", y = "Rent", title = "Relationship between Tenant Preferred and Rent") +
  theme_minimal() +
  scale_fill_manual(values = c("Contact Owner" = "cyan", "Contact Agent" = "pink"))  # Adjust colors as needed

print(scatter_plot_tenant_rent)
#3.8
scatter_plot_tenant_rent <- ggplot(houseRent, aes(x = Tenant.Preferred, y = Rent, color = Point.of.Contact)) +
  geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.2)) +
  labs(x = "Tenant Preferred", y = "Rent", title = "Relationship between Tenant Preferred and Rent") +
  theme_minimal()

print(scatter_plot_tenant_rent)

#3.9
scatter_plot_size_rent <- ggplot(houseRent, aes(x = Size, y = Rent, color = Point.of.Contact)) +
  geom_point(size = 3, alpha = 0.7, position = position_jitter(width = 0.2)) +
  labs(x = "Size", y = "Rent", title = "Position of Point of Contact Reguarding on Rent and Size") +
  theme_minimal()

print(scatter_plot_size_rent)
}

# Chooi Yao Feng
{# Question1 : Which categories have more people that are willing to pay more than 5000 rent price and house size >800.
  # Prove hypothesis
  {
    # Analysis 1.1:Find the distribution of houses which is larger than 800 sqrt feet. 
    
    houseRent$SizeCategory <- ifelse(houseRent$Size > 800, "Size > 800", "Size <= 800")
    size_counts <- table(houseRent$SizeCategory)
    size_percentages <- prop.table(size_counts) * 100
    colors <- c("lightblue", "lightgreen")
    labels <- paste(names(size_counts), "\n", round(size_percentages, 1), "%")
    pie(size_counts, labels = labels, col = colors, main = "Distribution of Property Sizes")
    
    #Analysis 1.2: Find the distribution of house based on different Furnishing Status when the property size is more than 800 sqft.
    summary_data <- table(houseRent$SizeCategory, houseRent$Furnishing.Status)
    summary_df <- as.data.frame(summary_data)
    colnames(summary_df) <- c("SizeCategory", "Furnishing.Status", "Count")
    bar01<-ggplot(summary_df, aes(x = SizeCategory, y = Count, fill = Furnishing.Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add text labels on bars
      labs(x = "Size Category", y = "Count",
           title = "Distribution of Size based on Size Category in different Furnishing Status",
           fill = "Furnishing Status") +
      theme_minimal()
    
    
    
    LS_dataset <- LS_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    summary_table <- LS_dataset %>%
      group_by(Furnishing.Status,SizeRange) %>%
      summarise(count = n())
    
    hist01<-ggplot(summary_table, aes(x = SizeRange, y = count)) +
      geom_bar(stat = "identity",fill="skyblue") +
      facet_wrap(~ Furnishing.Status, ncol = 1) +
      labs(x = "Size Range(1x10e2)", y = "Count",
           title = "Distribution of SizeRange(Size > 800) based on Furnishing Status",
           fill = "Size Range") +
      scale_fill_discrete(name = "Size Range") +
      theme_minimal()
    
    ggarrange(bar01,hist01,nrow=1)
    
    # Analysis 1.3:Find the distribution of house based on different Tenant Preferred when the property size is more than 800 sqft and the house is semi-furnished.
    summary_data_FS_TP <- table(houseRent$SizeCategory, houseRent$Furnishing.Status,houseRent$Tenant.Preferred)
    summary_df_FS_TP <- as.data.frame(summary_data_FS_TP)
    colnames(summary_df_FS_TP) <- c("SizeCategory", "Furnishing.Status","Tenant.Preferred","Count")
    bar02<-ggplot(summary_df_FS_TP, aes(x = SizeCategory, y = Count, fill = Tenant.Preferred)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add text labels on bars
      labs(x = "Size Category", y = "Count",
           title = "Relationship between Tenant Preferred and Size Category based on different Furnishing Status",
           fill = "Tenant Preferred") +
      theme_minimal()+
      facet_wrap(~Furnishing.Status)
    
    LS_SF_dataset <- LS_SF_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    summary_table <- LS_SF_dataset %>%
      group_by(Tenant.Preferred,SizeRange) %>%
      summarise(count = n())
    
    
    hist02<-ggplot(summary_table, aes(x = SizeRange, y = count)) +
      geom_bar(stat = "identity",fill="skyblue") +
      facet_wrap(~Tenant.Preferred , ncol = 1) +
      labs(x = "Size Range (1x10e2)", y = "Count",
           title = "Distribution of houses(Size>800 & Semi-Furnished) based on Tenant Preferred",
           fill = "Size Range") +
      scale_fill_discrete(name = "Size Range") +
      theme_minimal()
    ggarrange(bar02,hist02,nrow=1)
    
    
    #Analysis 1.4:Find the distribution of house based on different Point of Contact when the property size is more than 800 sqft and the house is semi-furnished regardless of tenants.
    summary_data_FS_TP_PC <- table(houseRent$SizeCategory, houseRent$Furnishing.Status,houseRent$Tenant.Preferred,houseRent$Point.of.Contact)
    summary_df_FS_TP_PC <- as.data.frame(summary_data_FS_TP_PC)
    colnames(summary_df_FS_TP_PC) <- c("SizeCategory", "Furnishing.Status","Tenant.Preferred","Point.of.Contact","Count")
    ggplot(summary_df_FS_TP_PC, aes(x = SizeCategory, y = Count, fill = Point.of.Contact)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add text labels on bars
      labs(x = "Size Category", y = "Count",
           title = "Distribution of houses in different situation",
           fill = "Point of Contact") +
      scale_y_continuous(limits = c(0,800))+
      theme_minimal()+
      facet_wrap(Furnishing.Status~Tenant.Preferred)
    
    ###
    LS_SF_BF_dataset <- LS_SF_BF_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    summary_table <- LS_SF_BF_dataset %>%
      group_by(Point.of.Contact,SizeRange) %>%
      summarise(count = n())
    
    
    ggplot(summary_table, aes(x = SizeRange, y = count)) +
      geom_bar(stat = "identity",fill="skyblue") +
      facet_wrap(~Point.of.Contact , ncol = 1) +
      labs(x = "Size Range", y = "Count",
           title = "Distribution of SizeRange(Size>800,Semi,Bachelors/Family) based on Point of Contact",
           fill = "Size Range") +
      scale_fill_discrete(name = "Size Range") +
      theme_minimal()
    
    #Analysis 1.5 Find the most majority cases in different situation based on Rent.
    SS_SF_BF_CO_dataset <- SS_SF_BF_CO_dataset %>% mutate(DatasetName = "SS_SF_BF_CO")
    LS_SF_BF_CO_dataset <- LS_SF_BF_CO_dataset %>% mutate(DatasetName = "LS_SF_BF_CO")
    SS_UF_BF_CO_dataset <- SS_UF_BF_CO_dataset %>% mutate(DatasetName = "SS_UF_BF_CO")
    
    SS_SF_BF_CO_dataset <- SS_SF_BF_CO_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    LS_SF_BF_CO_dataset <- LS_SF_BF_CO_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    SS_UF_BF_CO_dataset <- SS_UF_BF_CO_dataset %>%
      mutate(SizeRange = as.integer(Size / 100))
    
    
    # Combine the datasets
    all_datasets <- bind_rows(
      SS_SF_BF_CO_dataset,
      LS_SF_BF_CO_dataset,
      SS_UF_BF_CO_dataset
    )
    
    
    count_summary <- all_datasets %>%
      group_by(DatasetName, RentCategory) %>%
      summarise(count = n())
    
    # Create a stacked bar chart to compare RentCategory (>5000) based on DatasetName
    ggplot(count_summary, aes(x = RentCategory, y = count, fill = DatasetName)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +  # Add count labels
      labs(x = "Rent Category", y = "Count",
           title = "Comparison of RentCategory by different dataset",
           fill = "Situation") +
      theme_minimal() +
      theme(legend.position = "top")
    
  }
  
  # Testing
  {
    #Analysis 2.1 Find the relationship between Size and Rent in different SizeCategory and RentCategory. 
    
    modelv1 =subset(houseRent,select=c(Rent,Size,RentCategory,SizeCategory))
    regression_results <- houseRent %>%
      group_by(SizeCategory, RentCategory) %>%
      do(model = lm(Rent ~ Size, data = .))
    
    regression_coefficients <- regression_results %>%
      mutate(intercept = coef(model)[1], slope = coef(model)[2]) %>%
      select(SizeCategory, RentCategory, intercept, slope)
    
    table <- data.frame(regression_coefficients)
    table.p <- ggtexttable(table, rows = NULL)
    
    modelv2 <- left_join(modelv1,regression_coefficients,by=c("SizeCategory"="SizeCategory","RentCategory"="RentCategory"))
    # Visualize the regression lines
    sctt02<-ggplot(modelv2, aes(x = Size, y = Rent)) +
      geom_point(aes(color = RentCategory)) +
      geom_smooth(aes(group = RentCategory),
                  method = "lm", formula = y ~ x,
                  se = FALSE, color = "black") +
      facet_wrap(RentCategory~ SizeCategory, scales = "free") +
      labs(x = "Size", y = "Rent",
           title = "The relationship between Size and Rent by SizeCategory and RentCategory") +
      theme_minimal()
    ggarrange(sctt02,table.p,ncol=1,heights=c(0.7,0.3))
    
    modelv3 <- table(houseRent$SizeCategory, houseRent$RentCategory)
    chi_square_modelv3 <- chisq.test(modelv3)
    print(chi_square_modelv3)
    
    
    #Analysis 2.2: Find the relationship between Size and Rent based on different Furnishing Status and Size Category 
    modelv4 <- table(houseRent$SizeCategory, houseRent$Furnishing.Status)
    chi_square_modelv4 <- chisq.test(modelv4)
    print(chi_square_modelv4)
    
    modelv5 =subset(houseRent,select=c(Rent,Size,Furnishing.Status,SizeCategory))
    regression_results <- houseRent %>%
      group_by(SizeCategory, Furnishing.Status) %>%
      do(model = lm(Rent ~ Size, data = .))
    
    regression_coefficients <- regression_results %>%
      mutate(intercept = coef(model)[1], slope = coef(model)[2]) %>%
      select(SizeCategory, Furnishing.Status, intercept, slope)
    
    modelv6 <- left_join(modelv5,regression_coefficients,by=c("SizeCategory"="SizeCategory","Furnishing.Status"="Furnishing.Status"))
    
    table <- data.frame(regression_coefficients)
    table.p <- ggtexttable(table, rows = NULL)
    sctt01 <-ggplot(modelv6, aes(x = Size, y = Rent)) +
      geom_point(aes(color = Furnishing.Status)) +
      geom_smooth(aes(group = Furnishing.Status),
                  method = "lm", formula = y ~ x,
                  se = FALSE, color = "black") +
      facet_wrap(Furnishing.Status~ SizeCategory, scales = "free",nrow=3) +
      labs(x = "Size", y = "Rent",
           title = "The relationship between Size and Rent by SizeCategory and Furnishing Status") +
      theme_minimal()
    
    violin<-ggplot(houseRent, aes(x = Size, fill = Furnishing.Status)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ Furnishing.Status, ncol = 1) +
      geom_vline(xintercept = 800, color = "red", linetype = "solid",size=1) +
      geom_vline(aes(xintercept = median(Size)), color = "blue", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean(Size)), color = "green", linetype = "dotted", size = 1) +
      labs(x = "Size (sq ft)", y = "Density",
           title = "Density Plot of Size by Furnishing Status",
           fill = "Furnishing Status") +
      scale_fill_manual(values = c("Unfurnished"="cyan","Semi-Furnished"="lightgreen","Furnished"="orange")) 
    theme_minimal()
    ggarrange(sctt01,table.p,ncol=1,heights = c(0.7,0.3))
    
    #Analysis 2.3: Find the relationship between Size and Rent based on different Tenant Preferred and Size Category 
    modelv7 <- table(houseRent$SizeCategory, houseRent$Tenant.Preferred)
    chi_square_modelv7 <- chisq.test(modelv7)
    print(chi_square_modelv7)
    
    
    modelv8 =subset(houseRent,select=c(Rent,Size,Tenant.Preferred,SizeCategory))
    regression_results <- houseRent %>%
      group_by(SizeCategory, Tenant.Preferred) %>%
      do(model = lm(Rent ~ Size, data = .))
    
    regression_coefficients <- regression_results %>%
      mutate(intercept = coef(model)[1], slope = coef(model)[2]) %>%
      select(SizeCategory, Tenant.Preferred, intercept, slope)
    View(regression_coefficients)
    
    table <- data.frame(regression_coefficients)
    table.p <- ggtexttable(table, rows = NULL)
    
    modelv9 <- left_join(modelv8,regression_coefficients,by=c("SizeCategory"="SizeCategory","Tenant.Preferred"="Tenant.Preferred"))
    
    sctt03<-ggplot(modelv9, aes(x = Size, y = Rent)) +
      geom_point(aes(color = Tenant.Preferred)) +
      geom_smooth(aes(group = Tenant.Preferred),
                  method = "lm", formula = y ~ x,
                  se = FALSE, color = "black") +
      facet_wrap(Tenant.Preferred~ SizeCategory, scales = "free",nrow=3) +
      labs(x = "Size", y = "Rent",
           title = "The relationship between Size and Rent by SizeCategory and Tenant Preferred") +
      theme_minimal()
    
    ggarrange(sctt03,table.p,ncol=1,heights=c(0.7,0.3))
    
    #Analysis 2.4: Find the relationship between Size and Rent based on different Point of Contact and Size Category. 
    modelv10 <- table(houseRent$SizeCategory, houseRent$Point.of.Contact)
    chi_square_modelv10 <- chisq.test(modelv10)
    print(chi_square_modelv10)
    
    modelv11 =subset(houseRent,select=c(Rent,Size,Point.of.Contact,SizeCategory))
    regression_results <- houseRent %>%
      group_by(SizeCategory, Point.of.Contact) %>%
      do(model = lm(Rent ~ Size, data = .))
    
    regression_coefficients <- regression_results %>%
      mutate(intercept = coef(model)[1], slope = coef(model)[2]) %>%
      select(SizeCategory, Point.of.Contact, intercept, slope)
    
    table <- data.frame(regression_coefficients)
    table.p <- ggtexttable(table, rows = NULL)
    
    
    modelv12 <- left_join(modelv11,regression_coefficients,by=c("SizeCategory"="SizeCategory","Point.of.Contact"="Point.of.Contact"))
    
    sctt04<-ggplot(modelv12, aes(x = Size, y = Rent)) +
      geom_point(aes(color = Point.of.Contact)) +
      geom_smooth(aes(group = Point.of.Contact),
                  method = "lm", formula = y ~ x,
                  se = FALSE, color = "black") +
      facet_wrap(Point.of.Contact~ SizeCategory, scales = "free") +
      labs(x = "Size", y = "Rent",
           title = "The relationship between Size and Rent by SizeCategory and Point of Contact") +
      theme_minimal()
    
    ggarrange(sctt04,table.p,ncol=1,heights=c(0.7,0.3))
  }
  
  
  #Additional Features
  { 
    ###################
    ggplot(houseRent, aes(x =Size , y = Tenant.Preferred, color = SizeCategory)) +
      geom_jitter(width = 0.2, height = 0.3, alpha = 0.7) +
      labs(x = "Size (sq ft)", y = "Tenant Preferred",
           title = "Relationship between Tenant Preferred and Size (Jittered Scatter Plot)",
           color = "Tenant Preferred") +
      theme_minimal() +
      coord_flip() +
      theme(legend.position = "top") 
    
    
    ggplot(houseRent, aes(x = Point.of.Contact, y = Size, fill = Point.of.Contact)) +
      geom_violin(alpha = 0.7) +
      labs(x = "Point of Contact", y = "Size (sq ft)",
           title = "Violin Plot of Size by Point of Contact",
           fill = "Point of Contact") +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplot(houseRent, aes(x = Size, fill = Furnishing.Status)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ Furnishing.Status, ncol = 1) +
      geom_vline(xintercept = 800, color = "red", linetype = "solid",size=1) +
      geom_vline(aes(xintercept = median(Size)), color = "blue", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = mean(Size)), color = "green", linetype = "dotted", size = 1) +
      labs(x = "Size (sq ft)", y = "Density",
           title = "Density Plot of Size by Furnishing Status",
           fill = "Furnishing Status") +
      scale_fill_manual(values = c("Unfurnished"="cyan","Semi-Furnished"="lightgreen","Furnished"="orange")) 
    theme_minimal()
    
    
    
    
  }
  
  ############################################################################
  # Conclusion
  {
    frequency_table <- houseRent %>%
      group_by(SizeCategory, RentCategory, Furnishing.Status, Tenant.Preferred, Point.of.Contact) %>%
      summarise(count = n())
    View(frequency_table)
  }}
















