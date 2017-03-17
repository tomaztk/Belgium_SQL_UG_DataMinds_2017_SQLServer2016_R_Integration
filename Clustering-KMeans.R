#### 
###   Belgium SQL User group
###   R and SQL Server 2016
###   Tomaž Kaštrun; 16. MARCH, 2017
####


setwd('C:/DataTK')

library(ggplot2)
library(RODBC)


myconn <-odbcDriverConnect("driver={SQL Server};Server=SICN-KASTRUN;database=WideWorldImportersDW;trusted_connection=true")


cust.data <- sqlQuery(myconn, "SELECT 
                      fs.[Sale Key] AS SalesID
                      ,fs.[City Key] AS CityKey
                      ,c.[City] AS City
                      ,c.[State Province] AS StateProvince
                      ,c.[Sales Territory] AS SalesTerritory
                      ,fs.[Customer Key] AS CustomerKey
                      ,fs.[Stock Item Key] AS StockItem
                      ,fs.[Quantity] AS Quantity
                      ,fs.[Total Including Tax] AS Total
                      ,fs.[Profit] AS Profit
                      
                      
                      FROM [Fact].[Sale] AS  fs
                      JOIN dimension.city AS c
                      ON c.[City Key] = fs.[City Key]
                      WHERE
                      fs.[customer key] <> 0 ")



close(myconn) 

## Case of K-Means clustering
### Works ok!

#data sample
head(cust.data[,c(1,2,6,7,8)])


# Already getting some memory limitations if ran with default algorithm
startTimeKmeans <- proc.time()
SaleCluster <- kmeans(cust.data[,c(1,2,6,7,8)], 9, nstart = 20, algorithm="Lloyd")
proc.time() - startTimeKmeans
# ~ 3,5 second execution time 




# Don't even bother to run this code!
table(SaleCluster$cluster, cust.data$SalesTerritory)


#envoke RevoscaleR package
library(RevoScaleR)

startTimeKXmeans <- proc.time()
SalesCluster2 <- rxKmeans(formula= ~SalesID + CityKey + CustomerKey + StockItem + Quantity, data =cust.data, numCluster=9,algorithm = "lloyd", 
                          outFile = "SalesCluster.xdf", outColName = "Cluster", overwrite = TRUE)
proc.time() - startTimeKXmeans
# ~ 0,7 second execution time


table(SalesCluster2$cluster, cust.data$SalesTerritory)


# To show compatibilty between ScaleR and ggplot2 packages
ggplot(cust.data, aes(Quantity, Profit, color = cust.data$SalesTerritory)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = SalesCluster2) + 
  scale_color_manual(values = c('black', 'red', 'green','yellow','blue','lightblue','magenta','brown','lightgreen'))





################################################################
### Run Hierarchical clustering with narrow selection of data
################################################################
library(RODBC)

myconn <-odbcDriverConnect("driver={SQL Server};Server=SICN-KASTRUN;database=WideWorldImportersDW;trusted_connection=true")

cust.data <- sqlQuery(myconn, "SELECT					
                      SUM(fs.[Profit]) AS Profit
                      ,c.[Sales Territory] AS SalesTerritory
                      ,CASE 
                      WHEN c.[Sales Territory] = 'Rocky Mountain' THEN 1
                      WHEN c.[Sales Territory] = 'Mideast' THEN 2
                      WHEN c.[Sales Territory] = 'New England' THEN 3
                      WHEN c.[Sales Territory] = 'Plains' THEN 4
                      WHEN c.[Sales Territory] = 'Southeast' THEN 5
                      WHEN c.[Sales Territory] = 'Great Lakes' THEN 6
                      WHEN c.[Sales Territory] = 'Southwest' THEN 7
                      WHEN c.[Sales Territory] = 'Far West' THEN 8
                      END AS SalesTerritoryID
                      ,fs.[Customer Key] AS CustomerKey    
                      ,SUM(fs.[Quantity]) AS Quantity
                      ,SUM(fs.[Total Including Tax]) AS Total
                      
                      FROM [Fact].[Sale] AS  fs
                      JOIN dimension.city AS c
                      ON c.[City Key] = fs.[City Key]
                      WHERE
                      fs.[customer key] <> 0
                      AND c.[Sales Territory] NOT IN ('External')
                      GROUP BY c.[Sales Territory]
                      ,fs.[Customer Key]
                      ,CASE 
                      WHEN c.[Sales Territory] = 'Rocky Mountain' THEN 1
                      WHEN c.[Sales Territory] = 'Mideast' THEN 2
                      WHEN c.[Sales Territory] = 'New England' THEN 3
                      WHEN c.[Sales Territory] = 'Plains' THEN 4
                      WHEN c.[Sales Territory] = 'Southeast' THEN 5
                      WHEN c.[Sales Territory] = 'Great Lakes' THEN 6
                      WHEN c.[Sales Territory] = 'Southwest' THEN 7
                      WHEN c.[Sales Territory] = 'Far West' THEN 8
                      END 
                      order by CustomerKey")

close(myconn) 



#Hierarchical clustering

clusters <- hclust(dist(cust.data[, c(1,3,5,6)]))
plot(clusters)


clusterCut <- cutree(clusters, 5)
#table(clusterCut, cust.data$SalesTerritory)


ggplot(cust.data, aes(Total, Quantity, color = cust.data$SalesTerritory)) + 
  geom_point(alpha = 0.4, size = 2.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green','yellow','blue','lightblue','magenta','brown'))


