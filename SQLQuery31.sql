/*
-------------------------------------------------------
### Belgium SQL User group
### 16 March, 2017
### Author, presenter: Tomaž Kaštrun

## Email: tomaz.kastrun@gmail.com
## Blog: http://tomaztsql.wordpress.com
## Twitter: @tomaz_tsql
-------------------------------------------------------

*/


-- 0. Intro to SP_EXECUTE_EXTERNAL_SCRIPT

USE SQLR;
GO


-- or check the sp_configure
EXEC SP_CONFIGURE 'external scripts enabled',1
GO

RECONFIGURE;
GO

-- is MSSQLLaunchapd Installed
SELECT SERVERPROPERTY('IsAdvancedAnalyticsInstalled') 

SELECT @@VERSION

-- Check for the persmissions
SELECT *
FROM sys.fn_my_permissions(NULL, 'DATABASE')
WHERE
	 permission_name = 'EXECUTE ANY EXTERNAL SCRIPT';


-- 0.1. General outline of the external procedure

 EXECUTE sys.sp_execute_external_script
          @language = 
         ,@script = 
         ,@input_data_1 = 
         ,@input_data_1_name =
         ,@output_data_1_name =
         ,@parallel =
         ,@params = 
         ,@parameter1 =


--------------
-- STEP 1.
--------------

DECLARE @RScript nvarchar(max)
SET @RScript = N'OutputDataSet <- InputDataSet'

DECLARE @SQLScript nvarchar(max)
SET @SQLScript = N'SELECT soh.CustomerID, soh.OrderDate, sod.UnitPrice
FROM [AdventureWorks].Sales.SalesOrderHeader AS soh
INNER JOIN [AdventureWorks].Sales.SalesOrderDetail AS sod 
ON soh.SalesOrderID = sod.SalesOrderID
WHERE YEAR(soh.OrderDate) = 2014'

EXECUTE sp_execute_external_script
	 @language = N'R'
	,@script = @RScript
	,@input_data_1 = @SQLScript
WITH RESULT SETS (
					(CustomerID varchar(20)
					, OrderDate datetime
					, AvgUnitPrice money)
				  )


-- or sending T-SQL
SELECT 
		 soh.CustomerID
		,soh.OrderDate
		,sod.UnitPrice
	FROM [AdventureWorks].Sales.SalesOrderHeader AS soh
INNER JOIN [AdventureWorks].Sales.SalesOrderDetail AS sod 
ON soh.SalesOrderID = sod.SalesOrderID
WHERE 
	YEAR(soh.OrderDate) = 2014


--------------
-- STEP 2.
--------------


DECLARE @RScript nvarchar(max)
SET @RScript = N'df <- InputDataSet
				 #df$month <- factor(format(df$OrderDate, "%B"), levels = month.name)
				 mon_th <- format(df$OrderDate, "%b")
				 df$month <- factor(mon_th, levels=unique(mon_th), order=TRUE)
				 col1 <- levels(df$month)
				 col2 <- tapply(df$UnitPrice, df$month, mean)
				 OutputDataSet <- data.frame(col1, col2)'
				 


DECLARE @SQLScript nvarchar(max)
SET @SQLScript = N'
					SELECT 
						 soh.CustomerID
						,soh.OrderDate
						,sod.UnitPrice
					FROM [AdventureWorks].Sales.SalesOrderHeader AS soh
					INNER JOIN [AdventureWorks].Sales.SalesOrderDetail AS sod 
					ON soh.SalesOrderID = sod.SalesOrderID'

EXECUTE sp_execute_external_script
				@language = N'R',
				@script = @RScript,
				@input_data_1 = @SQLScript
WITH RESULT SETS (
				  ([Month] VARCHAR(20)
				  ,AvgUnitPrice MONEY)
				 )



-- or using t-SQL

SELECT 
	
	MONTH(soh.OrderDate) AS [Month]
	,AVG(sod.UnitPrice) AS AvgUnitPrice

FROM 
	[AdventureWorks].Sales.SalesOrderHeader AS soh
	INNER JOIN [AdventureWorks].Sales.SalesOrderDetail AS sod 
	ON soh.SalesOrderID = sod.SalesOrderID

GROUP BY
	MONTH(soh.OrderDate)
ORDER BY [Month] ASC


----------------------------------------------------	
-- 1. Using SP_EXECUTE_EXTERNAL_SCRIPT with T-SQL
----------------------------------------------------



-- Generating the chart!
DECLARE @RScript nvarchar(max)
SET @RScript = N'
				 library(cluster)	
				 image_file <- tempfile()
				 jpeg(filename = image_file, width = 400, height = 400)
				 mydata <- InputDataSet
				 d <- dist(mydata, method = "euclidean") 
				 fit <- hclust(d, method="ward.D")
				 plot(fit,xlab=" ", ylab=NULL, main=NULL, sub=" ")
				 groups <- cutree(fit, k=3) 
				 rect.hclust(fit, k=3, border="DarkRed")
				 dev.off() 
				 OutputDataSet <- data.frame(data=readBin(file(image_file, "rb"), what=raw(), n=1e6))'

DECLARE @SQLScript nvarchar(max)
SET @SQLScript = N'SELECT 
					 ps.[Name]
					,AVG(sod.[OrderQty]) AS OrderQty
					,so.[DiscountPct]
					,pc.name AS Category
				FROM  Adventureworks.[Sales].[SalesOrderDetail] sod
				INNER JOIN Adventureworks.[Sales].[SpecialOffer] so
				ON so.[SpecialOfferID] = sod.[SpecialOfferID]
				INNER JOIN Adventureworks.[Production].[Product] p
				ON p.[ProductID] = sod.[ProductID]
				INNER JOIN Adventureworks.[Production].[ProductSubcategory] ps
				ON ps.[ProductSubcategoryID] = p.ProductSubcategoryID
				INNER JOIN Adventureworks.[Production].[ProductCategory] pc
				ON pc.ProductCategoryID = ps.ProductCategoryID
				GROUP BY ps.[Name],so.[DiscountPct],pc.name'

EXECUTE sp_execute_external_script
	 @language = N'R'
	,@script = @RScript
	,@input_data_1 = @SQLScript
WITH RESULT SETS (
					(Hierarchical_cluster varbinary(max))
				 )
GO



-- Generating Data

DECLARE @RScript nvarchar(max)
SET @RScript = N'
				 library(cluster)	
				 mydata <- InputDataSet
				 d <- dist(mydata, method = "euclidean") 
				 fit <- hclust(d, method="ward.D")
				 #plot(fit,xlab=" ", ylab=NULL, main=NULL, sub=" ")
				 groups <- cutree(fit, k=3) 
				 #rect.hclust(fit, k=3, border="DarkRed")
				 #merge mydata and clusters
				 cluster_p <- data.frame(groups)
				 mydata <- cbind(mydata, cluster_p)

				 df_qt <- data.frame(table(mydata$OrderQty, mydata$groups),name = ''Qty'')
				 df_pc <- data.frame(table(mydata$DiscountPct, mydata$groups),name = ''Pct'')
				 df_cat <- data.frame(table(mydata$Category, mydata$groups),name = ''Cat'')
				 df_total <- df_qt
				 df_total <- rbind(df_total, df_pc)
				 df_total <- rbind(df_total, df_cat)
				 OutputDataSet <- df_total'

DECLARE @SQLScript nvarchar(max)
SET @SQLScript = N'SELECT 
					 ps.[Name]
					,AVG(sod.[OrderQty]) AS OrderQty
					,so.[DiscountPct]
					,pc.name AS Category
				FROM  Adventureworks.[Sales].[SalesOrderDetail] sod
				INNER JOIN Adventureworks.[Sales].[SpecialOffer] so
				ON so.[SpecialOfferID] = sod.[SpecialOfferID]
				INNER JOIN Adventureworks.[Production].[Product] p
				ON p.[ProductID] = sod.[ProductID]
				INNER JOIN Adventureworks.[Production].[ProductSubcategory] ps
				ON ps.[ProductSubcategoryID] = p.ProductSubcategoryID
				INNER JOIN Adventureworks.[Production].[ProductCategory] pc
				ON pc.ProductCategoryID = ps.ProductCategoryID
				GROUP BY ps.[Name],so.[DiscountPct],pc.name'

EXECUTE sp_execute_external_script
	 @language = N'R'
	,@script = @RScript
	,@input_data_1 = @SQLScript
	WITH result SETS ( (
						 Var1 VARCHAR(100)
						,Var2 VARCHAR(100)
						,Freq INT
						,name VARCHAR(100))
					 );

GO



---------------------------------------------------------
-- 2. Predicting Data (BikeBuyers; R code by Dejan Sarka)
---------------------------------------------------------

DECLARE @input AS NVARCHAR(MAX)
SET @input = N'
				SELECT CustomerKey, MaritalStatus, Gender,
				TotalChildren, NumberChildrenAtHome,
				Education, Occupation,
				HouseOwnerFlag, NumberCarsOwned, CommuteDistance,
				Region, BikeBuyer
				FROM SQLR.dbo.TargetMail;'

DECLARE @RKoda NVARCHAR(MAX)
SET @RKoda = N'
	library(RevoScaleR)
	bbLogR <- rxLogit(BikeBuyer ~  MaritalStatus + Gender + TotalChildren +
                    NumberChildrenAtHome + Education + Occupation +
                    HouseOwnerFlag + NumberCarsOwned + CommuteDistance + Region,
                  data = sqlTM);
	prtm <- rxPredict(modelObject = bbLogR, data = sqlTM, outData = NULL,
                  predVarNames = "BikeBuyerPredict", type = "response",
                  checkFactorLevels = FALSE, extraVarsToWrite = c("CustomerKey"),
                  writeModelVars = TRUE, overwrite = TRUE);
	OutputDataSet <- prtm[which(prtm$CustomerKey=="11000"),]';  -- Prediction is here

EXEC sys.sp_execute_external_script
 @language = N'R', 
 @script = @RKoda, 
  @input_data_1 = @input, 
  @input_data_1_name = N'sqlTM'

WITH RESULT SETS ((
	 BikeBuyerPredict FLOAT
	,CustomerKey INT
	,BikeBuyer INT
	,MaritalStatus NCHAR(1)
	,Gender NCHAR(1)
	,TotalChildren INT
	,NumberChildrenAtHome INT
	,Education NVARCHAR(40)
	,Occupation NVARCHAR(100)
	,HouseOwnerFlag NCHAR(1)
	,NumberCarsOwned INT
	,CommuteDistance NVARCHAR(15)
	,Region NVARCHAR(50)
 )); 
GO



---------------------------------------------------------
-- 3. PACKAGES, MULTIPARAMETERS, RESOURCE GOVERNON
---------------------------------------------------------

---- *******************************
---- RESOURCE GOVERNOR
---- *******************************

-- Enable Resource Governor
ALTER RESOURCE GOVERNOR RECONFIGURE;  
GO

-- Default value
ALTER EXTERNAL RESOURCE POOL [default] 
WITH (AFFINITY CPU = AUTO)
GO

CREATE EXTERNAL RESOURCE POOL RService_Resource_Pool  
WITH (  
     MAX_CPU_PERCENT = 10  
    ,MAX_MEMORY_PERCENT = 5
);  

ALTER RESOURCE POOL [default] WITH (max_memory_percent = 60, max_cpu_percent=90);  
ALTER EXTERNAL RESOURCE POOL [default] WITH (max_memory_percent = 40, max_cpu_percent=10);  
ALTER RESOURCE GOVERNOR reconfigure;

ALTER RESOURCE GOVERNOR RECONFIGURE;  
GO

-- CREATING CLASSIFICATION FUNCTION
CREATE WORKLOAD GROUP R_workgroup WITH (importance = medium) USING "default", 
EXTERNAL "RService_Resource_Pool";  

ALTER RESOURCE GOVERNOR WITH (classifier_function = NULL);  
ALTER RESOURCE GOVERNOR reconfigure;  

USE master  
GO  
CREATE FUNCTION RG_Class_function()  
RETURNS sysname  
WITH schemabinding  
AS  
BEGIN  
    IF program_name() in ('Microsoft R Host', 'RStudio') RETURN 'R_workgroup';  
    RETURN 'default'  
    END;  
GO  

ALTER RESOURCE GOVERNOR WITH  (classifier_function = dbo.RG_Class_function);  
ALTER RESOURCE GOVERNOR reconfigure;  
go



--- ##############################
---- INSTALLING MISSING PACKAGES
--- ##############################

USE WideWorldImporters;
GO
-- enable xp_cmdshell
EXECUTE SP_CONFIGURE 'xp_cmdshell','1';
GO

RECONFIGURE;
GO

EXEC xp_cmdshell '"C:\Program Files\Microsoft SQL Server\MSSQL13.MSSQLSERVER\R_SERVICES\bin\R.EXE" cmd -e install.packages(''tree'')';  
GO

--- ##############################
---- USING multiparameters
--- ##############################

USE WideWorldImporters;
GO

CREATE PROCEDURE Something

DECLARE @F_Value VARCHAR(1000)
DECLARE @Signif VARCHAR(1000)


EXECUTE sys.sp_execute_external_script
          @language = N'R'
         ,@script = N'mytable <- table(WWI_OrdersPerCustomer$CustomerID, WWI_OrdersPerCustomer$Nof_Orders) 
                     data.frame(margin.table(mytable, 2))
                     Ch <- unlist(chisq.test(mytable))
                     F_Val <- as.character(Ch[1])
                     Sig <- as.character(Ch[3])
                     OutputDataSet<-data.frame(margin.table(mytable, 2))'
         ,@input_data_1 = N'
							SELECT TOP 10 
							 CustomerID
							,COUNT(*) AS Nof_Orders 
						FROM [Sales].[Orders] 
						GROUP BY CustomerID'

         ,@input_data_1_name = N'WWI_OrdersPerCustomer'
         ,@params = N' @F_Val VARCHAR(1000) OUTPUT, @Sig VARCHAR(1000) OUTPUT'
         ,@F_Val = @F_Value OUTPUT 
         ,@Sig = @Signif OUTPUT
 WITH RESULT SETS(
                  (Cust_data INT
                  ,Freq INT)
                  )

SELECT @F_Value AS CHI_Value
    ,@Signif AS CHI_Square_SIGNIFICANCE
