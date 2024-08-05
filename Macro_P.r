library(readxl)

Data_P <- read_excel("D:/HBP/Disertasi/Macro_baru.xlsx", 
    sheet = "P", col_types = c("text", "text", 
        "skip", "text", "numeric", "numeric", 
        "numeric", "numeric"))
View(Data_P)
str(Data_P)
		tibble [828 × 7] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:828] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:828] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:828] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:828] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:828] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:828] 20 40 70 20 40 70 20 40 70 20 ...
		 $ P     : num [1:828] 2082 1361 1931 1303 1641 ...

# ------------- PREPROCESSING ------------------------

## 1. Outlier Removal using:
### iqr method = boxplot = 1.5*iqr = Tukey method
## 2. Data centering and scaling using:
### min-max normalization
## 3. eliminate NA or 

EL_Data_P <- Data_P %>% 
				mutate( # make quality control column for P named qc_P. outliers flagged in qc_P = 0, good data qc_P = 1
						qc_P = ifelse(P > quantile(P, 0.75) + 1.5*IQR(P) | P < quantile(P, 0.25) - 1.5*IQR(P), 0, 1), #based on IQR method = boxplot = 1.5iqr
				) %>% 
				mutate(
						P = ifelse(qc_P == 0, NA_real_, P)) %>% #eliminate the value of P, if qc_P = 0
				mutate(
						P_norm = (P - min(P, na.rm = TRUE)) / (max(P, na.rm = TRUE) - min(P, na.rm = TRUE)))%>%  #normalize the value using min-max normalization
				drop_na() #remove NA values

str(EL_Data_P)
		tibble [817 × 9] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:817] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:817] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:817] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:817] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:817] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:817] 20 40 70 20 40 70 20 40 70 20 ...
		 $ P     : num [1:817] 2082 1361 1931 1303 1641 ...
		 $ qc_P  : num [1:817] 1 1 1 1 1 1 1 1 1 1 ...
		 $ P_norm: num [1:817] 0.599 0.389 0.555 0.372 0.471 ...


# eliminate P and qc_P columns

EL_Data_P2 <- EL_Data_P %>% 
				select(-c(P, qc_P)) %>% # eliminate P and qc_P columns
				mutate(P = P_norm) %>% # set normalized P as P for input modelling
				select(-P_norm) # eliminate P_norm column

str(EL_Data_P2)
		tibble [817 × 7] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:817] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:817] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:817] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:817] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:817] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:817] 20 40 70 20 40 70 20 40 70 20 ...
		 $ P     : num [1:817] 0.599 0.389 0.555 0.372 0.471 ...


# one hot encoding the character columns
library(fastDummies)

EL_Data_P2 <- dummy_cols(EL_Data_P2, 
                         select_columns = c("Age", "Thick", "Season"), 
                         remove_selected_columns = TRUE)
str(EL_Data_P2)
		tibble [817 × 11] (S3: tbl_df/tbl/data.frame)
		 $ DC          : num [1:817] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT          : num [1:817] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth       : num [1:817] 20 40 70 20 40 70 20 40 70 20 ...
		 $ P           : num [1:817] 0.599 0.389 0.555 0.372 0.471 ...
		 $ Age_<6      : int [1:817] 1 1 1 1 1 1 1 1 1 1 ...
		 $ Age_>15     : int [1:817] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Age_6-15    : int [1:817] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Thick_<3    : int [1:817] 1 1 1 1 1 1 1 1 1 1 ...
		 $ Thick_>3    : int [1:817] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Season_Dry  : int [1:817] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Season_Rainy: int [1:817] 1 1 1 1 1 1 1 1 1 1 ...

		 
# Set the seed for reproducibility
set.seed(123)	
					
# Split the data into training and validation sets (I prefer to use dplyr/tidyverse style, my beloved wife )
# Load the rsample package
library(rsample)
split_P <- initial_split(EL_Data_P2, prop = 0.7)
trainSet_P <- training(split_P)
testSet_P <- testing(split_P)
	
P_train <- trainSet_P %>%
  mutate(Status = "TrainingP")
P_test <- testSet_P %>%
  mutate(Status = "ValidationP")
P_df <- bind_rows(P_train, P_test)	

str(P_df)
		tibble [817 × 12] (S3: tbl_df/tbl/data.frame)
		 $ DC          : num [1:817] 25 150 100 150 150 100 50 50 100 25 ...
		 $ DT          : num [1:817] 1 3 3 1 4 4 4 4 1 1 ...
		 $ Depth       : num [1:817] 20 20 40 40 70 20 70 20 40 40 ...
		 $ P           : num [1:817] 0.28149 0.25474 0.74451 0.00311 0.5453 ...
		 $ Age_<6      : int [1:817] 0 0 1 0 1 1 0 1 1 1 ...
		 $ Age_>15     : int [1:817] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Age_6-15    : int [1:817] 1 1 0 1 0 0 1 0 0 0 ...
		 $ Thick_<3    : int [1:817] 0 0 0 0 0 1 1 0 0 1 ...
		 $ Thick_>3    : int [1:817] 1 1 1 1 1 0 0 1 1 0 ...
		 $ Season_Dry  : int [1:817] 0 0 0 1 0 1 0 1 1 0 ...
		 $ Season_Rainy: int [1:817] 1 1 1 0 1 0 1 0 0 1 ...
		 $ Status      : chr [1:817] "TrainingP" "TrainingP" "TrainingP" "TrainingP" ...


# convert dataframe tiblles to maxtrix for convenience usage for XGBoost

x_P_train = subset(trainSet_P, select = -P) %>% as.matrix() 
y_P_train = trainSet_P$P

x_P_test = subset(testSet_P, select = -P) %>% as.matrix() 
y_P_test = testSet_P$P


## ------ Macro Modeling -------- ## 

# ---- multiple linear model P -----

set.seed = 42
tuned_lm_P <- train(
  x = x_P_train,
  y = y_P_train,
  method = "lm",
  family = "gaussian",
  metric = "RMSE",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
)

summary(tuned_lm_P)
		Call:
		lm(formula = .outcome ~ ., data = dat, family = "gaussian")

		Residuals:
			 Min       1Q   Median       3Q      Max 
		-0.36926 -0.14411 -0.03392  0.09827  0.75039 

		Coefficients: (3 not defined because of singularities)
					   Estimate Std. Error t value Pr(>|t|)    
		(Intercept)   0.2872257  0.0345104   8.323 6.53e-16 ***
		DC            0.0003327  0.0001777   1.873  0.06160 .  
		DT            0.0324043  0.0075502   4.292 2.09e-05 ***
		Depth        -0.0001459  0.0004096  -0.356  0.72190    
		`Age_<6`     -0.0167742  0.0205056  -0.818  0.41368    
		`Age_>15`    -0.1170168  0.0204584  -5.720 1.73e-08 ***
		`Age_6-15`           NA         NA      NA       NA    
		`Thick_<3`    0.0506274  0.0168061   3.012  0.00271 ** 
		`Thick_>3`           NA         NA      NA       NA    
		Season_Dry   -0.1829005  0.0167411 -10.925  < 2e-16 ***
		Season_Rainy         NA         NA      NA       NA    
		---
		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		Residual standard error: 0.1999 on 563 degrees of freedom
		Multiple R-squared:  0.2567,	Adjusted R-squared:  0.2475 
		F-statistic: 27.78 on 7 and 563 DF,  p-value: < 2.2e-16


## -------- LogGLM

set.seed(42)
logGLM_P <- train(
  x = x_P_train,
  y = y_P_train,
  method = "glm",
  family = "binomial",
  metric = "RMSE",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
)

summary(logGLM_P)
		Call:
		NULL

		Coefficients: (3 not defined because of singularities)
					   Estimate Std. Error z value Pr(>|z|)    
		(Intercept)  -0.9954688  0.3979713  -2.501   0.0124 *  
		DC            0.0018280  0.0020635   0.886   0.3757    
		DT            0.1757736  0.0880368   1.997   0.0459 *  
		Depth        -0.0007755  0.0047729  -0.162   0.8709    
		`Age_<6`     -0.0853842  0.2286515  -0.373   0.7088    
		`Age_>15`    -0.6582518  0.2449175  -2.688   0.0072 ** 
		`Age_6-15`           NA         NA      NA       NA    
		`Thick_<3`    0.2751849  0.1959995   1.404   0.1603    
		`Thick_>3`           NA         NA      NA       NA    
		Season_Dry   -0.9748494  0.1993613  -4.890 1.01e-06 ***
		Season_Rainy         NA         NA      NA       NA    
		---
		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		(Dispersion parameter for binomial family taken to be 1)

			Null deviance: 164.13  on 570  degrees of freedom
		Residual deviance: 123.54  on 563  degrees of freedom
		AIC: 575.63

		Number of Fisher Scoring iterations: 4


## ------- Multivariate Adaptive regression spline  --------

hyper_grid_mars_P <- expand.grid(
  degree = 1:5, 
  nprune = seq(2, 200, length.out = 10) %>% floor()
  )

set.seed(42)

# cross validated model
mars_P <- train(
  x = x_P_train,
  y = y_P_train,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10),
  tuneGrid = hyper_grid_mars_P)
  
# best model
mars_P$bestTune
		   nprune degree
		32     24      4

ggplot(mars_P)

summary(mars_P)
			Call: earth(x=matrix[571,10], y=c(0.2815,0.2547...),
						keepxy=TRUE, degree=4, nprune=24)

														 coefficients
			(Intercept)                                   0.281951244
			Age_>15                                      -0.216614502
			Thick_>3                                     -0.156846272
			Season_Rainy                                  0.111832174
			Age_<6 * Thick_>3                             0.092040119
			Age_>15 * Thick_>3                            0.271262610
			h(75-DC) * Age_>15                           -0.001449439
			h(3-DT) * Season_Rainy                        0.055150379
			h(DT-3) * Season_Rainy                        0.115691362
			h(DT-3) * Age_>15 * Season_Rainy              0.293846136
			h(75-DC) * h(3-DT) * Season_Rainy            -0.002408410
			h(75-DC) * h(3-DT) * Thick_<3 * Season_Rainy  0.002347745

			Selected 12 of 14 terms, and 7 of 10 predictors (nprune=24)
			Termination condition: Reached nk 21
			Importance: Season_Rainy, DT, Age_>15, Thick_>3, DC, ...
			Number of terms at each degree of interaction: 1 3 5 2 1
			GCV 0.03287655    RSS 16.94531    GRSq 0.3818494    RSq 0.4400567


## ----------- Cubist ------------------

grid_P_cub <- expand.grid(committees = c(1, 10, 50, 100, 150), neighbors = c(0, 1, 3, 5, 7, 9))

set.seed(42)
P_Cub <- train(
  x = x_P_train,
  y = y_P_train,
  method = "cubist",
  metric = "RMSE",
  tuneGrid = grid_P_cub,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
  )

P_Cub$bestTune
		   committees neighbors
		16         50         5		 
		 
		 
summary(P_Cub)
		Evaluation on training data (571 cases):

			Average  |error|          0.1314788
			Relative |error|               0.66
			Correlation coefficient        0.67


			Attribute usage:
			  Conds  Model

			   57%    44%    Season_Dry
			   35%    66%    DT
			   34%    50%    Age_>15
			   20%    48%    Thick_<3
			   11%    29%    DC
				8%    22%    Age_6-15
					   9%    Depth
					   4%    Age_<6


		Time: 0.1 secs



ggplot(P_Cub)


## ------------------Tree Regression ---------------


tGridRT <- expand.grid(cp = seq(0, .02, .0001))


set.seed(42)
TR_P <- train(
			x = x_P_train,
			y = y_P_train,
                   method = 'rpart',
                   metric = 'RMSE',
                   tuneGrid = tGridRT, 
				   trControl = trainControl(method = "repeatedcv", number = 10, repeats=10))

ggplot(TR_P)

TR_P$bestTune
			  cp
		91 0.009

			TR_P
			CART 

			571 samples
			 10 predictor

			No pre-processing
			Resampling: Cross-Validated (10 fold, repeated 10 times) 
			Summary of sample sizes: 513, 513, 515, 513, 514, 514, ... 
			Resampling results across tuning parameters:

			  cp      RMSE       Rsquared   MAE      
			  ......
			  0.0198  0.1619774  0.5109190  0.1188692
			  0.0199  0.1620770  0.5102454  0.1189356
			  0.0200  0.1620770  0.5102454  0.1189356

			RMSE was used to select the optimal model using the
			 smallest value.
			The final value used for the model was cp = 0.009.


# ------ random forest --------

## tuning RF 1

mtry <- sqrt(ncol(x_P_train))
#ntree: Number of trees to grow.
ntree <- 3

tunegrid_P <- expand.grid(.mtry = 5) 

set.seed(123)
RF_P <- train(
			x = x_P_train,
			y = y_P_train,
                   method = 'rf',
                   metric = 'RMSE',
                   tuneGrid = tunegrid_P, 
				   nodesize = 13,
					ntree = 250,
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats=10))
RF_P
		Random Forest 

		571 samples
		 10 predictor

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 514, 515, 513, 513, 515, 514, ... 
		Resampling results:

		  RMSE       Rsquared   MAE      
		  0.1457663  0.6086702  0.1065537

		Tuning parameter 'mtry' was held constant at a value of 5


## tuning RF 2
### search optimum range of RF parameterisation from ranger 

hyper_grid_RF_P_all <- expand.grid(
  mtry       = seq(2, 10, by = 1),
  node_size  = seq(20, 30, by = 1),
  num.trees	 = seq(50, 1000, by = 50),
  OOB_RMSE   = 0
)
#library(ranger)
for(i in 1:nrow(hyper_grid_RF_P_all)) {
  
  # train model
  model_rf_P_all <- ranger(
	x = x_P_train,
	y = y_P_train,
    num.trees       = hyper_grid_RF_P_all$num.trees[i],
    mtry            = hyper_grid_RF_P_all$mtry[i],
    min.node.size   = hyper_grid_RF_P_all$node_size[i],
    seed            = 42
  )
  
  # add OOB error to grid
  hyper_grid_RF_P_all$OOB_RMSE[i] <- sqrt(model_rf_P_all$prediction.error)
}

hyper_grid_RF_P_all %>% 
  dplyr::arrange(OOB_RMSE) %>%  
  arrange(OOB_RMSE) %>%
		  top_n(-10, wt = OOB_RMSE)
			   mtry node_size num.trees  OOB_RMSE
			1     6        20        50 0.1453887
			2     6        21        50 0.1456685
			3     8        20       400 0.1457980
			4     8        20       650 0.1458353
			5     8        20       850 0.1458412
			6     8        20       600 0.1458439
			7     8        20       950 0.1458582
			8     8        20       450 0.1458707
			9     8        20       900 0.1458807
			10    8        20       700 0.1458839
		# optimum mtry is located between 6 to 9, at nodesize 21, and ntree 950;  


### apply aforementioned ranger's tuning parameterisation to the caret
				
tunegrid_P2 <- expand.grid(.mtry = c(6:9)) 

set.seed(123)
RF_P2 <- train(
	x = x_P_train,
	y = y_P_train,
                   method = 'rf',
                   metric = 'RMSE',
                   tuneGrid = tunegrid_P2, 
				   nodesize = 21,
					ntree = 950,
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats=10))

print(RF_P2)
		Random Forest 

		571 samples
		 10 predictor

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 514, 515, 513, 513, 515, 514, ... 
		Resampling results across tuning parameters:

		  mtry  RMSE       Rsquared   MAE      
		  6     0.1483007  0.5949180  0.1084919
		  7     0.1476685  0.5970164  0.1074844
		  8     0.1474008  0.5977730  0.1069559
		  9     0.1475694  0.5967555  0.1070151

		RMSE was used to select the optimal model using the
		 smallest value.
		The final value used for the model was mtry = 8.


## tuning RF 3
## general adjustment based on ranger's tuning information

tunegrid_P3 <- expand.grid(.mtry = c(6:9)) 

RF_P3 <- train(
				x = x_P_train,
				y = y_P_train,
					method = "rf", 
					trControl = trainControl(method = "repeatedcv", number = 10, repeats=10), 
					metric= "RMSE",
					verbose = FALSE, 
					tuneGrid = tunegrid_P3,
					nodesize = c(20:21),   # tweaking the nodesize 
					n.trees = c(4:50)*100)  # tweaking the number of trees

  
print(RF_P3)
		Random Forest 

		571 samples
		 10 predictor

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 514, 514, 514, 514, 515, 513, ... 
		Resampling results across tuning parameters:

		  mtry  RMSE       Rsquared   MAE      
		  6     0.1482029  0.5944267  0.1082206
		  7     0.1476007  0.5962020  0.1071905
		  8     0.1473387  0.5968766  0.1067696
		  9     0.1475433  0.5953639  0.1068123

		RMSE was used to select the optimal model using the
		 smallest value.
		The final value used for the model was mtry = 8.



## tuning RF 4
## more adjustment/tweaking based on ranger's tuning information

tunegrid_P4 <- expand.grid(.mtry = c(7:9)) 

RF_P4 <- train(
				x = x_P_train,
				y = y_P_train,
					method = "rf", 
					trControl = trainControl(method = "repeatedcv", number = 10, repeats=10), 
					metric= "RMSE",
					verbose = FALSE, 
					tuneGrid = tunegrid_P4,
					nodesize = c(20:21),   # tweaking the nodesize 
					n.trees = seq(700, 1000, by = 25)) # tweaking the number of trees
  
print(RF_P4)
		Random Forest 

		571 samples
		 10 predictor

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 514, 514, 512, 514, 514, 514, ... 
		Resampling results across tuning parameters:

		  mtry  RMSE       Rsquared   MAE      
		  7     0.1474245  0.5963397  0.1069397
		  8     0.1472807  0.5961018  0.1066012
		  9     0.1473518  0.5952475  0.1064723

		RMSE was used to select the optimal model using the
		 smallest value.
		The final value used for the model was mtry = 8.		


# plot comparison for obtaining the RF final model

res_RF <- resamples(list(RF1 = RF_P, RF2 = RF_P2,  RF3 = RF_P3, RF4 = RF_P4))

summary(res_RF)
		Call:
		summary.resamples(object = res_RF)

		Models: RF1, RF2, RF3, RF4 
		Number of resamples: 100 

		MAE 
				  Min.    1st Qu.    Median      Mean   3rd Qu.
		RF1 0.07969558 0.09952845 0.1057493 0.1065537 0.1143108
		RF2 0.07956154 0.09966148 0.1069305 0.1069559 0.1155449
		RF3 0.07463149 0.09931632 0.1068161 0.1067696 0.1144668
		RF4 0.08295878 0.09800691 0.1061225 0.1066012 0.1133870
				 Max. NAs
		RF1 0.1437628    0
		RF2 0.1444822    0
		RF3 0.1341311    0
		RF4 0.1367180    0

		RMSE 
				  Min.   1st Qu.    Median      Mean   3rd Qu.
		RF1 0.10540656 0.1340603 0.1447655 0.1457663 0.1572972
		RF2 0.10458566 0.1361006 0.1464229 0.1474008 0.1585881
		RF3 0.09992823 0.1368104 0.1464659 0.1473387 0.1605112
		RF4 0.11252024 0.1355990 0.1486587 0.1472807 0.1588775
				 Max. NAs
		RF1 0.1960832    0
		RF2 0.1994631    0
		RF3 0.1825886    0
		RF4 0.1928988    0

		Rsquared 
				 Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
		RF1 0.3929387 0.5708817 0.6140418 0.6086702 0.6515194 0.7761929
		RF2 0.3735916 0.5531874 0.6036090 0.5977730 0.6404752 0.7790433
		RF3 0.3417787 0.5464178 0.6061722 0.5968766 0.6464292 0.8176122
		RF4 0.4151918 0.5480328 0.5976805 0.5961018 0.6474768 0.7734135
			NAs
		RF1    0
		RF2    0
		RF3    0
		RF4    0		
		
		
		
		
scales_RF <- list(x=list(relation="free"), y=list(relation="free"))

bwplot(res_RF, scales=scales_RF)

ggplot(res_RF, scales=scales_RF)

res_RF$values %>% #extract the values
  select(1, ends_with("Rsquared")) %>% #select the first column and all columns with a name ending with "Rsquared"
  gather(model, Rsquared, -1) %>% #convert to long table
  mutate(model = sub("~Rsquared", "", model)) %>% #leave just the model names
  ggplot()+ #call ggplot
  geom_boxplot(aes(x = Rsquared, y = model, fill=model)) -> plotres_RF #and plot the box plot

plot_res_RF<- plotres_RF + scale_y_discrete(limits = c("RF1", "RF2", "RF3", "RF4"))+
    scale_fill_viridis(discrete = TRUE) + theme_bw()  + 
	theme(text=element_text(size=13),
	axis.title.y=element_blank())+theme( legend.position="none") ## kesimpulan: RF4 is the best: stable (low deviance and moderate prediction)

plot_res_RF



# Gradient Boosting Machine

## notes for my beloved wife: I use 5-fold repeatedcv  for relieving computational burden. Feel sleepy and lonely without you tonight [-_-] wkwkwkwk. kangen
## you can wait forever for 10-fold repeatedcv wkwkwkw, I choose not. kangen [''_'']

# first step : fixing minobsinnode
grid_gbm_P <-expand.grid(n.trees = seq(50, 1000, by = 50),
			interaction.depth = c(1, 3, 5), 
			shrinkage = c(.01, .1, .3, .5),
			n.minobsinnode = c(3, 5, 7))
			

set.seed(1234)
GBM_P1 <- train(
				x = x_P_train,
				y = y_P_train,
	method 	= "gbm",
	metric 	= "RMSE",
	trControl = trainControl(method = "repeatedcv", number = 5, repeats=10),
	tuneGrid = grid_gbm_P,
	verbose = FALSE
)

GBM_P1$bestTune
			n.trees interaction.depth shrinkage n.minobsinnode
		140    1000                 5      0.01              3

#evaluate
ggplot(GBM_P1) ## my beloved wife, please analyze the graphs carefully. I found these:

# RMSE is lower and continued to stabilize at low shrinkage (0.01). 
# RMSE is lower at high interaction.depth (5)
# seemingly no difference in varying n.minobsinnode at interaction.depth = 5
# but if we look more closely, lowest RMSE is obtained at n.minobsinnode = 3 and ntrees = 1000 with slightly declining trend. 
# the opposite patterns are observed at higher n.minobsinnode

# conclusion: we develop model parameterisation with: low shrinkage (0.01), high interaction.depth (5), low n.minobsinnode (3), more ntrees (>850)
# we also try lowering the learning rate = shrinkage <0.01. suggested by Bradley https://bradleyboehmke.github.io/HOML/gbm.html
# tune more of other parameters, i.e., n.minobsinnode < 3 or interaction.depth >5 are never suggested in literatures. I never read it

# second step : try adding more trees. test if it can lower RMSE 
grid_gbm_P2 <-expand.grid(n.trees = seq(500, 5000, by = 500),
			interaction.depth = 5, 
			shrinkage = c(.001, 0.005, .01),
			n.minobsinnode = 3)
			

set.seed(1234)
GBM_P2 <- train(
				x = x_P_train,
				y = y_P_train,
	method 	= "gbm",
	metric 	= "RMSE",
	trControl = trainControl(method = "repeatedcv", number = 5, repeats=10),
	tuneGrid = grid_gbm_P2,
	verbose = FALSE
)

GBM_P2$bestTune
   n.trees interaction.depth shrinkage n.minobsinnode
17    3500                 5     0.005              3


#evaluate
ggplot(GBM_P2) 

# GBM_N2$bestTune suggests that lowest RMSE is obtained at moderately low learning rate = shrinkage = 0.005 when n.trees = 2000.
# but if we look at the curve, very small learning rate (shrinkage = 0.001) indicates lower RMSE at higher trees. 
# we use this information to develop much trees >5000 to chect if the RMSE could beat the bestTune's sugggestion


# third step : Okay. we try adding more trees >5000 . test if it can lower RMSE 
grid_gbm_P3 <-expand.grid(n.trees = seq(5000, 15000, by = 500),
			interaction.depth = 5, 
			shrinkage = 0.001,
			n.minobsinnode = 3)
			

set.seed(1234)
GBM_P3 <- train(
				x = x_P_train,
				y = y_P_train,
	method 	= "gbm",
	metric 	= "RMSE",
	trControl = trainControl(method = "repeatedcv", number = 5, repeats=10),
	tuneGrid = grid_gbm_P3,
	verbose = FALSE
)

GBM_P3$bestTune
		   n.trees interaction.depth shrinkage n.minobsinnode
		21   15000                 5     0.001              3

#evaluate
ggplot(GBM_P3) 

# the curve's inflection point located at n.trees = 9500. Adding more trees will higher the RMSE
# if we look at GBM_P2$results, the lowest RMSE at n.trees = 2000 and shrinkage = 0.005 is 0.1541962  
#  GBM_N3$results of n.trees = 9500 and shrinkage = 0.001 shows higher RMSE = 0.1542810. 
# okay we accept GBM_N2's bestTune and set GBM_N2 as best model


# we represent the parameterisation process by ggplot

res_GBM <- resamples(list(GBM1 = GBM_N1, GBM2 = GBM_N2, GBM3 = GBM_N3)) 
summary(res_GBM)
		Call:
		summary.resamples(object = res_GBM)

		Models: GBM1, GBM2, GBM3 
		Number of resamples: 50 

		MAE 
				  Min.   1st Qu.    Median      Mean   3rd Qu.
		GBM1 0.1043253 0.1149655 0.1215468 0.1204990 0.1264961
		GBM2 0.1035171 0.1146441 0.1209567 0.1203832 0.1267875
		GBM3 0.1037050 0.1150322 0.1212224 0.1204138 0.1263579
				  Max. NAs
		GBM1 0.1349979    0
		GBM2 0.1350976    0
		GBM3 0.1342789    0

		RMSE 
				  Min.   1st Qu.    Median      Mean   3rd Qu.
		GBM1 0.1356917 0.1467015 0.1535492 0.1542874 0.1620387
		GBM2 0.1343394 0.1470077 0.1529707 0.1541962 0.1616542
		GBM3 0.1349606 0.1468253 0.1530120 0.1542810 0.1617691
				  Max. NAs
		GBM1 0.1751799    0
		GBM2 0.1743286    0
		GBM3 0.1739755    0

		Rsquared 
				  Min.   1st Qu.    Median      Mean   3rd Qu.
		GBM1 0.3203226 0.4248878 0.4703728 0.4652185 0.5064340
		GBM2 0.3179806 0.4294322 0.4733646 0.4658446 0.5084679
		GBM3 0.3187166 0.4300325 0.4691100 0.4652491 0.5065379
				  Max. NAs
		GBM1 0.5963890    0
		GBM2 0.6027787    0
		GBM3 0.5965656    0
	
scales2 <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(res_GBM, scales=scales2)

ggplot(res_GBM, scales=scales2)

res_GBM$values %>% #extract the values
  select(1, ends_with("Rsquared")) %>% #select the first column and all columns with a name ending with "Rsquared"
  gather(model, Rsquared, -1) %>% #convert to long table
  mutate(model = sub("~Rsquared", "", model)) %>% #leave just the model names
  ggplot()+ #call ggplot
  geom_boxplot(aes(x = Rsquared, y = model, fill=model)) -> plotres_GBM #and plot the box plot

plot_res_GBM <- plotres_GBM + scale_y_discrete(limits = c("GBM1", "GBM2", "GBM3"))+ 
    scale_fill_viridis(discrete = TRUE) + theme_bw()  + 
	theme(text=element_text(size=13),
	axis.title.y=element_blank())+theme( legend.position="none")  # best model = GBM_N2

plot_res_GBM

# from this final GBM model, lets develop full 10-fold-cv iterated 10 times

grid_gbm_P_final <-expand.grid(n.trees = 2000,
			interaction.depth = 5, 
			shrinkage =  0.005,
			n.minobsinnode = 3)
			

set.seed(1234)
GBM_P_final <- train(
				x = x_P_train,
				y = y_P_train,
	method 	= "gbm",
	metric 	= "RMSE",
	trControl = trainControl(method = "repeatedcv", number = 10, repeats=10),
	tuneGrid = grid_gbm_P_final,
	verbose = FALSE
)

GBM_P_final
			Stochastic Gradient Boosting 

			571 samples
			 10 predictor

			No pre-processing
			Resampling: Cross-Validated (10 fold, repeated 10 times) 
			Summary of sample sizes: 515, 514, 514, 512, 512, 515, ... 
			Resampling results:

			  RMSE       Rsquared   MAE      
			  0.1576763  0.5366895  0.1192393

			Tuning parameter 'n.trees' was held constant at a value of
			 constant at a value of 0.005
			Tuning parameter
			 'n.minobsinnode' was held constant at a value of 3


## extreeme gradient boosting

# First step: Fixing nround, learning rate (eta), max tree depth
xgb_trc_P = trainControl(
  method = "repeatedcv",
  number = 5,  
  #repeats = 10, # drop this to maximize the computational speed. Soooo lonely and sleepy. kangen [--_--]
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


nrounds <- 1000
xgbGrid_P1 <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


set.seed(42) 
xgb_P1 = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P,
	tuneGrid = xgbGrid_P1,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P1$bestTune
		   nrounds max_depth   eta gamma colsample_bytree
		69     200         6 0.025     0                1
		   min_child_weight subsample
		69                1         1

#evaluate
ggplot(xgb_P1)
# lowest RMSE is obtained at lowest eta = 0.025
# All curves indicate lower nrounds (< 200)
# better max tree depth is between 3 to 4


# Second step: lower nrounds, set eta to 0.025,

nrounds2 <- 200
xgbGrid_P2 <- expand.grid(
  nrounds = seq(from = 20, to = nrounds2, by = 10),
  eta = 0.025,
  max_depth = c(3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(42) 
xgb_P2 = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P,
	tuneGrid = xgbGrid_P2,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P2$bestTune
		   nrounds max_depth   eta gamma colsample_bytree
		69     130         6 0.025     0                1
		   min_child_weight subsample
		69                1         1

#evaluate
ggplot(xgb_P2)
# low RMSE at nrounds = 100 - 150 
# choose max_depth = 5


# third step: fixing colsample_bytree, min_child_weight, and subsample. narrow nrounds

nrounds3 <- 150
xgbGrid_P3 <- expand.grid(
  nrounds = seq(from = 90, to = nrounds3, by = 5),
  eta = 0.025,
  max_depth = 5,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = c(1,2,3),
  subsample = c(0.5, 0.75, 1.0)
)

set.seed(42) 
xgb_P3 = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P,
	tuneGrid = xgbGrid_P3,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P3$bestTune
			nrounds max_depth   eta gamma colsample_bytree
		299     150         5 0.025     0              0.8
			min_child_weight subsample
		299                2      0.75



#evaluate
ggplot(xgb_P3)
# higher subsample (subsample = 1) significantly lower RMSE 
# lowest and stable RMSE is obtained at colsample_bytree = 0.8
# I cannot distinguish the differences betweeen min_child_weight's classess. it slightly indicates better RMSE at low rate. lets play more with it
# based on the curves, adding more nrounds apparently lowering RMSE


# fourth step: fixing min_child_weight. higher nrounds, also dont forget gamma.

nrounds4 <- 500
xgbGrid_P4 <- expand.grid(
  nrounds = seq(from = 100, to = nrounds4, by = 25),
  eta = 0.025,
  max_depth = 5,
  gamma = c(0,1),
  colsample_bytree = 0.8,
  min_child_weight = c(2,3),
  subsample = 1
)

set.seed(42) 
xgb_P4 = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P,
	tuneGrid = xgbGrid_P4,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P4$bestTune
		   nrounds max_depth   eta gamma colsample_bytree
		21     175         5 0.025     0              0.8
		   min_child_weight subsample
		21                3         1

#evaluate
ggplot(xgb_P4)
# jebakan betmen. steep inclined curve after 125. try smooth the nrounds
# worst RMSE at higher gamma
# it is clear that min_child_weight = 2 is the winner. we apply that.



# fifth step: smoothing and narrowing nrounds

nrounds5 <- 150
xgbGrid_P5 <- expand.grid(
  nrounds = seq(from = 100, to = nrounds5, by = 5),
  eta = 0.025,
  max_depth = 5,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 2,
  subsample = 1
)

set.seed(42) 
xgb_P5 = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P,
	tuneGrid = xgbGrid_P5,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P5$bestTune
		  nrounds max_depth   eta gamma colsample_bytree
		9     140         5 0.025     0              0.8
		  min_child_weight subsample
		9                2         1

#evaluate
ggplot(xgb_P5)
# yes finally fe found it. lowest RMSE at nrounds = 125.
# lets accept that xgb_N5 is the best XGB model that we succesfully tuned. 
# dont forget to build full model using 10-fold repeated cv

xgb_trc_P_final = trainControl(
  method = "repeatedcv",
  number = 10,  
  repeats = 10, 
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


xgbGrid_P_final <- expand.grid(
  nrounds = 125,
  eta = 0.025,
  max_depth = 5,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 2,
  subsample = 1
)

set.seed(42) 
xgb_P_final = train(
				x = x_P_train,
				y = y_P_train,
	trControl = xgb_trc_P_final,
	tuneGrid = xgbGrid_P_final,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_P_final 
			eXtreme Gradient Boosting 

			No pre-processing
			Resampling: Cross-Validated (10 fold, repeated 10 times) 
			Summary of sample sizes: 513, 513, 515, 513, 514, 514, ... 
			Resampling results:

			  RMSE       Rsquared   MAE      
			  0.1516911  0.5802154  0.1136997

			Tuning parameter 'nrounds' was held constant at a value of
			 2
			Tuning parameter 'subsample' was held constant at a value
			 of 1


#----------------------- Model Comparison -----------------------------------------

# performance by calibration method (internally by 10-fold 10-repeated cross validation)

models_compare_P <- resamples(list(LM=tuned_lm_P, LogGLM=logGLM_P, MARS=mars_P, Cubist=P_Cub, TR=TR_P, RF=RF_P4, GBM = GBM_P_final, XGB = xgb_P_final))

# Summary of the models performances
summary(models_compare_P)
			Call:
			summary.resamples(object = models_compare_P)

			Models: LM, LogGLM, MARS, Cubist, TR, RF, GBM, XGB 
			Number of resamples: 100 

			MAE 
						 Min.    1st Qu.    Median      Mean   3rd Qu.
			LM     0.12614304 0.14672331 0.1557500 0.1562311 0.1650869
			LogGLM 0.12972041 0.14970321 0.1565292 0.1565084 0.1658533
			MARS   0.09942195 0.13250781 0.1416406 0.1423738 0.1538691
			Cubist 0.08717218 0.10174195 0.1111178 0.1109157 0.1179836
			TR     0.08846290 0.10567928 0.1127905 0.1140076 0.1211590
			RF     0.08295878 0.09800691 0.1061225 0.1066012 0.1133870
			GBM    0.09537868 0.11177774 0.1201625 0.1192393 0.1259172
			XGB    0.09315563 0.10617297 0.1123332 0.1136997 0.1201269
						Max. NAs
			LM     0.1875943    0
			LogGLM 0.1944199    0
			MARS   0.1785493    0
			Cubist 0.1471867    0
			TR     0.1422917    0
			RF     0.1367180    0
			GBM    0.1438454    0
			XGB    0.1486220    0

			RMSE 
						Min.   1st Qu.    Median      Mean   3rd Qu.
			LM     0.1517093 0.1898150 0.2005157 0.2004358 0.2124491
			LogGLM 0.1656317 0.1875936 0.2036269 0.2019874 0.2141815
			MARS   0.1304892 0.1710815 0.1831194 0.1841027 0.1966009
			Cubist 0.1105659 0.1395597 0.1497231 0.1510312 0.1610636
			TR     0.1219036 0.1448917 0.1544335 0.1566463 0.1667798
			RF     0.1125202 0.1355990 0.1486587 0.1472807 0.1588775
			GBM    0.1188286 0.1461818 0.1593791 0.1576763 0.1678318
			XGB    0.1213836 0.1403591 0.1491870 0.1516911 0.1618229
						Max. NAs
			LM     0.2506248    0
			LogGLM 0.2549905    0
			MARS   0.2449848    0
			Cubist 0.1968731    0
			TR     0.1968676    0
			RF     0.1928988    0
			GBM    0.1924647    0
			XGB    0.2015845    0

			Rsquared 
						 Min.   1st Qu.    Median      Mean   3rd Qu.
			LM     0.08323335 0.2020303 0.2418604 0.2503341 0.2885591
			LogGLM 0.09004029 0.1865609 0.2347968 0.2382929 0.2792632
			MARS   0.14658807 0.3039928 0.3677853 0.3706655 0.4399257
			Cubist 0.36424182 0.5344001 0.5770520 0.5765854 0.6282087
			TR     0.34139570 0.4893715 0.5546110 0.5448169 0.6021978
			RF     0.41519184 0.5480328 0.5976805 0.5961018 0.6474768
			GBM    0.35371930 0.4969051 0.5441297 0.5366895 0.5863068
			XGB    0.38630660 0.5318182 0.5869568 0.5802154 0.6381187
						Max. NAs
			LM     0.4488856    0
			LogGLM 0.4576325    0
			MARS   0.6213126    0
			Cubist 0.7526505    0
			TR     0.7062493    0
			RF     0.7734135    0
			GBM    0.6820518    0
			XGB    0.7410606    0



scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare_P, scales=scales)

ggplot(models_compare_P, scales=scales)

models_compare_P$values %>% #extract the values
  select(1, ends_with("RMSE")) %>% #select the first column and all columns with a name ending with "RMSE"
  gather(model, RMSE, -1) %>% #convert to long table
  mutate(model = sub("~RMSE", "", model)) %>% #leave just the model names
  ggplot()+ #call ggplot
  geom_boxplot(aes(x = RMSE, y = model, fill=model)) -> p1_P #and plot the box plot

plot1_P <- p1_P + scale_y_discrete(limits = c("LM", "LogGLM", "MARS", "Cubist", "TR", "RF", "GBM", "XGB"))+
    scale_fill_viridis(discrete = TRUE) + theme_bw() + ggtitle("P") +
	theme(legend.position="none", axis.title.y = element_blank())	


models_compare_P$values %>% #extract the values
  select(1, ends_with("MAE")) %>% #select the first column and all columns with a name ending with "MAE"
  gather(model, MAE, -1) %>% #convert to long table
  mutate(model = sub("~MAE", "", model)) %>% #leave just the model names
  ggplot()+ #call ggplot
  geom_boxplot(aes(x = MAE, y = model, fill=model)) -> p2_P #and plot the box plot

plot2_P <- p2_P + scale_y_discrete(limits = c("LM", "LogGLM", "MARS", "Cubist", "TR", "RF", "GBM", "XGB"))+
    scale_fill_viridis(discrete = TRUE) + theme_bw() + 
	theme(legend.position="none", axis.title.y = element_blank())	


models_compare_P$values %>% #extract the values
  select(1, ends_with("Rsquared")) %>% #select the first column and all columns with a name ending with "Rsquared"
  gather(model, Rsquared, -1) %>% #convert to long table
  mutate(model = sub("~Rsquared", "", model)) %>% #leave just the model names
  ggplot()+ #call ggplot
  geom_boxplot(aes(x = Rsquared, y = model, fill=model)) -> p3_P #and plot the box plot

plot3_P <- p3_P + scale_y_discrete(limits = c("LM", "LogGLM", "MARS", "Cubist", "TR", "RF", "GBM", "XGB"))+
    scale_fill_viridis(discrete = TRUE)  + theme_bw() +  
	theme(legend.position="none", axis.title.y = element_blank())	

plot1_P+plot2_P+plot3_P


# performance by validation method (externally by the 30% validation data)
	
preds_MLR_P <- predict(tuned_lm_P, testSet_P)
preds_LOG_GLM_P <- predict(logGLM_P, testSet_P)
preds_MARS_P<- predict(mars_P, testSet_P)
preds_CUBIST_P <- predict(P_Cub, testSet_P)
preds_TR_P <- predict(TR_P, testSet_P)
preds_RF_P <- predict(RF_P4, testSet_P)
preds_GBM_P <- predict(GBM_P_final, testSet_P)
preds_XGB_P <- predict(xgb_P_final, as.matrix(x_P_test))


MLR_df_p <- data.frame(	Rsq = R2(preds_MLR_P, y_P_test),  
						RMSE = RMSE(preds_MLR_P, y_P_test),  
						MAE = MAE(preds_MLR_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_MLR_P)) %>% add_column(Model="MLR")

LogGLM_df_p <- data.frame(	Rsq = R2(preds_LOG_GLM_P, y_P_test),  
						RMSE = RMSE(preds_LOG_GLM_P, y_P_test),  
						MAE = MAE(preds_LOG_GLM_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_LOG_GLM_P))%>% add_column(Model="LogGLM")

MARS_df_p <- data.frame(Rsq = R2(preds_MARS_P, y_P_test),  
						RMSE = RMSE(preds_MARS_P, y_P_test),  
						MAE = MAE(preds_MARS_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_MARS_P))%>% add_column(Model="MARS")

names(MARS_df_p)[names(MARS_df_p) == 'y'] <- 'Rsq'

CUBIST_df_p <- data.frame(	Rsq = R2(preds_CUBIST_P, y_P_test),  
						RMSE = RMSE(preds_CUBIST_P, y_P_test),  
						MAE = MAE(preds_CUBIST_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_CUBIST_P))%>% add_column(Model="CUBIST")

TR_df_p <- data.frame(	Rsq = R2(preds_TR_P, y_P_test),  
						RMSE = RMSE(preds_TR_P, y_P_test),  
						MAE = MAE(preds_TR_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_TR_P))%>% add_column(Model="TR")

RF_df_p <- data.frame(	Rsq = R2(preds_RF_P, y_P_test),  
						RMSE = RMSE(preds_RF_P, y_P_test),  
						MAE = MAE(preds_RF_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_RF_P))%>% add_column(Model="RF")
						
GBM_df_p <- data.frame(	Rsq = R2(preds_GBM_P, y_P_test),  
						RMSE = RMSE(preds_GBM_P, y_P_test),  
						MAE = MAE(preds_GBM_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_GBM_P))%>% add_column(Model="GBM")					
						
XGB_df_p <- data.frame(	Rsq = R2(preds_XGB_P, y_P_test),  
						RMSE = RMSE(preds_XGB_P, y_P_test),  
						MAE = MAE(preds_XGB_P, y_P_test),
						BIAS = Metrics::bias(y_P_test, preds_XGB_P))%>% add_column(Model="XGB")					
						

merged_all_model_P <- bind_rows(MLR_df_p, LogGLM_df_p, MARS_df_p, CUBIST_df_p, TR_df_p, RF_df_p, GBM_df_p, XGB_df_p)

merged_all_model_P

				   Rsq      RMSE        MAE          BIAS  Model
			1 0.3186533 0.1947913 0.15409948 -0.0035577849    MLR
			2 0.3109532 0.1956260 0.15327617 -0.0035900534 LogGLM
			3 0.5202286 0.1634573 0.12645271 -0.0019067331   MARS
			4 0.6439785 0.1404074 0.10381790 -0.0014953062 CUBIST
			5 0.7007125 0.1288360 0.09507818 -0.0043946501     TR
			6 0.7021977 0.1290761 0.09635755  0.0001788308     RF
			7 0.6557814 0.1411204 0.11029352  0.0012934845    GBM
			8 0.6942145 0.1349530 0.10403311 -0.0090613624    XGB
