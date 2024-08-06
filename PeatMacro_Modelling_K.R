
library(readxl)

Data_K <- read_excel("D:/HBP/Disertasi/Macro_baru.xlsx", 
    sheet = "K", col_types = c("text", "text", 
        "skip", "text", "numeric", "numeric", 
        "numeric", "numeric"))
View(Data_K)
str(Data_K)
		tibble [854 × 7] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:854] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:854] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:854] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:854] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:854] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:854] 20 40 70 20 40 70 20 40 70 20 ...
		 $ K     : num [1:854] 787 237 624 252 259 ...


# ------------- PREPROCESSING ------------------------

## 1. Outlier Removal using:
### iqr method = boxplot = 1.5*iqr = Tukey method
## 2. Data centering and scaling using:
### min-max normalization
## 3. eliminate NA or 

EL_Data_K <- Data_K %>% 
  mutate( # make quality control column for K named qc_K. outliers flagged in qc_K = 0, good data qc_K = 1
    qc_K = ifelse(K > quantile(K, 0.75) + 1.5*IQR(K) | K < quantile(K, 0.25) - 1.5*IQR(K), 0, 1), #based on IQR method = boxplot = 1.5iqr
  ) %>% 
  mutate(
    K = ifelse(qc_K == 0, NA_real_, K)) %>% #eliminate the value of K, if qc_K = 0
  mutate(
    K_norm = (K - min(K, na.rm = TRUE)) / (max(K, na.rm = TRUE) - min(K, na.rm = TRUE)))%>%  #normalize the value using min-max normalization
  drop_na() #remove NA values

str(EL_Data_K)
		tibble [781 × 9] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:781] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:781] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:781] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:781] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:781] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:781] 20 40 70 20 40 70 20 40 70 20 ...
		 $ K     : num [1:781] 787 237 624 252 259 ...
		 $ qc_K  : num [1:781] 1 1 1 1 1 1 1 1 1 1 ...
		 $ K_norm: num [1:781] 0.894 0.264 0.707 0.281 0.288 ...


# eliminate K and qc_K columns

EL_Data_K2 <- EL_Data_K %>% 
  select(-c(K, qc_K)) %>% # eliminate K and qc_K columns
  mutate(K = K_norm) %>% # set normalized K as K for input modelling
  select(-K_norm) # eliminate K_norm column

str(EL_Data_K2 )
		tibble [781 × 7] (S3: tbl_df/tbl/data.frame)
		 $ Age   : chr [1:781] "<6" "<6" "<6" "<6" ...
		 $ Thick : chr [1:781] "<3" "<3" "<3" "<3" ...
		 $ Season: chr [1:781] "Rainy" "Rainy" "Rainy" "Rainy" ...
		 $ DC    : num [1:781] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT    : num [1:781] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth : num [1:781] 20 40 70 20 40 70 20 40 70 20 ...
		 $ K     : num [1:781] 0.894 0.264 0.707 0.281 0.288 ...
# one hot encoding the character columns

library(fastDummies)

EL_Data_K2 <- dummy_cols(EL_Data_K2, 
                         select_columns = c("Age", "Thick", "Season"), 
                         remove_selected_columns = TRUE)
str(EL_Data_K2)
		tibble [781 × 11] (S3: tbl_df/tbl/data.frame)
		 $ DC          : num [1:781] 10 10 10 10 10 10 10 10 10 10 ...
		 $ DT          : num [1:781] 1 1 1 2 2 2 3 3 3 4 ...
		 $ Depth       : num [1:781] 20 40 70 20 40 70 20 40 70 20 ...
		 $ K           : num [1:781] 0.894 0.264 0.707 0.281 0.288 ...
		 $ Age_<6      : int [1:781] 1 1 1 1 1 1 1 1 1 1 ...
		 $ Age_>15     : int [1:781] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Age_6-15    : int [1:781] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Thick_<3    : int [1:781] 1 1 1 1 1 1 1 1 1 1 ...
		 $ Thick_>3    : int [1:781] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Season_Dry  : int [1:781] 0 0 0 0 0 0 0 0 0 0 ...
		 $ Season_Rainy: int [1:781] 1 1 1 1 1 1 1 1 1 1 ...


# Set the seed for reproducibility
set.seed(123) 

# Split the data into training and validation sets (I prefer to use dplyr/tidyverse style, my beloved wife )
# Load the rsample package
library(rsample)
split_K <- initial_split(EL_Data_K2, prop = 0.7)
trainSet_K <- training(split_K)
testSet_K <- testing(split_K)

K_train <- trainSet_K %>%
  mutate(Status = "TrainingK")
K_test <- testSet_K %>%
  mutate(Status = "ValidationK")
K_df <- bind_rows(K_train, K_test) 

str(K_df)
		tibble [781 × 12] (S3: tbl_df/tbl/data.frame)
		 $ DC          : num [1:781] 50 25 100 10 150 100 75 75 100 25 ...
		 $ DT          : num [1:781] 4 1 1 4 2 3 4 2 4 2 ...
		 $ Depth       : num [1:781] 40 40 40 70 70 70 40 40 70 20 ...
		 $ K           : num [1:781] 0.2743 0.0946 0.2261 0.1441 0.4239 ...
		 $ Age_<6      : int [1:781] 0 0 1 0 1 1 0 1 1 1 ...
		 $ Age_>15     : int [1:781] 0 0 0 1 0 0 0 0 0 0 ...
		 $ Age_6-15    : int [1:781] 1 1 0 0 0 0 1 0 0 0 ...
		 $ Thick_<3    : int [1:781] 0 0 0 1 0 1 1 0 0 1 ...
		 $ Thick_>3    : int [1:781] 1 1 1 0 1 0 0 1 1 0 ...
		 $ Season_Dry  : int [1:781] 0 1 0 0 0 1 0 1 1 0 ...
		 $ Season_Rainy: int [1:781] 1 0 1 1 1 0 1 0 0 1 ...
		 $ Status      : chr [1:781] "TrainingK" "TrainingK" "TrainingK" "TrainingK" ...


# convert dataframe tiblles to maxtrix for convenience usage for XGBoost

x_K_train = subset(trainSet_K, select = -K) %>% as.matrix() 
y_K_train = trainSet_K$K

x_K_test = subset(testSet_K, select = -K) %>% as.matrix() 
y_K_test = testSet_K$K


## ------ Macro Modeling -------- ## 

# ---- multiple linear model K -----

set.seed = 42
tuned_lm_K <- train(
  x = x_K_train,
  y = y_K_train,
  method = "lm",
  family = "gaussian",
  metric = "RMSE",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
)

summary(tuned_lm_K)
		Call:
		lm(formula = .outcome ~ ., data = dat, family = "gaussian")

		Residuals:
			 Min       1Q   Median       3Q      Max 
		-0.38585 -0.16092 -0.04698  0.12212  0.69390 

		Coefficients: (3 not defined because of singularities)
					   Estimate Std. Error t value Pr(>|t|)    
		(Intercept)   0.3959315  0.0375102  10.555  < 2e-16 ***
		DC           -0.0005710  0.0001947  -2.933 0.003502 ** 
		DT           -0.0187230  0.0082364  -2.273 0.023407 *  
		Depth        -0.0010726  0.0004317  -2.485 0.013263 *  
		`Age_<6`      0.0861312  0.0223562   3.853 0.000131 ***
		`Age_>15`    -0.0475849  0.0221554  -2.148 0.032177 *  
		`Age_6-15`           NA         NA      NA       NA    
		`Thick_<3`    0.0070633  0.0180698   0.391 0.696032    
		`Thick_>3`           NA         NA      NA       NA    
		Season_Dry    0.0971107  0.0180923   5.368 1.19e-07 ***
		Season_Rainy         NA         NA      NA       NA    
		---
		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		Residual standard error: 0.2104 on 538 degrees of freedom
		Multiple R-squared:  0.135,	Adjusted R-squared:  0.1237 
		F-statistic: 11.99 on 7 and 538 DF,  p-value: 2.961e-14
