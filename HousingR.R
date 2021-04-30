# Clear workspace
rm(list = ls())

# Set working directory
setwd("/Users/juandollaa/Desktop/Spring2021/CSC687/Project1")

##################
## Loading data ##
##################

# install.packages("ISLR")
library(ISLR)
#DataSet Information 

# SalePrice - the property's sale price in dollars. This is the target variable that you're trying to predict.
# MSSubClass: The building class
# MSZoning: The general zoning classification
# LotFrontage: Linear feet of street connected to property
# LotArea: Lot size in square feet
# Street: Type of road access
# Alley: Type of alley access
# LotShape: General shape of property
# LandContour: Flatness of the property
# Utilities: Type of utilities available
# LotConfig: Lot configuration
# LandSlope: Slope of property
# Neighborhood: Physical locations within Ames city limits
# Condition1: Proximity to main road or railroad
# Condition2: Proximity to main road or railroad (if a second is present)
# BldgType: Type of dwelling
# HouseStyle: Style of dwelling
# OverallQual: Overall material and finish quality
# OverallCond: Overall condition rating
# YearBuilt: Original construction date
# YearRemodAdd: Remodel date
# RoofStyle: Type of roof
# RoofMatl: Roof material
# Exterior1st: Exterior covering on house
# Exterior2nd: Exterior covering on house (if more than one material)
# MasVnrType: Masonry veneer type
# MasVnrArea: Masonry veneer area in square feet
# ExterQual: Exterior material quality
# ExterCond: Present condition of the material on the exterior
# Foundation: Type of foundation
# BsmtQual: Height of the basement
# BsmtCond: General condition of the basement
# BsmtExposure: Walkout or garden level basement walls
# BsmtFinType1: Quality of basement finished area
# BsmtFinSF1: Type 1 finished square feet
# BsmtFinType2: Quality of second finished area (if present)
# BsmtFinSF2: Type 2 finished square feet
# BsmtUnfSF: Unfinished square feet of basement area
# TotalBsmtSF: Total square feet of basement area
# Heating: Type of heating
# HeatingQC: Heating quality and condition
# CentralAir: Central air conditioning
# Electrical: Electrical system
# 1stFlrSF: First Floor square feet
# 2ndFlrSF: Second floor square feet
# LowQualFinSF: Low quality finished square feet (all floors)
# GrLivArea: Above grade (ground) living area square feet
# BsmtFullBath: Basement full bathrooms
# BsmtHalfBath: Basement half bathrooms
# FullBath: Full bathrooms above grade
# HalfBath: Half baths above grade
# Bedroom: Number of bedrooms above basement level
# Kitchen: Number of kitchens
# KitchenQual: Kitchen quality
# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
# Functional: Home functionality rating
# Fireplaces: Number of fireplaces
# FireplaceQu: Fireplace quality
# GarageType: Garage location
# GarageYrBlt: Year garage was built
# GarageFinish: Interior finish of the garage
# GarageCars: Size of garage in car capacity
# GarageArea: Size of garage in square feet
# GarageQual: Garage quality
# GarageCond: Garage condition
# PavedDrive: Paved driveway
# WoodDeckSF: Wood deck area in square feet
# OpenPorchSF: Open porch area in square feet
# EnclosedPorch: Enclosed porch area in square feet
# 3SsnPorch: Three season porch area in square feet
# ScreenPorch: Screen porch area in square feet
# PoolArea: Pool area in square feet
# PoolQC: Pool quality
# Fence: Fence quality
# MiscFeature: Miscellaneous feature not covered in other categories
# MiscVal: $Value of miscellaneous feature
# MoSold: Month Sold
# YrSold: Year Sold
# SaleType: Type of sale
# SaleCondition: Condition of sale

# Read in Data

dataset = read.csv("/Users/juandollaa/Desktop/Spring2021/CSC687/Project1/train.csv")


# Remove Missing Rows
colSums((is.na(dataset)))

dataset <- dataset[ , colSums(is.na(dataset)) == 0]
colSums((is.na(dataset)))
sum(is.na(dataset))

#Summary of Data
summary(dataset)

#Clean Data
library("dplyr") 


#MSZoning: Identifies the general zoning classification of the sale.

#A	Agriculture
#C	Commercial
#FV	Floating Village Residential
#I	Industrial
#RH	Residential High Density
#RL	Residential Low Density
#RP	Residential Low Density Park 
#RM	Residential Medium Density


dataset$MSZoning[dataset$MSZoning=='A'] = 0
dataset$MSZoning[dataset$MSZoning=='C (all)'] = 1
dataset$MSZoning[dataset$MSZoning=='FV'] = 2
dataset$MSZoning[dataset$MSZoning=='I'] = 3
dataset$MSZoning[dataset$MSZoning=='RH'] =4
dataset$MSZoning[dataset$MSZoning=='RL'] = 5
dataset$MSZoning[dataset$MSZoning=='RP'] = 6
dataset$MSZoning[dataset$MSZoning=='RM'] =7

dataset$MSZoning = as.numeric(dataset$MSZoning)

datasets <- unclass(dataset)
#Street: Type of road access to property

#Grvl	Gravel	
#Pave	Paved

dataset$Street[dataset$Street=='Grvl'] = 0
dataset$Street[dataset$Street=='Pave'] = 1
dataset$Street = as.numeric(dataset$Street)
datasets <- unclass(dataset)

#LotShape: General shape of property

#Reg	Regular	
#IR1	Slightly irregular
#IR2	Moderately Irregular
#IR3	Irregular

dataset$LotShape[dataset$LotShape=='Reg'] = 0
dataset$LotShape[dataset$LotShape=='IR1'] = 1
dataset$LotShape[dataset$LotShape=='IR2'] = 2
dataset$LotShape[dataset$LotShape=='IR3'] = 3

dataset$LotShape = as.numeric(dataset$LotShape)
datasets <- unclass(dataset)
#LandContour: Flatness of the property

#Lvl	Near Flat/Level	
#Bnk	Banked - Quick and significant rise from street grade to building
#HLS	Hillside - Significant slope from side to side
#Low	Depression

dataset$LandContour[dataset$LandContour=='Lvl'] = 0
dataset$LandContour[dataset$LandContour=='Bnk'] = 1
dataset$LandContour[dataset$LandContour=='HLS'] = 2
dataset$LandContour[dataset$LandContour=='Low'] = 3

dataset$LandContour = as.numeric(dataset$LandContour)
datasets <- unclass(dataset)


#Utilities: Type of utilities available

#AllPub	All public Utilities (E,G,W,& S)	
#NoSewr	Electricity, Gas, and Water (Septic Tank)
#NoSeWa	Electricity and Gas Only
#ELO	Electricity only

dataset$Utilities[dataset$Utilities=='AllPub'] = 0
dataset$Utilities[dataset$Utilities=='NoSewr'] = 1
dataset$Utilities[dataset$Utilities=='NoSeWa'] = 2
dataset$Utilities[dataset$Utilities=='ELO'] = 3

dataset$Utilities = as.numeric(dataset$Utilities)
datasets <- unclass(dataset)

#LotConfig: Lot configuration

#Inside	Inside lot
#Corner	Corner lot
#CulDSac	Cul-de-sac
#FR2	Frontage on 2 sides of property
#FR3	Frontage on 3 sides of property

dataset$LotConfig[dataset$LotConfig=='Inside'] = 0
dataset$LotConfig[dataset$LotConfig=='Corner'] = 1
dataset$LotConfig[dataset$LotConfig=='CulDSac'] = 2
dataset$LotConfig[dataset$LotConfig=='FR2'] = 3
dataset$LotConfig[dataset$LotConfig=='FR3'] = 4

dataset$LotConfig = as.numeric(dataset$LotConfig)
datasets <- unclass(dataset)

#LandSlope: Slope of property

#Gtl	Gentle slope
#Mod	Moderate Slope	
#Sev	Severe Slope


dataset$LandSlope[dataset$LandSlope=='Gtl'] = 0
dataset$LandSlope[dataset$LandSlope=='Mod'] = 1
dataset$LandSlope[dataset$LandSlope=='Sev'] = 2

dataset$LandSlope = as.numeric(dataset$LandSlope)
datasets <- unclass(dataset)


#Neighborhood: Physical locations within Ames city limits

#Blmngtn	Bloomington Heights
#Blueste	Bluestem
#BrDale	Briardale
#BrkSide	Brookside
#ClearCr	Clear Creek
#CollgCr	College Creek
#Crawfor	Crawford
#Edwards	Edwards
#Gilbert	Gilbert
#IDOTRR	Iowa DOT and Rail Road
#MeadowV	Meadow Village
#Mitchel	Mitchell
#Names	North Ames
#NoRidge	Northridge
#NPkVill	Northpark Villa
#NridgHt	Northridge Heights
#NWAmes	Northwest Ames
#OldTown	Old Town
#SWISU	South & West of Iowa State University
#Sawyer	Sawyer
#SawyerW	Sawyer West
#Somerst	Somerset
#StoneBr	Stone Brook
#Timber	Timberland
#Veenker	Veenker


dataset$Neighborhood[dataset$Neighborhood=='Blmngtn'] = 0
dataset$Neighborhood[dataset$Neighborhood=='Blueste'] = 1
dataset$Neighborhood[dataset$Neighborhood=='BrDale'] = 2
dataset$Neighborhood[dataset$Neighborhood=='BrkSide'] = 3
dataset$Neighborhood[dataset$Neighborhood=='Crawfor'] = 4
dataset$Neighborhood[dataset$Neighborhood=='ClearCr'] = 24
dataset$Neighborhood[dataset$Neighborhood=='CollgCr'] = 5
dataset$Neighborhood[dataset$Neighborhood=='Edwards'] = 6
dataset$Neighborhood[dataset$Neighborhood=='Gilbert'] = 7
dataset$Neighborhood[dataset$Neighborhood=='IDOTRR'] = 8
dataset$Neighborhood[dataset$Neighborhood=='MeadowV'] = 9
dataset$Neighborhood[dataset$Neighborhood=='Mitchel'] = 10
dataset$Neighborhood[dataset$Neighborhood=='NAmes'] = 11
dataset$Neighborhood[dataset$Neighborhood=='NoRidge'] = 12
dataset$Neighborhood[dataset$Neighborhood=='NPkVill'] = 13
dataset$Neighborhood[dataset$Neighborhood=='NridgHt'] = 14
dataset$Neighborhood[dataset$Neighborhood=='NWAmes'] = 15
dataset$Neighborhood[dataset$Neighborhood=='OldTown'] = 16
dataset$Neighborhood[dataset$Neighborhood=='SWISU'] = 17
dataset$Neighborhood[dataset$Neighborhood=='Sawyer'] = 18
dataset$Neighborhood[dataset$Neighborhood=='SawyerW'] = 19
dataset$Neighborhood[dataset$Neighborhood=='Somerst'] = 20
dataset$Neighborhood[dataset$Neighborhood=='StoneBr'] = 21
dataset$Neighborhood[dataset$Neighborhood=='Timber'] = 22
dataset$Neighborhood[dataset$Neighborhood=='Veenker'] = 23

dataset$Neighborhood = as.numeric(dataset$Neighborhood)
datasets <- unclass(dataset)
#Condition1: Proximity to various conditions

#Artery	Adjacent to arterial street
#Feedr	Adjacent to feeder street	
#Norm	Normal	
#RRNn	Within 200' of North-South Railroad
#RRAn	Adjacent to North-South Railroad
#PosN	Near positive off-site feature--park, greenbelt, etc.
#PosA	Adjacent to postive off-site feature
#RRNe	Within 200' of East-West Railroad
#RRAe	Adjacent to East-West Railroad

dataset$Condition1[dataset$Condition1=='Artery'] = 0
dataset$Condition1[dataset$Condition1=='Feedr'] = 1
dataset$Condition1[dataset$Condition1=='Norm'] = 2
dataset$Condition1[dataset$Condition1=='RRNn'] = 3
dataset$Condition1[dataset$Condition1=='RRAn'] = 4
dataset$Condition1[dataset$Condition1=='PosN'] = 5
dataset$Condition1[dataset$Condition1=='PosA'] = 6
dataset$Condition1[dataset$Condition1=='RRNe'] = 7
dataset$Condition1[dataset$Condition1=='RRAe'] = 8


dataset$Condition1 = as.numeric(dataset$Condition1)
datasets <- unclass(dataset)

#Condition2: Proximity to various conditions

#Artery	Adjacent to arterial street
#Feedr	Adjacent to feeder street	
#Norm	Normal	
#RRNn	Within 200' of North-South Railroad
#RRAn	Adjacent to North-South Railroad
#PosN	Near positive off-site feature--park, greenbelt, etc.
#PosA	Adjacent to postive off-site feature
#RRNe	Within 200' of East-West Railroad
#RRAe	Adjacent to East-West Railroad

dataset$Condition2[dataset$Condition2=='Artery'] = 0
dataset$Condition2[dataset$Condition2=='Feedr'] = 1
dataset$Condition2[dataset$Condition2=='Norm'] = 2
dataset$Condition2[dataset$Condition2=='RRNn'] = 3
dataset$Condition2[dataset$Condition2=='RRAn'] = 4
dataset$Condition2[dataset$Condition2=='PosN'] = 5
dataset$Condition2[dataset$Condition2=='PosA'] = 6
dataset$Condition2[dataset$Condition2=='RRNe'] = 7
dataset$Condition2[dataset$Condition2=='RRAe'] = 8

dataset$Condition2 = as.numeric(dataset$Condition2)
datasets <- unclass(dataset)


#BldgType: Type of dwelling

#1Fam	Single-family Detached	
#2FmCon	Two-family Conversion; originally built as one-family dwelling
#Duplx	Duplex
#TwnhsE	Townhouse End Unit
#TwnhsI	Townhouse Inside Unit

dataset$BldgType[dataset$BldgType=='1Fam'] = 0
dataset$BldgType[dataset$BldgType=='2fmCon'] = 1
dataset$BldgType[dataset$BldgType=='Duplex'] = 2
dataset$BldgType[dataset$BldgType=='TwnhsE'] = 3
dataset$BldgType[dataset$BldgType=='Twnhs'] = 4

dataset$BldgType = as.numeric(dataset$BldgType)
datasets <- unclass(dataset)

#Exterior2nd: Exterior covering on house (if more than one material)

#AsbShng	Asbestos Shingles
#AsphShn	Asphalt Shingles
#BrkComm	Brick Common
#BrkFace	Brick Face
#CBlock	Cinder Block
#CemntBd	Cement Board
#HdBoard	Hard Board
#ImStucc	Imitation Stucco
#MetalSd	Metal Siding
#Other	Other
#Plywood	Plywood
#PreCast	PreCast
#Stone	Stone
#Stucco	Stucco
#VinylSd	Vinyl Siding
#Wd Sdng	Wood Siding
#WdShing	Wood Shingles

dataset$Exterior2nd[dataset$Exterior2nd=='AsbShng'] = 0
dataset$Exterior2nd[dataset$Exterior2nd=='AsphShn'] = 1
dataset$Exterior2nd[dataset$Exterior2nd=='Brk Cmn'] = 2
dataset$Exterior2nd[dataset$Exterior2nd=='BrkFace'] = 3
dataset$Exterior2nd[dataset$Exterior2nd=='CBlock'] = 4
dataset$Exterior2nd[dataset$Exterior2nd=='CmentBd'] = 5
dataset$Exterior2nd[dataset$Exterior2nd=='HdBoard'] = 6
dataset$Exterior2nd[dataset$Exterior2nd=='ImStucc'] = 7
dataset$Exterior2nd[dataset$Exterior2nd=='MetalSd'] = 8
dataset$Exterior2nd[dataset$Exterior2nd=='Other'] = 9
dataset$Exterior2nd[dataset$Exterior2nd=='Plywood'] = 10
dataset$Exterior2nd[dataset$Exterior2nd=='PreCast'] = 11
dataset$Exterior2nd[dataset$Exterior2nd=='Stone'] = 12
dataset$Exterior2nd[dataset$Exterior2nd=='Stucco'] = 13
dataset$Exterior2nd[dataset$Exterior2nd=='VinylSd'] = 14
dataset$Exterior2nd[dataset$Exterior2nd=='Wd Sdng'] = 15
dataset$Exterior2nd[dataset$Exterior2nd=='Wd Shng'] = 16

dataset$Exterior2nd = as.numeric(dataset$Exterior2nd )
datasets <- unclass(dataset)


#ExterQual: Evaluates the quality of the material on the exterior 

#Ex	Excellent
#Gd	Good
#TA	Average/Typical
#Fa	Fair
#Po	Poor

dataset$ExterQual[dataset$ExterQual=='Ex'] = 0
dataset$ExterQual[dataset$ExterQual=='Gd'] = 1
dataset$ExterQual[dataset$ExterQual=='TA'] = 2
dataset$ExterQual[dataset$ExterQual=='Fa'] = 3
dataset$ExterQual[dataset$ExterQual=='Po'] = 4

dataset$ExterQual = as.numeric(dataset$ExterQual )
datasets <- unclass(dataset)


#ExterCond: Evaluates the present condition of the material on the exterior

#Ex	Excellent
#Gd	Good
#TA	Average/Typical
#Fa	Fair
#Po	Poor

dataset$ExterCond[dataset$ExterCond=='Ex'] = 0
dataset$ExterCond[dataset$ExterCond=='Gd'] = 1
dataset$ExterCond[dataset$ExterCond=='TA'] = 2
dataset$ExterCond[dataset$ExterCond=='Fa'] = 3
dataset$ExterCond[dataset$ExterCond=='Po'] = 4

dataset$ExterCond = as.numeric(dataset$ExterCond )
datasets <- unclass(dataset)

#Foundation: Type of foundation

#BrkTil	Brick & Tile
#CBlock	Cinder Block
#PConc	Poured Contrete	
#Slab	Slab
#Stone	Stone
#Wood	Wood

dataset$Foundation[dataset$Foundation=='BrkTil'] = 0
dataset$Foundation[dataset$Foundation=='CBlock'] = 1
dataset$Foundation[dataset$Foundation=='Slab'] = 2
dataset$Foundation[dataset$Foundation=='Stone'] = 3
dataset$Foundation[dataset$Foundation=='Wood'] = 4
dataset$Foundation[dataset$Foundation=='PConc'] = 5


dataset$Foundation = as.numeric(dataset$Foundation )
datasets <- unclass(dataset)



         
# Heating: Type of heating
         
#Floor	Floor Furnace
#GasA	Gas forced warm air furnace
#GasW	Gas hot water or steam heat
#Grav	Gravity furnace	
#OthW	Hot water or steam heat other than gas
#Wall	Wall furnace

dataset$Heating[dataset$Heating=='Floor'] = 0
dataset$Heating[dataset$Heating=='GasA'] = 1
dataset$Heating[dataset$Heating=='GasW'] = 2
dataset$Heating[dataset$Heating=='Grav'] = 3
dataset$Heating[dataset$Heating=='OthW'] = 4
dataset$Heating[dataset$Heating=='Wall'] = 5

dataset$Heating = as.numeric(dataset$Heating)
datasets <- unclass(dataset)
         
#HeatingQC: Heating quality and condition
         
#Ex	Excellent
#Gd	Good
#TA	Average/Typical
#Fa	Fair
#Po	Poor

dataset$HeatingQC[dataset$HeatingQC=='Ex'] = 0
dataset$HeatingQC[dataset$HeatingQC=='Gd'] = 1
dataset$HeatingQC[dataset$HeatingQC=='TA'] = 2
dataset$HeatingQC[dataset$HeatingQC=='Fa'] = 3
dataset$HeatingQC[dataset$HeatingQC=='Po'] = 4
         
dataset$HeatingQC = as.numeric(dataset$HeatingQC)
datasets <- unclass(dataset)
         
# KitchenQual: Kitchen quality
         
# Ex	Excellent
# Gd	Good
#TA	Typical/Average
#Fa	Fair
#Po	Poor
         
dataset$KitchenQual[dataset$KitchenQual=='Ex'] = 0
dataset$KitchenQual[dataset$KitchenQual=='Gd'] = 1
dataset$KitchenQual[dataset$KitchenQual=='TA'] = 2
dataset$KitchenQual[dataset$KitchenQual=='Fa'] = 3
dataset$KitchenQual[dataset$KitchenQual=='Po'] = 4

dataset$KitchenQual = as.numeric(dataset$KitchenQual)
datasets <- unclass(dataset)

#CentralAir: Central air conditioning

#N	No
#Y	Yes

dataset$CentralAir[dataset$CentralAir=='N'] = 0
dataset$CentralAir[dataset$CentralAir=='Y'] = 1

dataset$CentralAir = as.numeric(dataset$CentralAir)
datasets <- unclass(dataset)
         
#Functional: Home functionality (Assume typical unless deductions are warranted)
         
#Typ	Typical Functionality
#Min1	Minor Deductions 1
#Min2	Minor Deductions 2
#Mod	Moderate Deductions
# Maj1	Major Deductions 1
#Maj2	Major Deductions 2
#Sev	Severely Damaged
#Sal	Salvage only
         
dataset$Functional[dataset$Functional=='Typ'] = 0
dataset$Functional[dataset$Functional=='Min1'] = 1
dataset$Functional[dataset$Functional=='Min2'] = 2
dataset$Functional[dataset$Functional=='Mod'] = 3
dataset$Functional[dataset$Functional=='Maj1'] = 4
dataset$Functional[dataset$Functional=='Maj2'] = 5
dataset$Functional[dataset$Functional=='Sev'] = 6
dataset$Functional[dataset$Functional=='Sal'] = 7

dataset$Functional = as.numeric(dataset$Functional)
datasets <- unclass(dataset)
         

         
#PavedDrive: Paved driveway
#Y	Paved 
#P	Partial Pavement
#N	Dirt/Gravel
         
dataset$PavedDrive[dataset$PavedDrive=='Y'] = 0
dataset$PavedDrive[dataset$PavedDrive=='P'] = 1
dataset$PavedDrive[dataset$PavedDrive=='N'] = 2
         

dataset$PavedDrive = as.numeric(dataset$PavedDrive)
datasets <- unclass(dataset)

#SaleType: Type of sale
         
#WD 	Warranty Deed - Conventional
#CWD	Warranty Deed - Cash
#VWD	Warranty Deed - VA Loan
#New	Home just constructed and sold
#COD	Court Officer Deed/Estate
#Con	Contract 15% Down payment regular terms
#ConLw	Contract Low Down payment and low interest
#ConLI	Contract Low Interest
#ConLD	Contract Low Down
#Oth	Other

dataset$SaleType[dataset$SaleType=='WD'] = 0
dataset$SaleType[dataset$SaleType=='CWD'] = 1
dataset$SaleType[dataset$SaleType=='VWD'] = 2
dataset$SaleType[dataset$SaleType=='New'] = 3
dataset$SaleType[dataset$SaleType=='COD'] = 4
dataset$SaleType[dataset$SaleType=='Con'] = 5
dataset$SaleType[dataset$SaleType=='ConLw'] = 6
dataset$SaleType[dataset$SaleType=='ConLI'] = 7
dataset$SaleType[dataset$SaleType=='ConLD'] = 8
dataset$SaleType[dataset$SaleType=='Oth'] = 9

dataset$SaleType = as.numeric(dataset$SaleType)
datasets <- unclass(dataset)

#SaleCondition: Condition of sale
         
#Normal	Normal Sale
#Abnorml	Abnormal Sale -  trade, foreclosure, short sale
#AdjLand	Adjoining Land Purchase
#Alloca	Allocation - two linked properties with separate deeds, typically condo with a garage unit	
#Family	Sale between family members
#Partial	Home was not completed when last assessed (associated with New Homes)

dataset$SaleCondition[dataset$SaleCondition=='Normal'] = 0
dataset$SaleCondition[dataset$SaleCondition=='Abnorml'] = 1
dataset$SaleCondition[dataset$SaleCondition=='AdjLand'] = 2
dataset$SaleCondition[dataset$SaleCondition=='Alloca'] = 3
dataset$SaleCondition[dataset$SaleCondition=='Family'] = 4
dataset$SaleCondition[dataset$SaleCondition=='Partial'] = 5

dataset$SaleCondition = as.numeric(dataset$SaleCondition)
datasets <- unclass(dataset)


#HouseStyle: Style of dwelling

#1Story	One story
#1.5Fin	One and one-half story: 2nd level finished
#1.5Unf	One and one-half story: 2nd level unfinished
#2Story	Two story
#2.5Fin	Two and one-half story: 2nd level finished
#2.5Unf	Two and one-half story: 2nd level unfinished
#SFoyer	Split Foyer
#SLvl	Split Level

dataset$HouseStyle[dataset$HouseStyle=='1Story'] = 0
dataset$HouseStyle[dataset$HouseStyle=='1.5Story'] = 1
dataset$HouseStyle[dataset$HouseStyle=='1.5Fin'] = 8
dataset$HouseStyle[dataset$HouseStyle=='1.5Unf'] = 2
dataset$HouseStyle[dataset$HouseStyle=='2Story'] = 3
dataset$HouseStyle[dataset$HouseStyle=='2.5Fin'] = 4
dataset$HouseStyle[dataset$HouseStyle=='2.5Unf'] = 5
dataset$HouseStyle[dataset$HouseStyle=='SFoyer'] = 6
dataset$HouseStyle[dataset$HouseStyle=='SLvl'] = 7

dataset$HouseStyle = as.numeric(dataset$HouseStyle)
datasets <- unclass(dataset)

#RoofStyle: Type of roof

#Flat	Flat
#Gable	Gable
#Gambrel	Gabrel (Barn)
#Hip	Hip
#Mansard	Mansard
#Shed	Shed

dataset$RoofStyle[dataset$RoofStyle=='Flat'] = 0
dataset$RoofStyle[dataset$RoofStyle=='Gable'] = 1
dataset$RoofStyle[dataset$RoofStyle=='Gambrel'] = 2
dataset$RoofStyle[dataset$RoofStyle=='Hip'] = 3
dataset$RoofStyle[dataset$RoofStyle=='Mansard'] = 4
dataset$RoofStyle[dataset$RoofStyle=='Shed'] = 5

dataset$RoofStyle = as.numeric(dataset$RoofStyle)
datasets <- unclass(dataset)

#RoofMatl: Roof material

#ClyTile	Clay or Tile
#CompShg	Standard (Composite) Shingle
#Membran	Membrane
#Metal	Metal
#Roll	Roll
#Tar&Grv	Gravel & Tar
#WdShake	Wood Shakes
#WdShngl	Wood Shingles

dataset$RoofMatl[dataset$RoofMatl=='ClyTile'] = 0
dataset$RoofMatl[dataset$RoofMatl=='CompShg'] = 1
dataset$RoofMatl[dataset$RoofMatl=='Membran'] = 2
dataset$RoofMatl[dataset$RoofMatl=='Metal'] = 3
dataset$RoofMatl[dataset$RoofMatl=='Roll'] = 4
dataset$RoofMatl[dataset$RoofMatl=='Tar&Grv'] = 5
dataset$RoofMatl[dataset$RoofMatl=='WdShake'] = 6
dataset$RoofMatl[dataset$RoofMatl=='WdShngl'] = 7

dataset$RoofMatl = as.numeric(dataset$RoofMatl)
datasets <- unclass(dataset)

#Exterior1st: Exterior covering on house

#AsbShng	Asbestos Shingles
#AsphShn	Asphalt Shingles
#BrkComm	Brick Common
#BrkFace	Brick Face
#CBlock	Cinder Block
#CemntBd	Cement Board
#HdBoard	Hard Board
#ImStucc	Imitation Stucco
#MetalSd	Metal Siding
#Other	Other
#Plywood	Plywood
#PreCast	PreCast	
#Stone	Stone
#Stucco	Stucco
#VinylSd	Vinyl Siding
#Wd Sdng	Wood Siding
#WdShing	Wood Shingles

dataset$Exterior1st[dataset$Exterior1st=='AsbShng'] = 0
dataset$Exterior1st[dataset$Exterior1st=='AsphShn'] = 1
dataset$Exterior1st[dataset$Exterior1st=='BrkComm'] = 2
dataset$Exterior1st[dataset$Exterior1st=='BrkFace'] = 3
dataset$Exterior1st[dataset$Exterior1st=='CBlock'] = 4
dataset$Exterior1st[dataset$Exterior1st=='CemntBd'] = 5
dataset$Exterior1st[dataset$Exterior1st=='HdBoard'] = 6
dataset$Exterior1st[dataset$Exterior1st=='ImStucc'] = 7
dataset$Exterior1st[dataset$Exterior1st=='MetalSd'] = 8
dataset$Exterior1st[dataset$Exterior1st=='Other'] = 9
dataset$Exterior1st[dataset$Exterior1st=='Plywood'] = 10
dataset$Exterior1st[dataset$Exterior1st=='PreCast'] = 11
dataset$Exterior1st[dataset$Exterior1st=='Stone'] = 12
dataset$Exterior1st[dataset$Exterior1st=='Stucco'] = 13
dataset$Exterior1st[dataset$Exterior1st=='VinylSd'] = 14
dataset$Exterior1st[dataset$Exterior1st=='Wd Sdng'] = 15
dataset$Exterior1st[dataset$Exterior1st=='WdShing'] = 16

dataset$Exterior1st = as.numeric(dataset$Exterior1st)
datasets <- unclass(dataset)



#Standardize SalePrice

xbar = mean(dataset$SalePrice)
xbar

sde = sd(dataset$SalePrice)
sde

dataset$SalePrice = (dataset$SalePrice-xbar)/sde

#

sum(is.na(dataset))
colSums(is.na(dataset))
dataset <- na.omit(dataset)
sum(is.na(dataset))

nums <- unlist(lapply(dataset, is.numeric))  
dataset = dataset[,nums]
dataset = dataset[c("SalePrice",           "MSSubClass" ,   "MSZoning" ,     "LotArea"   ,    "Street"    ,    "LotShape", 
                    "LandContour"  , "Utilities" ,    "LotConfig"  ,   "LandSlope"  ,   "Neighborhood"  ,"Condition1"   ,
                    "Condition2" ,   "BldgType"   ,   "HouseStyle" ,   "OverallQual"   ,"OverallCond" ,  "YearBuilt"   , 
                    "YearRemodAdd",  "RoofStyle"  ,   "RoofMatl"   ,   "Exterior1st" ,  "Exterior2nd"  , "ExterQual" ,   
                    "ExterCond",     "Foundation"   , "BsmtFinSF1"  ,  "BsmtFinSF2"  ,  "BsmtUnfSF" ,    "TotalBsmtSF"  ,
                    "Heating"  ,     "HeatingQC"   ,  "CentralAir" ,   "X1stFlrSF"  ,   "X2ndFlrSF" ,    "LowQualFinSF" ,
                    "GrLivArea" ,    "BsmtFullBath"  ,"BsmtHalfBath" , "FullBath"  ,    "HalfBath"  ,    "BedroomAbvGr" ,
                    "KitchenAbvGr" , "KitchenQual"  , "TotRmsAbvGrd" , "Functional" ,   "Fireplaces"  ,  "GarageCars" ,  
                    "GarageArea"    ,"PavedDrive"   , "WoodDeckSF"  ,  "OpenPorchSF"  , "EnclosedPorch", "X3SsnPorch"   ,
                    "ScreenPorch"  , "PoolArea"  ,    "MiscVal"   ,    "MoSold"  ,      "YrSold"  ,      "SaleType"   ,  
                    "SaleCondition" , "Id" )]
#Looking at Correlations
library(corrplot)

correlations <- cor(dataset,use = "everything")
corrplot(correlations, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')

correlations <- cor(dataset[0:15],use = "everything")
corrplot(correlations, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')

correlations <- cor(dataset[16:30],use = "everything")
corrplot(correlations, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')

correlations <- cor(dataset[30:45],use = "everything")
corrplot(correlations, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')

correlations <- cor(dataset[45:62],use = "everything")
corrplot(correlations, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')

correlationsSalePrice <-cor(dataset[-1], dataset$SalePrice) 
corrplot(correlationsSalePrice, method = 'circle', type = 'lower', sig.level = 0.01, insig = 'blank')


correlationsSalePrice

library(lares)

corr_cross(dataset, # name of dataset
         # display only significant correlations (at 5% level)
           top = 20 # display top 10 couples of variables (by correlation coefficient)
)

corr_var(dataset, # name of dataset
         SalePrice, # name of variable to focus on
         top = 20 # display top 5 correlations
) 


# Some ScatterPlots

pairs(SalePrice~YearBuilt+OverallQual+TotalBsmtSF + GrLivArea+OverallQual, data = dataset, main = "Some ScatterPlot Examples")






#Important Scatterplots with a lot of relation
attach(dataset)



scatter.smooth(YearBuilt,SalePrice,main = 'Linear Relation',lpars = list(col='red',lwd = 3))

scatter.smooth(TotalBsmtSF,SalePrice,main = 'Linear Relation',lpars = list(col='red',lwd = 3))

scatter.smooth(GrLivArea,SalePrice,main = 'Linear Relation',lpars = list(col='red',lwd = 3))


scatter.smooth(OverallQual,SalePrice,main = 'Linear Relation',lpars = list(col='red',lwd = 3))




#Splitting dataset in training and testing using validation set 70/30
dataset <- subset(dataset,select=-c(TotalBsmtSF,GrLivArea,Utilities))
numSamples = dim(dataset)[1]
train = sample(numSamples,numSamples*0.7)
training_set = dataset[train,]
testing_set = dataset[-train,]



#Using all Input Variables Build a Linear Regression Model

library(caret)

lm.fit = lm(SalePrice~.,data=dataset,subset = train)
summary(lm.fit)

#Let's calculate Train and Test MSE

#Train
mean((lm.fit$residuals^2))

#Test
length(lm.fit$coefficients) > lm.fit$rank
lm.fit$coefficients
prediction <- predict(lm.fit, testing_set, type='response')
mean((testing_set$SalePrice-prediction)^2)



#Using Only Significant Variables

summary(lm.fit)
sm.fit = lm(SalePrice~Condition2+OverallQual+OverallCond+YearBuilt+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr,data=dataset,subset=train)
summary(sm.fit)

#Train
mean((sm.fit$residuals^2))
#Test
prediction <- predict(sm.fit, testing_set, type='response')
mean((testing_set$SalePrice-prediction)^2)


#Polynomial transformation Predictors
pm.fit = lm(SalePrice~OverallQual+OverallCond+YearBuilt+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+I(OverallQual^2)+I(YearBuilt^2)+I(BsmtUnfSF^2)+I(BsmtFinSF1^2)++I(X1stFlrSF^2)+I(X2ndFlrSF^2)+I(BedroomAbvGr^2), data=dataset, subset=train)
summary(pm.fit)

pm.fit = lm(SalePrice~OverallQual+OverallCond+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+BedroomAbvGr+I(OverallQual^2)+I(BsmtFinSF1^2)++I(X1stFlrSF^2), data=dataset, subset=train)
summary(pm.fit)

pm.fit = lm(SalePrice~OverallQual+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+I(OverallQual^2)+I(BsmtFinSF1^2)+I(X1stFlrSF^2), data=dataset, subset=train)
summary(pm.fit)

pm.fit = lm(SalePrice~OverallQual+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+I(OverallQual^2)+I(X1stFlrSF^2), data=dataset, subset=train)
summary(pm.fit)

#Train
mean((pm.fit$residuals^2))

#Test
prediction <- predict(pm.fit, testing_set, type='response')
mean((testing_set$SalePrice-prediction)^2)

#Diagnostic Plots
par(mfrow=c(2,2))
plot(lm.fit)
plot(sm.fit)
plot(pm.fit)

# LOOCV
sum(is.na(dataset))
library(boot)


train_Control = trainControl(method = "LOOCV")
model <- train(SalePrice ~.,data = training_set, method = 'lm',trControl = train_Control)
print(model)


#10 Fold

set.seed(123)

train_Control = trainControl(method = "repeatedcv", number =10,repeats = 5)

# All Attributes Linear
model <- train(SalePrice ~.,data = training_set, method = 'lm',trControl = train_Control)
print(model)
model$bestTune
#Only Significant 
model <- train(SalePrice~Condition2+OverallQual+OverallCond+YearBuilt+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr,data = training_set, method = 'lm',trControl = train_Control)
print(model)
model$bestTune
#Signifcant and Polynomial
model <- train(SalePrice~OverallQual+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+I(OverallQual^2)+I(X1stFlrSF^2),data = training_set, method = 'lm',trControl = train_Control)
print(model)

model$bestTune
# Building Regression Model Using Bagging

library(randomForest)
library(adabag)
library(gbm)

#All Variables
baggingModel= train(SalePrice~.,data=training_set,method="treebag", trControl=train_Control)

print(baggingModel)
baggingModel$bestTune
#Significant
baggingModel= train(SalePrice~Condition2+OverallQual+OverallCond+YearBuilt+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr,data=training_set,method="treebag", trControl=train_Control)

print(baggingModel)
baggingModel$bestTune
#Significant and Polynomial

baggingModel= train(SalePrice~OverallQual+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+I(OverallQual^2)+I(X1stFlrSF^2),data=training_set,method="treebag", trControl=train_Control)

print(baggingModel)

baggingModel$bestTune

#Random Forest Cross Validation
?train()

#All Variables
RFModel = train(SalePrice ~ . , data=training_set, method = "rf", trControl = train_Control) 
print(RFModel)
RFModel$bestTune
#Significant
RFModel = train(SalePrice ~ Condition2+OverallQual+OverallCond+YearBuilt+BsmtFinSF1+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+BedroomAbvGr, data=training_set, method = "rf", trControl = train_Control) 
print(RFModel)
RFModel$bestTune
#Significant and Polynomial

RFModel = train(SalePrice~OverallQual+BsmtFinSF1+X1stFlrSF+X2ndFlrSF+I(OverallQual^2)+I(X1stFlrSF^2), data=training_set, method = "rf", trControl = train_Control) 
print(RFModel)

RFModel$bestTune


predictions <- RFModel %>% predict(testing_set)
head(predictions)
RMSE(predictions, testing_set$SalePrice)

#

#ExtremeGradientBoosting
boostingModel=gbm(SalePrice~.,
                  data=training_set,distribution="gaussian",n.trees=5000,
                  interaction.depth=3, cv.folds=10)
boostingModel
summary(boostingModel)

predictions <- boostingModel %>% predict(testing_set)
head(predictions)
RMSE(predictions, testing_set$SalePrice)


#RESULTS: 
RFModel = train(SalePrice ~ . , data=training_set, method = "rf", trControl = train_Control) 
print(RFModel)
RFModel$bestTune
predictions <- RFModel %>% predict(testing_set)
head(predictions)
RMSE(predictions, testing_set$SalePrice)
