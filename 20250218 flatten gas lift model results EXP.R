#this takes the model result and prepares it for a BI report

rm(list=ls())
setwd("C:\\Users\\#redacted\\no sync files")

models<- read.csv('PressureModels.stdf', header = TRUE, sep = ";", skip=6, quote = "")
models <- models[-c(1),]

LastFlLvl<- read.csv('WellTest_LastFluidLevel.stdf', header = TRUE, sep = ";", skip=6, quote = "")
LastFlLvl <- LastFlLvl[-c(1),]

LastDaily <- read.csv('#redacted#redacted.DailyAllocated_Last.stdf', header = TRUE, sep = ";", skip=6, quote = "")
LastDaily <- LastDaily[-c(1),]
LastDaily[,2:4] <- sapply(LastDaily[,2:4], as.numeric)

LastwTst<- read.csv('WellTest_LastTestRates.stdf', header = TRUE, sep = ";", skip=6, quote = "")
LastwTst <- LastwTst[-c(1),]
LastwTst[,2:4] <- sapply(LastwTst[,2:4], as.numeric)

library(jsonlite)

# set up empty data frames to return if there's an error or wrong number of records selected


messages <- data.frame(Error = "No errors.", Warning = "No warnings.")
model.deviation <- data.frame(MeasuredDepth = as.numeric(), TVD = as.numeric(), Inclination = as.numeric(), Elevation = as.numeric(), 
                              TrueAzimuth = as.numeric())
model.inflow <- data.frame(OilRate = as.numeric(), LiquidRate = as.numeric(), PerforationPressure = as.numeric())
model.verticalLiftPerformance <- data.frame(OilRate = as.numeric(), LiquidRate = as.numeric(), PerforationPressure = as.numeric())
model.valvePerf_ls <- data.frame( ProductionPressure = as.numeric(), FlowRate = as.numeric())
model.valvePerf <- data.frame( press = as.numeric(), rate = as.numeric(), number = as.numeric(), source = as.character())
model.PressureTraverse <- data.frame(  TrueVerticalDepth = as.numeric(), Depth = as.numeric(), TubingPressure = as.numeric(), 
                                       CasingPressure = as.numeric(), Temperature = as.numeric(), PressureGradient = as.numeric())
model.PerformanceCurve <- data.frame(  data.frame(LiftGasRate = as.numeric(), LiquidRate = as.numeric(), size = as.numeric(), 
                                                  type = as.character(), StartedOn = as.character()))
model.Valves <- data.frame(  ValveMfgr = as.character(), ValveSeries = as.character(), ValveTypeCode = as.character(), 
                             MeasuredDepth = as.numeric(), TrueVerticalDepth = as.numeric(), OpeningPressure = as.numeric(),
                             ClosingPressure = as.numeric(), InjectionPressureAtDepth = as.numeric()
                             , ProductionPressureAtDepth = as.numeric(),  Stat = as.character(),
                             TemperatureAtDepth = as.numeric(), FlowRate = as.numeric(), PortSize = as.numeric(),
                             Ptro = as.numeric(), Status = as.numeric(), Correlation = as.character(),
                             RValue = as.numeric(), Comments = as.character(), PortSize = as.numeric())

model.ipr_vlp_plot <- data.frame(  OilRate = as.numeric(), LiquidRate = as.numeric(), type = as.character()
                                   , DownholePressure = as.numeric(), size = as.numeric())

model.modelInfo <- data.frame(Sum_Date = as.character(), WellTestDate = as.character(), OilRate = as.numeric()
                              , ReservoirGasRate = as.numeric(), WaterRate = as.numeric(), LiftGasRate = as.numeric()
                              , DownholePressure = as.numeric(), TubingPressure = as.numeric()
                              , TubingTemperature = as.numeric(), CasingPressure = as.numeric()
                              , CasingTemperature = as.numeric(), IsLatestWellTest = as.numeric()
                              , GLR = as.numeric(), G_RLR = as.numeric(), GLR_total = as.numeric()
                              , GLR_injected = as.numeric(), GOR = as.numeric(), WOR = as.numeric()
                              , GOR_total = as.numeric(), GasRate = as.numeric(), WaterCut = as.numeric()
                              ,GasUtil = as.numeric(), GasLiftMethod = as.character(), FlowType = as.character()
                              , DepthOfInjection = as.numeric(), GasInjectionMethod = as.character()
                              , CorrelationName = as.character(), DeltaLength = as.numeric()
                              , DeltaLengthCloseToValve = as.numeric(), CalculateReservoirPressure = as.character()
                              , CalculateWaterDensityFromSalinity = as.character(), AutoCalibrate = as.character()
                              , calib_parameter_frictional_pressure_drop = as.numeric()
                              , calib_parameter_hydrostatic_pressure_drop = as.numeric()
                              , ReservoirTemperature = as.numeric(), ReservoirPressure = as.numeric()
                              , GaugeMD = as.numeric(), PerforationTopMD = as.numeric(), PerforationBottomMD = as.numeric()
                              , PerforationTopMDActual = as.numeric(), CasingDiameterInside = as.numeric()
                              , CasingDiameterOutside = as.numeric(), TubingDiameterInside = as.numeric()
                              , TubingDiameterOutside = as.numeric(), EndOfTubingMD = as.numeric()
                              , TubingRoughness = as.numeric(), CasingRoughness = as.numeric(), SG_g = as.numeric()
                              , SG_w = as.numeric(), SG_lift_gas = as.numeric(), API = as.numeric(), sigma_o = as.numeric()
                              , sigma_w = as.numeric(), mu_g = as.numeric(), NaCL_percent = as.numeric(), B_w = as.numeric()
                              , nitrogen_mole_percent = as.numeric(), H2S_mole_percent = as.numeric()
                              , CO2_mole_percent = as.numeric(), water_salinity = as.numeric(),orifice_depth = as.numeric()
                              , LiftGasFlowRate = as.numeric(), MeasuredGaugePressure = as.numeric()
                              , FluidLevel = as.numeric(), FluidLevelDate = as.character())


API <- models$AssetID
errors.str <- "Error parsing model result."
warnings.str <- ""

models[models == '\\?'] <- NA

model.deviation <- tryCatch(
  expr = {
    model.deviation <- as.data.frame(fromJSON(models$Deviation, flatten=TRUE))
    return = model.deviation
  },
  error = function(e){
    return(data.frame(MeasuredDepth = as.numeric(), TVD = as.numeric(), Inclination = as.numeric(), Elevation = as.numeric(), 
                      TrueAzimuth = as.numeric()))
  })


model.pvt <- fromJSON(models$PvtWellboreData, flatten=TRUE)
model.pvt[sapply(model.pvt,is.null)] <- NA
model.pvt <- as.data.frame(model.pvt)

model.conditions <- fromJSON(models$OperatingConditions, flatten=TRUE)
model.conditions[sapply(model.conditions,is.null)] <- NA
model.conditions <- as.data.frame(model.conditions)

model.PressureTraverse <- tryCatch(
  expr = {
    model.PressureTraverse <- as.data.frame(fromJSON(models$PressureTraverse, flatten=TRUE))
    return = model.PressureTraverse
  },
  error = function(e){
    return(data.frame(  TrueVerticalDepth = as.numeric(), Depth = as.numeric(), TubingPressure = as.numeric(), 
                        CasingPressure = as.numeric(), Temperature = as.numeric(), PressureGradient = as.numeric()))
  })      

model.wellbore <- fromJSON(models$PvtWellboreData, flatten=TRUE)
model.wellbore[sapply(model.wellbore,is.null)] <- NA
model.wellbore <- as.data.frame(model.wellbore)

model.valvePerf <- data.frame( press = as.numeric(), rate = as.numeric(), number = as.numeric())      
model.valvePerf_ls <- data.frame( press = as.numeric(), rate = as.numeric() )      

model.Valves <- tryCatch(
  expr = {
    model.Valves <- as.data.frame(fromJSON(models$Valves, flatten=TRUE))
    model.Valves$Stat <- "Closed"
    model.Valves$Stat[model.Valves$Status==1] <- "Open"
    model.Valves$Stat[model.Valves$Status==2] <- "Back check"
    model.Valves$number <- seq.int(nrow(model.Valves))  
    return = model.Valves
  }
  
  ,
  error = function(e){
    return(data.frame(  ValveMfgr = as.character(), ValveSeries = as.character(), ValveTypeCode = as.character(), 
                        MeasuredDepth = as.numeric(), TrueVerticalDepth = as.numeric(), OpeningPressure = as.numeric(),
                        ClosingPressure = as.numeric(), InjectionPressureAtDepth = as.numeric()
                        , ProductionPressureAtDepth = as.numeric(),
                        TemperatureAtDepth = as.numeric(), FlowRate = as.numeric(), PortSize = as.numeric(),
                        Ptro = as.numeric(), Status = as.numeric(), Correlation = as.character(), Stat = as.character(),
                        RValue = as.numeric(), Comments = as.character(), PortSize = as.numeric()))
  })   

model.valvePerf <- tryCatch(
  expr = {
    model.valvePerf_ls <- as.data.frame(fromJSON(models$ValvePerformance, flatten=TRUE))
    
    for (x in 1:nrow(model.valvePerf_ls)) {
      
      if(lengths(model.valvePerf_ls[x,1]) < 5){dfloop <- data.frame(0,0,x)
      colnames(dfloop) <- c("press", "rate", "number")
      model.valvePerf <- rbind(model.valvePerf,dfloop)}
      else{
        dfloop <- data.frame(unlist(model.valvePerf_ls[x,1]),unlist(model.valvePerf_ls[x,2]),x)
        colnames(dfloop) <- c("press", "rate", "number")
        model.valvePerf <- rbind(model.valvePerf,dfloop)}
    }
    model.valvePerf$source <- "PerfCurve"
    model.valveRate <- subset(model.Valves, select = c(ProductionPressureAtDepth, FlowRate, number))
    model.valveRate$source <- "ValveRate"
    colnames(model.valveRate)[colnames(model.valveRate) == 'ProductionPressureAtDepth'] = 'press'
    colnames(model.valveRate)[colnames(model.valveRate) == 'FlowRate'] = 'rate'
    model.valvePerf <- rbind(model.valvePerf,model.valveRate)
    return = model.valvePerf
  },
  error = function(e){
    return(data.frame(  press = as.numeric(), rate = as.numeric(), number = as.numeric(),source = as.character()))
  })    

model.PressureTraverse <- tryCatch(
  expr = {
    model.valvesTravClose <- model.Valves
    model.valvesTravClose$shape <- paste("Left",model.valvesTravClose$Stat,sep = ' ')
    model.valvesTravClose$pressure <- model.valvesTravClose$ClosingPressure
    model.valvesTravOpen <- model.Valves
    model.valvesTravOpen$shape <- paste("Right",model.valvesTravOpen$Stat,sep = ' ')
    model.valvesTravOpen$pressure <- model.valvesTravOpen$OpeningPressure
    model.valvesTrav <- rbind(model.valvesTravClose,model.valvesTravOpen)
    model.valvesTrav <- subset(model.valvesTrav, select = c(TrueVerticalDepth,pressure,shape))
    model.BHP <- data.frame(TrueVerticalDepth = models$GaugeTVD,pressure = models$MeasuredGaugePressure, shape = 'BHP')
    model.valvesTrav <- rbind (model.valvesTrav, model.BHP)
    model.valvesTrav$Depth <- NA
    model.valvesTrav$TubingPressure <- NA
    model.valvesTrav$CasingPressure <- NA
    model.valvesTrav$Temperature <- NA
    model.valvesTrav$PressureGradient <- NA
    model.PressureTraverse$pressure <- NA
    model.PressureTraverse$shape <- NA
    model.PressureTraverse <- rbind(model.PressureTraverse,model.valvesTrav)
    return = model.PressureTraverse
  },
  error = function(e){
    return(data.frame(  TrueVerticalDepth = as.numeric(), Depth = as.numeric(), TubingPressure = as.numeric(), 
                        CasingPressure = as.numeric(), Temperature = as.numeric(), PressureGradient = as.numeric()))
  })  


model.PerformanceCurve <- tryCatch(
  expr = {
    model.PerformanceCurve <- as.data.frame(fromJSON(models$PerformanceCurve, flatten=TRUE))
    colnames(model.PerformanceCurve)[colnames(model.PerformanceCurve) == 'FlowRate'] = 'LiftGasRate'
    model.PerformanceCurve <- subset(model.PerformanceCurve, select = c(LiftGasRate,LiquidRate))
    model.PerformanceCurve$size <- 1
    model.PerformanceCurve$type <- "InjPerf"
    model.PerformanceCurve$StartedOn <- NA
    LastwTst <- subset(LastwTst[LastwTst$ApiNo14==API,])
    LastDaily <- subset(LastDaily[LastDaily$Api_No14==API,])
    if(nrow(LastwTst) != 0){
      colnames(LastwTst)[colnames(LastwTst) == 'Injection24HourGasVolume'] = 'LiftGasRate'
      LastwTst$LiquidRate <- LastwTst$Gross24HourOilVolume + LastwTst$Gross24HourWaterVolume
      LastwTst$type <- rownames(LastwTst)
      LastwTst <- subset(LastwTst, select = c (LiftGasRate,LiquidRate,type,StartedOn))
      LastwTst$size <- 4
      model.PerformanceCurve <- rbind (LastwTst,model.PerformanceCurve)
    }
    
    if(nrow(LastwTst) == 0 & nrow(LastDaily) != 0){
      colnames(LastDaily)[colnames(LastDaily) == 'Gas_Lift'] = 'LiftGasRate'
      colnames(LastDaily)[colnames(LastDaily) == 'Total_Oil_Prod'] = 'Gross24HourOilVolume'
      colnames(LastDaily)[colnames(LastDaily) == 'Water_Prod'] = 'Gross24HourWaterVolume'
      colnames(LastDaily)[colnames(LastDaily) == 'Prod_Inj_Date'] = 'StartedOn'
      LastDaily$LiquidRate <- LastDaily$Gross24HourOilVolume + LastDaily$Gross24HourWaterVolume
      LastDaily$type <- rownames(LastDaily)
      LastDaily <- subset(LastDaily, select = c (LiftGasRate,LiquidRate,type,StartedOn))
      LastDaily$size <- 4
      model.PerformanceCurve <- rbind (LastDaily,model.PerformanceCurve)
    }
    
    return = model.PerformanceCurve
  },
  error = function(e){
    return(data.frame(LiftGasRate = as.numeric(), LiquidRate = as.numeric(), size = as.numeric(), 
                      type = as.character(), StartedOn = as.Date(character())))
  })

model.ipr_vlp_plot <- tryCatch(
  expr = {
    model.inflow <- as.data.frame(fromJSON(models$InflowPerformanceRelationship, flatten=TRUE))
    model.inflow$type <- "IPR"
    model.verticalLiftPerformance <- as.data.frame(fromJSON(models$VerticalLiftPerformance, flatten=TRUE))
    model.verticalLiftPerformance$type <- "VLP"
    model.ipr_vlp_plot <- rbind(model.inflow,model.verticalLiftPerformance)
    model.ipr_vlp_plot$DownholePressure <- model.ipr_vlp_plot$PerforationPressure
    model.ipr_vlp_plot <- subset(model.ipr_vlp_plot, select = -c(PerforationPressure))
    model.ipr_vlp_plot$size <- 4      
    return = model.ipr_vlp_plot
  },
  error = function(e){
    return(data.frame(OilRate = as.numeric(), LiquidRate = as.numeric(), type = as.character()
                      , DownholePressure = as.numeric(), size = as.numeric()))
  })

model.modelInfo <- tryCatch(
  expr = {
    model.modelInfo1 <- fromJSON(models$WellTestData, flatten=TRUE)
    model.modelInfo1[sapply(model.modelInfo1,is.null)] <- NA
    model.modelInfo1 <- as.data.frame(model.modelInfo1)
    model.modelInfo1$WellTestDate <- gsub('Z','',model.modelInfo1$WellTestDate)
    model.modelInfo1$Sum_Date <- gsub('Z','',model.modelInfo1$Sum_Date)
    model.modelInfo1$WaterCut = model.modelInfo1$WaterRate / (model.modelInfo1$WaterRate + model.modelInfo1$OilRate)
    model.modelInfo1$GasUtil = model.modelInfo1$OilRate / model.modelInfo1$LiftGasRate
    model.modelInfo2 <- fromJSON(models$ModelSettings, flatten=TRUE)
    model.modelInfo2[sapply(model.modelInfo2,is.null)] <- NA
    model.modelInfo2 <- as.data.frame(model.modelInfo2)
    model.modelInfo3 <- fromJSON(models$PvtWellboreData, flatten=TRUE)
    model.modelInfo3[sapply(model.modelInfo3,is.null)] <- NA
    model.modelInfo3 <- as.data.frame(model.modelInfo3)
    model.modelInfo4 <- subset(model.Valves[model.Valves$ValveTypeCode=='Orifice',], select = c(TrueVerticalDepth))
    colnames(model.modelInfo4)[colnames(model.modelInfo4) == 'TrueVerticalDepth'] = 'orifice_depth'
    if(nrow(model.modelInfo4) == 0){
      model.modelInfo4 <- rbind(model.modelInfo4,data.frame(orifice_depth = NA))
    }
    model.modelInfo5 <- fromJSON(models$OperatingConditions, flatten=TRUE)
    model.modelInfo5[sapply(model.modelInfo5,is.null)] <- NA
    model.modelInfo5 <- as.data.frame(model.modelInfo5)
    model.modelInfo5 <- subset(model.modelInfo5, select = c(LiftGasFlowRate,MeasuredGaugePressure))
    model.modelInfo6 <- subset(LastFlLvl[LastFlLvl$ApiNo14==API,], select = c(FluidLevel,FluidLevelDate))
    if(nrow(model.modelInfo6) == 0){
      model.modelInfo6 <- rbind(model.modelInfo6,data.frame(FluidLevel = NA, FluidLevelDate = NA))
    }
    model.modelInfo <- cbind(model.modelInfo1,model.modelInfo2,model.modelInfo3,model.modelInfo4,model.modelInfo5,model.modelInfo6)
    
    return = model.modelInfo
  },
  error = function(e){
    return(data.frame(Sum_Date = as.character(), WellTestDate = as.character(), OilRate = as.numeric()
                      , ReservoirGasRate = as.numeric(), WaterRate = as.numeric(), LiftGasRate = as.numeric()
                      , DownholePressure = as.numeric(), TubingPressure = as.numeric()
                      , TubingTemperature = as.numeric(), CasingPressure = as.numeric()
                      , CasingTemperature = as.numeric(), IsLatestWellTest = as.numeric()
                      , GLR = as.numeric(), G_RLR = as.numeric(), GLR_total = as.numeric()
                      , GLR_injected = as.numeric(), GOR = as.numeric(), WOR = as.numeric()
                      , GOR_total = as.numeric(), GasRate = as.numeric(), WaterCut = as.numeric()
                      ,GasUtil = as.numeric(), GasLiftMethod = as.character(), FlowType = as.character()
                      , DepthOfInjection = as.numeric(), GasInjectionMethod = as.character()
                      , CorrelationName = as.character(), DeltaLength = as.numeric()
                      , DeltaLengthCloseToValve = as.numeric(), CalculateReservoirPressure = as.character()
                      , CalculateWaterDensityFromSalinity = as.character(), AutoCalibrate = as.character()
                      , calib_parameter_frictional_pressure_drop = as.numeric()
                      , calib_parameter_hydrostatic_pressure_drop = as.numeric()
                      , ReservoirTemperature = as.numeric(), ReservoirPressure = as.numeric()
                      , GaugeMD = as.numeric(), PerforationTopMD = as.numeric(), PerforationBottomMD = as.numeric()
                      , PerforationTopMDActual = as.numeric(), CasingDiameterInside = as.numeric()
                      , CasingDiameterOutside = as.numeric(), TubingDiameterInside = as.numeric()
                      , TubingDiameterOutside = as.numeric(), EndOfTubingMD = as.numeric()
                      , TubingRoughness = as.numeric(), CasingRoughness = as.numeric(), SG_g = as.numeric()
                      , SG_w = as.numeric(), SG_lift_gas = as.numeric(), API = as.numeric(), sigma_o = as.numeric()
                      , sigma_w = as.numeric(), mu_g = as.numeric(), NaCL_percent = as.numeric(), B_w = as.numeric()
                      , nitrogen_mole_percent = as.numeric(), H2S_mole_percent = as.numeric()
                      , CO2_mole_percent = as.numeric(), water_salinity = as.numeric(),orifice_depth = as.numeric()
                      , LiftGasFlowRate = as.numeric(), MeasuredGaugePressure = as.numeric()
                      , FluidLevel = as.numeric(), FluidLevelDate = as.character()))
  })

# warnings and errors
messages <- tryCatch(
  expr = {
    messages <- data.frame(fromJSON(models$Messages, flatten=TRUE))
    if(nrow(subset(messages, messages$MessageType == "Error")) == 0){
      messages <- rbind(messages,data.frame(Message = "No errors.", MessageType = "Error"))
    }  
    if (nrow(subset(messages, messages$MessageType == "Warning")) == 0){
      messages <- rbind(messages,data.frame(Message = "No warning.", MessageType = "Warning"))
    }
    messages
  },
  error = function(e){
    return(data.frame(Message = c("No errors.", "No warnings."), MessageType = c("Error", "Warning")))
  }
)
errors.str <- paste(subset(messages, messages$MessageType == "Error")$Message, collapse = "\n")
warnings.str <- paste(subset(messages, messages$MessageType == "Warning")$Message, collapse = "\n")

