library(readstata13)
library(plyr)

#weight_choice can either be "x_years_after_adoption", "no_weight", "firstyr"#
get.aggregate.tep.x.years.after.adoption<-function(combined.panel.data, years, l_five_yr_split = FALSE,
                                                   drop_TX_FL = FALSE, weight_choice) {
  library(sandwich)
  raw.df<- read.dta13("Z:/RTC (New)/sharing/raw_dataset.dta")
  rel.raw.df = raw.df[, names(raw.df) %in% c('state_id', 'year', 'population_demographic')]
  
  if(drop_TX_FL == TRUE) {
    combined.panel.data = combined.panel.data[!(combined.panel.data$state_id %in% c("TX", "FL")),]
  }
  
  rel=ddply(combined.panel.data, 'state_id', function(df) {
    if (l_five_yr_split == T) {
      
      add.years = df$passage[1] + years - 5
      finalyr = min(2012, (df$passage[1]) + years - 5)
      t.pass = df$violent_rate[df$indicator == 'Treatment' & df$year == (df$passage[1] - 5)]
      s.pass = df$violent_rate[df$indicator == 'Synthetic Control' & df$year == (df$passage[1] - 5)]
      
    } else {
      
      add.years = df$passage[1] + years
      finalyr = min(2012, (df$passage[1]) + years)
      t.pass = df$violent_rate[df$indicator == 'Treatment' & df$year == df$passage[1]]
      s.pass = df$violent_rate[df$indicator == 'Synthetic Control' & df$year == df$passage[1]]
      
    }
    
    t.finalyr = df$violent_rate[df$indicator == 'Treatment' & df$year == finalyr[1]]
    s.finalyr = df$violent_rate[df$indicator == 'Synthetic Control' & df$year == finalyr[1]]
    
    a=data.frame(t.pass, s.pass, t.finalyr, s.finalyr, finalyr)
    a[add.years == finalyr,]
  })
  
  rel$tep = (((rel$t.finalyr - rel$s.finalyr)/rel$s.finalyr) - 
               ((rel$t.pass - rel$s.pass)/rel$s.pass))*100
  names(rel)[names(rel) == 'finalyr'] = 'year'
  
  if (weight_choice == 'x_years_after_adoption') {
    
    rel.with.weight = merge(rel, rel.raw.df, by = c('state_id', 'year')) 
    
    model<-lm(rel.with.weight$tep ~ 1, weights = rel.with.weight$population_demographic)
    model$newse<-round(sqrt(vcovHC(model, type = 'HC1')),3)
  } else if (weight_choice == 'no_weight') {
    
    rel.with.weight = merge(rel, rel.raw.df, by = c('state_id', 'year')) 
    
    model<-lm(rel.with.weight$tep ~ 1)
    model$newse<-round(sqrt(vcovHC(model, type = 'HC1')),3)
    
  } else if (weight_choice == 'firstyr') {
    rel$firstyr = 1977
    rel.with.weight = merge(rel, rel.raw.df, by.x = c("state_id", "firstyr"), by.y = c("state_id", "year"))
    
    model<-lm(rel.with.weight$tep ~ 1, weights = rel.with.weight$population_demographic)
    model$newse<-round(sqrt(vcovHC(model, type = 'HC1')),3)
  }
  
  (a = data.frame(tep = coefficients(model), se = model$newse<-round(sqrt(vcovHC(model, type = 'HC1')),3)[1,1]))
  
}

make.6x10.table<- function(specification ,weight_choice, drop_TX_FL = FALSE) {
  if (toupper(specification) == "STANDARD") {
    fiveyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_5yrwindow/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
    sixyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_6yrwindow/panel_standard_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_7yrwindow/panel_standard_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_8yrwindow/panel_standard_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_9yrwindow/panel_standard_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1lag_last_year_pre_full/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
  } else if (toupper(specification) == 'MM') {
    fiveyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_5yrwindow/panel_mm_violent_1lag_last_year_pre_full_5yrwindow/combined_panel.dta")
    sixyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_6yrwindow/panel_mm_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow  <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_7yrwindow/panel_mm_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_8yrwindow/panel_mm_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_9yrwindow/panel_mm_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_10yrwindow/panel_mm_violent_1lag_last_year_pre_full_10yrwindow/combined_panel.dta")
  } else if (toupper(specification) == 'LOTT') {
    fiveyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_5yrwindow/panel_lott_violent_1lag_last_year_pre_full_5yrwindow/combined_panel.dta")
    sixyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_6yrwindow/panel_lott_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_7yrwindow/panel_lott_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_8yrwindow/panel_lott_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_9yrwindow/panel_lott_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_10yrwindow/panel_lott_violent_1lag_last_year_pre_full_10yrwindow/combined_panel.dta")
  }
  
  
  five=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(fiveyrwindow, x,   weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  six=sapply(1:6, function(x) get.aggregate.tep.x.years.after.adoption(sixyrwindow, x,     weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  seven=sapply(1:7, function(x) get.aggregate.tep.x.years.after.adoption(sevenyrwindow, x, weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  eight=sapply(1:8, function(x) get.aggregate.tep.x.years.after.adoption(eightyrwindow, x, weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  nine=sapply(1:9, function(x) get.aggregate.tep.x.years.after.adoption(nineyrwindow, x,   weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  ten=sapply(1:10, function(x) get.aggregate.tep.x.years.after.adoption(tenyrwindow, x,    weight_choice = weight_choice, drop_TX_FL = drop_TX_FL))
  
  make.table.with.10.cols<- function(my.list) {
    my.list[1,]=round(unlist(my.list[1,]), digits = 3)
    test = matrix(c(my.list, rep(NA, 2*(10 - ncol(my.list)))), nrow = 2, dimnames = list(c("coefficient","se"), paste(1:10, "yr tep", sep = " ")))
    test[2,]=paste('(', test[2,], ')', sep = '')
    test
  }
  
  my.table = lapply(list(five, six, seven, eight, nine, ten), make.table.with.10.cols)
  my.table = do.call(rbind, my.table)
  
  if (weight_choice == "x_years_after_adoption") {
    my.note = "Note: Weight used to aggregate treatment effects is population in a given number of years after RTC adoption"
  } else if (weight_choice == "no_weight") {
    my.note = "Note: Aggregate treatment effect is a simple average of state specific TEPs"
  } else if (weight_choice == "firstyr") {
    my.note = "Note: Weight used to aggregate treatment effects is population in 1977"
  }
  
  if(drop_TX_FL == TRUE) {
    my.note = paste(my.note, "TX and FL dropped from analysis", sep = "; ")
  }
  
  my.title = c("Aggregate TEP for 1 through 10 years after RTC adoption using 5 through 10 year windows and",
               specification, "variables")
  my.title = paste(my.title, collapse = " ")
  
  stargazer(my.table, title = my.title, notes = my.note)
  
}
make.trend_regression.table<- function(specification, use_only_first_five_years, 
                                       weight_choice = "x_years_after_adoption") {
  
  if (toupper(specification) == "STANDARD") {
    fiveyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_5yrwindow/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
    sixyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_6yrwindow/panel_standard_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_7yrwindow/panel_standard_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_8yrwindow/panel_standard_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_9yrwindow/panel_standard_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1lag_last_year_pre_full/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
  } else if (toupper(specification) == 'MM') {
    fiveyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_5yrwindow/panel_mm_violent_1lag_last_year_pre_full_5yrwindow/combined_panel.dta")
    sixyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_6yrwindow/panel_mm_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow  <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_7yrwindow/panel_mm_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_8yrwindow/panel_mm_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_9yrwindow/panel_mm_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/mm_1laglastyr_full_10yrwindow/panel_mm_violent_1lag_last_year_pre_full_10yrwindow/combined_panel.dta")
  } else if (toupper(specification) == 'LOTT') {
    fiveyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_5yrwindow/panel_lott_violent_1lag_last_year_pre_full_5yrwindow/combined_panel.dta")
    sixyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_6yrwindow/panel_lott_violent_1lag_last_year_pre_full_6yrwindow/combined_panel.dta")
    sevenyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_7yrwindow/panel_lott_violent_1lag_last_year_pre_full_7yrwindow/combined_panel.dta")
    eightyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_8yrwindow/panel_lott_violent_1lag_last_year_pre_full_8yrwindow/combined_panel.dta")
    nineyrwindow<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_9yrwindow/panel_lott_violent_1lag_last_year_pre_full_9yrwindow/combined_panel.dta")
    tenyrwindow <- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/lott_1laglastyr_full_10yrwindow/panel_lott_violent_1lag_last_year_pre_full_10yrwindow/combined_panel.dta")
  }
  
  
  if (use_only_first_five_years == FALSE) {
    five=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(fiveyrwindow, x,   weight_choice = weight_choice))
    six=sapply(1:6, function(x) get.aggregate.tep.x.years.after.adoption(sixyrwindow, x,     weight_choice = weight_choice))
    seven=sapply(1:7, function(x) get.aggregate.tep.x.years.after.adoption(sevenyrwindow, x, weight_choice = weight_choice))
    eight=sapply(1:8, function(x) get.aggregate.tep.x.years.after.adoption(eightyrwindow, x, weight_choice = weight_choice))
    nine=sapply(1:9, function(x) get.aggregate.tep.x.years.after.adoption(nineyrwindow, x,   weight_choice = weight_choice))
    ten=sapply(1:10, function(x) get.aggregate.tep.x.years.after.adoption(tenyrwindow, x,    weight_choice = weight_choice))
    
    
    do.trend.regression<- function(my.list) {
      tep.by.year = unlist(my.list[1,])
      trend.covariate = 1: length(tep.by.year)
      lm (tep.by.year ~ trend.covariate)
    }
    
    reg.5 = do.trend.regression(five)
    reg.6 = do.trend.regression(six)
    reg.7 = do.trend.regression(seven)
    reg.8 = do.trend.regression(eight)
    reg.9 = do.trend.regression(nine)
    reg.10 = do.trend.regression(ten)
    
    my.title = c("Dependent Variable: Aggregate TEP for all years available;",
                 specification, "variables used")
    my.title = paste(my.title, collapse = " ")
    
    stargazer(reg.5, reg.6, reg.7, reg.8, reg.9, reg.10, 
              dep.var.caption = my.title, dep.var.labels = "Window Size",
              column.labels = c('5 years', '6 years', '7 years', '8 years', '9 years', '10 years'),
              omit.stat = c("adj.rsq", "f"), 
              notes = "Weight used to aggregate treatment effects is population in a given number of years after RTC adoption.")
  } else {
    five=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(fiveyrwindow, x, weight_choice = weight_choice))
    six=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(sixyrwindow, x, weight_choice = weight_choice))
    seven=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(sevenyrwindow, x, weight_choice = weight_choice))
    eight=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(eightyrwindow, x, weight_choice = weight_choice))
    nine=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(nineyrwindow, x, weight_choice = weight_choice))
    ten=sapply(1:5, function(x) get.aggregate.tep.x.years.after.adoption(tenyrwindow, x, weight_choice = weight_choice))
    
    
    do.trend.regression<- function(my.list) {
      tep.by.year = unlist(my.list[1,])
      trend.covariate = 1: length(tep.by.year)
      lm (tep.by.year ~ trend.covariate)
    }
    
    reg.5 = do.trend.regression(five)
    reg.6 = do.trend.regression(six)
    reg.7 = do.trend.regression(seven)
    reg.8 = do.trend.regression(eight)
    reg.9 = do.trend.regression(nine)
    reg.10 = do.trend.regression(ten)
    
    my.title = c("Dependent Variable: Aggregate TEP for first 5 years after adoption;",
                 specification, "variables used")
    my.title = paste(my.title, collapse = " ")
    
    stargazer(reg.5, reg.6, reg.7, reg.8, reg.9, reg.10, 
              dep.var.caption = my.title, dep.var.labels = "Window Size",
              column.labels = c('5 years', '6 years', '7 years', '8 years', '9 years', '10 years'),
              omit.stat = c("adj.rsq", "f"), 
              notes = "Weight used to aggregate treatment effects is population in a given number of years after RTC adoption.")
    
  }
}

make.6x10.table("standard","x_years_after_adoption", FALSE)
make.6x10.table("standard","no_weight", FALSE)
make.6x10.table("standard","firstyr", FALSE)
make.6x10.table("standard","x_years_after_adoption", TRUE)
make.6x10.table("standard","no_weight", TRUE)
make.6x10.table("standard","firstyr", TRUE)

make.6x10.table("mm","x_years_after_adoption", FALSE)
make.6x10.table("mm","no_weight", FALSE)
make.6x10.table("mm","firstyr", FALSE)
make.6x10.table("mm","x_years_after_adoption", TRUE)
make.6x10.table("mm","no_weight", TRUE)
make.6x10.table("mm","firstyr", TRUE)

make.6x10.table("lott","x_years_after_adoption", FALSE)
make.6x10.table("lott","no_weight", FALSE)
make.6x10.table("lott","firstyr", FALSE)
make.6x10.table("lott","x_years_after_adoption", TRUE)
make.6x10.table("lott","no_weight", TRUE)
make.6x10.table("lott","firstyr", TRUE)

make.trend_regression.table("standard", T)
make.trend_regression.table("standard", F)
make.trend_regression.table("mm", T)
make.trend_regression.table("mm", F)
make.trend_regression.table("lott", T)
make.trend_regression.table("lott", F)