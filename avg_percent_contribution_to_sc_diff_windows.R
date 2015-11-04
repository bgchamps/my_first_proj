compare.avg.percent.contribution.to.sc<- function(lagchoice, earliest_passage, latest_passage) {
  library(readstata13)
  library(plyr)
  
  if (lagchoice == '1 lag in the final year') {
    
    five.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_5yrwindow/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
    ten.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1lag_last_year_pre_full/panel_standard_violent_1lag_last_year_pre_full/combined_panel.dta")
    
    five.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1laglastyr_full_5yrwindow/analysis_standard_violent_1lag_last_year_pre_full/synthetic_weights.dta")
    ten.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_1lag_last_year_pre_full/analysis_standard_violent_1lag_last_year_pre_full/synthetic_weights.dta")
  } else if (lagchoice == 'yearly lags') {
    five.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_yearlylag_full_5yrwindow/version_2/panel_standard_violent_yearlylag_full/combined_panel.dta")
    ten.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_yearlylag_full/panel_standard_violent_yearlylag_full/combined_panel.dta")
    
    five.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_yearlylag_full_5yrwindow/version_2/analysis_standard_violent_yearlylag_full/synthetic_weights.dta")
    ten.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_yearlylag_full/analysis_standard_violent_yearlylag_full/synthetic_weights.dta")
  } else if (lagchoice == '3 lags') {
    
    five.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_3lags_full_5yrwindow/panel_standard_violent_3lags_full/combined_panel.dta")
    ten.panel<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_3lags_full/panel_standard_violent_3lags_full/combined_panel.dta")
    
    five.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_3lags_full_5yrwindow/analysis_standard_violent_3lags_full/synthetic_weights.dta")
    ten.weights<- read.dta13("Y:/Synthetic_Controls_Treatment_Effect/standard_3lags_full/analysis_standard_violent_3lags_full/synthetic_weights.dta")  
  } 
  
  test = merge(five.weights, ten.weights, by = c('state_id', 'synth_state'), all.x = T)
  test[is.na(test)] = 0
  
  weight.df = data.frame(state_id = test$state_id, synth_state = test$synth_state, passage = test$passage.x, 
                         five.weight = test$synth_weight.x, ten.weight = test$synth_weight.y)
  me.info = weight.df[weight.df$state_id == "ME",]
  me.info = data.frame(synth_state = me.info$synth_state, stringsAsFactors = F)
  
  all.weight.df = ddply(weight.df, 'state_id', function(df) {
    merge(df, me.info, by = 'synth_state' ,all.y = T)
  })
  
  all.weight.df$five.weight[is.na(all.weight.df$five.weight)] = 0
  all.weight.df$ten.weight[is.na(all.weight.df$ten.weight)] = 0
  all.weight.df$state_id = rep(unique(all.weight.df$state_id[!(is.na(all.weight.df$state_id))]), each = 33)
  
  all.weight.df =ddply(all.weight.df, 'state_id', function(df) {
    df$passage[is.na(df$passage)]<- (df$passage[is.finite(df$passage)])[1]
    return(df)
  })
  
  all.weight.df = all.weight.df[,c(2,1,3,4,5)]
  all.weight.df = all.weight.df[all.weight.df$passage %in% earliest_passage: latest_passage,]
  
  a = ddply(all.weight.df, 'synth_state', function(df) mean(df$five.weight))
  b = ddply(all.weight.df, 'synth_state', function(df) mean(df$ten.weight))
  names(a)[names(a) == 'V1']<- "five.weight"
  names(b)[names(b) == 'V1']<- "ten.weight"
  compare.weight = merge(a,b, by = 'synth_state')
  compare.weight$five.weight = compare.weight$five.weight * 100
  compare.weight$ten.weight = compare.weight$ten.weight * 100
  
  year.of.adoption<- read.csv("U:/Synthetic Controls Project/Data/RTC_Adoption_Year_New.csv")
  
  compare.weight = merge(compare.weight, year.of.adoption, by.x = 'synth_state', by.y = 'state.abb')
  
  compare.weight$adoption.year.manual[compare.weight$adoption.year.manual == 0]<- NA
  compare.weight = compare.weight[order(compare.weight$adoption.year.manual),]
  
  my.matrix = t(compare.weight[,2:3])
  my.bar = barplot(my.matrix, beside = T, axisnames = F, ylim = c(min(sapply(my.matrix, min)) - 5, max(sapply(my.matrix, max)) + 5),
                   col = c('red', 'blue'), main = "average percent contribution to synthetic control", ylab = 'percent',
                   sub = 
                     paste(paste(paste(paste("states ordered by year of rtc adoption; use",lagchoice, sep = " "), "; standard variables; averaged over all states that adopted between", 
                                       earliest_passage, sep = " "), "and", sep = " "), latest_passage, sep = " "))
  
  
  text.state.year = paste("(", compare.weight$adoption.year.manual, ")", sep = "")
  text.state.year = paste(compare.weight$synth_state, text.state.year, sep = "\n")
  text(x = apply(my.bar, 2, mean), y = -2, text.state.year, cex = .6)
  
  legend('topleft', c('5 year window', '10 year window'), col = c('red', 'blue'), 
         pch = 15, bty = 'n')
  
}