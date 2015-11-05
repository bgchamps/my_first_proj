clear all
set more off
version 13.0
pause on
net describe _gwtmean, from(http://fmwww.bc.edu/RePEc/bocode/_)
net install _gwtmean.pkg

use "./programs/output_standard_violent_3lags/append_data.dta", clear
drop if year == .

generate post = year - passage
bysort state_id: egen max_post = max(post)
keep if (post == 0 & max_post == 10) | post == 10

gen tep = ((treated / synthetic)-1)*100
gen tep_0 = tep if post == 0
bysort state_id: egen tep0 = max(tep_0)
gen tep_norm = tep - tep0
drop if post == 0
mmerge state_id year using "./data/synthetic_dataset (1981-2012).dta", type(n:1) unmatched(master) ukeep(popstatecensus) urename(popstatecensus population)

gen treat8589 = tep_norm if (passage>=1985 & passage<=1989)
gen treat9094 = tep_norm if (passage>=1990 & passage<=1994)
gen treat9599 = tep_norm if (passage>=1995 & passage<=1999)
gen treat0004 = tep_norm if (passage>=2000 & passage<=2004)

egen Mean = wtmean(tep_norm), weight(population)
egen Median = median(tep_norm)

graph bar (asis) treat8589 treat9094 treat9599 treat0004, over(state_id, label(nolabels) sort(tep_norm)) blabel(group, pos(outside) gap(.1)) title("The Effect of RTC Laws on Violent Crime After 10 Years") subtitle("Synthetic Cohort Estimates for 26 States (1977-2012)") ytitle("Treatment Effect Percentage -- TEP" " ") yline(18.5, lcolor(red) lw(thin)) text(22 7 "Mean = 18.5%", size(3) color(red)) yline(13.3, lcolor(blue) lw(thin)) text(10 7 "Median = 13.3%", size(3) color(blue)) nofill bar(1, color(gs16) lc(black) lw(thin)) bar(2, color(red*.7) lc(black) lw(thin)) bar(3, color(blue*.9)) bar(4, color(black)) legend(order(1 2 3 4) label(1 "85-89 (6)") label(2 "90-94 (6)") label(3 "95-99 (13)") label(4 "2001 (1)") subtitle("Year of passage") rows(1)) graphregion(fcolor(white) lcolor(white))
graph export "Treatment Effect Bar Chart - Total.eps", as(eps) replace

tab passage

drop if state_id=="AK"
drop if state_id=="MT"
drop if state_id=="ND"
drop if state_id=="ME"
drop if state_id=="NE"
drop if state_id=="NM"
drop if state_id=="NV"
drop if state_id=="SD"
drop if state_id=="TN"
drop if state_id=="WV"
drop if state_id=="WY"

egen Mean_17 = wtmean(tep_norm), weight(population)
egen Median_17 = median(tep_norm)

graph bar (asis) treat8589 treat9094 treat9599 treat0004, over(state_id, label(nolabels) sort(tep_norm)) blabel(group, pos(outside) gap(.1)) title("The Effect of RTC Laws on Violent Crime After 10 Years") subtitle("Synthetic Cohort Estimates for 17 States (1977-2012)") ytitle("Treatment Effect Percentage -- TEP" " ") yline(16.9, lcolor(red) lw(thin)) text(20 7 "Mean = 16.9%", size(3) color(red)) yline(9.6, lcolor(blue) lw(thin)) text(7 7 "Median = 9.6%", size(3) color(blue)) nofill bar(1, color(gs16) lc(black) lw(thin)) bar(2, color(red*.7) lc(black) lw(thin)) bar(3, color(blue*.9)) bar(4, color(black)) legend(order(1 2 3 4) label(1 "85-89 (3)") label(2 "90-94 (4)") label(3 "95-99 (9)") label(4 "2001 (1)") subtitle("Year of passage") rows(1)) graphregion(fcolor(white) lcolor(white))
graph export "Treatment Effect Bar Chart - Total Alt.eps", as(eps) replace

