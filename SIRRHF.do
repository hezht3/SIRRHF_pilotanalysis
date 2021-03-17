cd "F:\Box Sync\Duke Kunshan University Intern\1 SIRRHF Trial\data analysis\code"

insheet using "data_htn_add all patients in 12 week.csv", clear

destring sbp_baseline-hr_12week, force replace
destring baseline_dsbp-week12_24pr, force replace

******************************** summary statistics on all data ********************************
* summary statistics
*sum

* summary statistics stratified by sex
*sum if sex == "male"
*sum if sex == "female"
*foreach var in sbp_screen dbp_screen hr_screen sbp_baseline dbp_baseline hr_baseline sbp_4week dbp_4week hr_4week sbp_8week dbp_8week hr_8week sbp_12week dbp_12week hr_12week baseline_dsbp baseline_ddbp baseline_dpr baseline_nsbp baseline_ndbp baseline_npr baseline_24sbp baseline_24dbp baseline_24pr week12_dsbp week12_ddbp week12_dpr week12_nsbp week12_ndbp week12_npr week12_24sbp week12_24dbp week12_24pr {
 *   tabstat `var' sex, 
*}

**************************** summary statistics on patients included ****************************
drop if randomization == "NA"

* summary statistics
sum

* summary statistics stratified by randomization
sum if randomization == "spironolactone"
sum if randomization == "indapamide"

************************** summary statistics on patients finished study ***********************
drop if week12_24pr == .

* summary statistics
sum

* summary statistics stratified by sex
sum if sex == "male"
sum if sex == "female"

* summary statistics stratified by randomization
sum if randomization == "spironolactone"
sum if randomization == "indapamide"