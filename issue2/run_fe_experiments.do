
import delimited "C:/Users/c1nhs01/Downloads/Burda-Harding.csv"

gen year_firm = (year * 1000) + firm

foreach var in year firm year_firm {
	
	reg lsales1 pat_any lgmalspi lgmalspt i.firm i.year, cluster(`var')

}

