clear all

set obs 40

gen id = 1
replace id = 2 if _n > 20

bys id: gen t = _n

gen y = uniform()
gen z = uniform() > 0.5

** Check if the missing variables in the middle of the sample are imputed /// indeed imputed

replace z = 1 if z[_n - 1] == 1
replace z = . if _n == 15 | _n == 30

xtset id t

xtevent y, policyvar(z) w(2) nodrop

** Check if the first period var is 1

drop __k _k*

replace z = 1 if t == 1 & id == 2

xtevent y, policyvar(z) w(2) nodrop

** Check behavior when a unit does not adopt the policy /// looks fine

drop __k _k*

replace z = 0 if id == 2

xtevent y, policyvar(z) w(2) nodrop

** Check behavior when a unit has adopted the policy before the data begins /// seems to assume that the unit adopted when the data begins

drop __k _k*

replace z = 1 if id == 2

xtevent y, policyvar(z) w(2) nodrop

*** Check behavior when the last observation has missing policy variable /// seems to assume that nothing changes

drop __k _k*

replace z = . if t == 20

xtevent y, policyvar(z) w(2) nodrop

