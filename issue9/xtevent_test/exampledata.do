clear all
set more off
set seed 15

program main
    simulate_data, save_as("simulation_data_dynamic") alpha_m(42) alpha_r(1.5)

    simulate_data, static save_as("simulation_data_static") sigma(0.5) alpha_static(5.21)
end

program simulate_data
    syntax, save_as(str) [static n(int 50) t(int 40) l(int 5) beta_smooth(real 0) ///
        beta_jump(real 1) alpha_m(real 0.5) alpha_r(real 0.5) alpha_static(real 0.5)  sigma(real 0.7) tau(real 0.55) gamma_min(real 0.18)]

    clear
    set obs `n'  
    gen id = _n

    gen eventtime = ceil(`t'*runiform())

    expand `t'
    bys id: gen t = _n

    bys id (t): gen z = t >= eventtime

    if "`static'"=="static" {
        gen y_static = `alpha_static' + `beta_jump'*z -`beta_jump'*0.15*(t >= eventtime+1) - ///
            `beta_jump'*0.3*(t >= eventtime+3) -`beta_jump'*0.15*(t >= eventtime+4) + `sigma'*rnormal(0,1)
    }

    else {
        gen y_base = `sigma'*rnormal(0,1)

        * Monotone confound
        gen eta_m = 0
        replace eta_m = `gamma_min' if t <= eventtime - `l' - 1

        forvalues k = -`l'(1)-1 {
            replace eta_m = `gamma_min'/`l'*(-`k') if t == eventtime + `k'
        }

        forvalues k = 0(1)`l' {
            replace eta_m = -`beta_jump'/`l'*(`k'+1) if t == eventtime + `k'
        }

        bys id (t): replace eta_m = eta_m[_n-1] if t >= eventtime + `l'

        foreach stub in jump smooth {
            gen y_`stub'_m = y_base + eta_m + `beta_`stub''*z + `alpha_m'
        }

        set seed 11
        gen x_jump_m = eta_m + `tau'*rnormal(0,1)

        * Mean reverting confound
        gen eta_r = 0
        replace eta_r = `beta_jump' if t <= eventtime - `l' - 1 | t>= eventtime + `l' + 1

        forvalues k = -`l'(1)-1 {
            replace eta_r = `beta_jump'/`l'*(-`k') if t == eventtime + `k'
        }

        forvalues k = 0(1)`l' {
            replace eta_r = `beta_jump'/`l'*(`k'+1) if t == eventtime + `k'
        }

        gen y_r = y_base + eta_r + `beta_jump'*z + `alpha_r'

        gen x_r = eta_r + `tau'*rnormal(0,1)
    }

    xtset id t

    save "release/exampleplots/`save_as'", replace
end

* Execute
main
