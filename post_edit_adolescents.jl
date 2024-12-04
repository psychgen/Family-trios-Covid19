using LinearAlgebra
using VCModels
using JLD
using StatsModels, StatsBase, Statistics
using DataFrames, CSV

cd("L:/projects/cme_trio/results/")

vari = "Mean_SCL_scaled"

m_full = JLD.load(vari * "_full_35034.jld")["model"]
m_mo = JLD.load(vari * "_mo_35034.jld")["model"]
m_fo = JLD.load(vari * "_fo_35034.jld")["model"]
m_direct = JLD.load(vari * "_direct_35034.jld")["model"]
m_null = JLD.load(vari * "_null_35034.jld")["model"]

# Compare fits
# -------------------------------------------
ms = [m_full, m_mo, m_fo, m_direct, m_null]
round.(aic.(ms), digits = 2)
round.(bic.(ms), digits = 2)
round.(deviance.(ms), digits = 2)

aic_m = round.(aic.(ms), digits = 2)
bic_m = round.(bic.(ms), digits = 2)
deviance_m = round.(deviance.(ms), digits = 2)

lrtest(m_mo, m_direct) #to compare models that are similar in AIC

for i in 2:length(ms)
    @show i
    @show lrtest(ms[1],ms[i])
end

fit_1 = hcat(aic_m, bic_m, deviance_m)
fit_2 = DataFrame(fit_1, ["aic", "bic", "deviance"])
CSV.write(vari * "_fit.csv", fit_2)

# Full model
# --------------------------------------------
# rs = [A_m, A_f, A_c, D_fm, D_cm, D_cf, R]
function VCModels.transform(θ::Vector) 
    δ = Vector{eltype(θ)}(undef, length(θ))
    δ[1] = θ[1]^2
    δ[2] = θ[2]^2 + θ[4]^2
    δ[3] = θ[3]^2 + θ[5]^2 + θ[6]^2
    δ[4] = θ[2] * θ[1]
    δ[5] = θ[3] * θ[1]
    δ[6] = θ[5] * θ[4] + θ[3] * θ[2]
    δ[7] = θ[7]
    println(δ)
    println("working")
    δ ./ (sum(δ)-δ[4])
end

δ = m_full.δ
VCModels.transform(m_full.θ)
println(δ)
Δ = [δ[1] δ[4] δ[5] 0;
     δ[4] δ[2] δ[6] 0;
     δ[5] δ[6] δ[3] 0;
     0 0 0 δ[7]]
# Rescale to % variance
v_pos = [1, 2, 3, 5, 6, 7]
var_tot = sum(δ[v_pos])
sc = diagm(fill(1 / sqrt(var_tot), 4))
Δ_std = sc * Δ * sc

#standard error full model 
J_full = jacobian(m_full)
C_full = vcovvc(m_full)
Ct_full = J_full * C_full * J_full'
set_full = sqrt.(diag(Ct_full))


par_full = [Δ_std[3, 3], Δ_std[1, 1], Δ_std[2, 2], # Switch first to be focal individual (1 for mother, 2 for father, 3 for child)
Δ_std[3, 1], Δ_std[3, 2], Δ_std[2, 1], Δ_std[4, 4]]
round.(par_full, digits = 3)'
sum(par_full[[1, 2, 3, 4, 5, 7]])

# Correlations
s = sqrt.(diag(Δ))
cor_mat = diagm(1 ./ s) * Δ * diagm(1 ./ s)

#[mf, cm, cf]
cor_1 = hcat( ["mf", "cm", "cf"], [cor_mat[1,2], cor_mat[1,3],cor_mat[2,3]]) 

cor = DataFrame(cor_1, ["var", "cor"])
CSV.write(vari * "_cor.csv", cor)

# MO model 
#---------------------------------------------
function VCModels.transform(θ::Vector)
    δ = Vector{eltype(θ)}(undef, length(θ))
    δ[1] = θ[1]^2    
    δ[2] = θ[2]^2 * θ[3]^2    
    δ[3] = θ[1] * θ[2]  
    δ[4] = θ[4]
    println("worked")
    δ ./ sum(δ)
end

# [rs[1], rs[3], rs[5], rs[7]]
δ_mo = m_mo.δ
δ_std_mo = δ_mo ./sum(δ_mo)
sum(δ_std_mo)

#standard error
J_mo = jacobian(m_mo)
C_mo = vcovvc(m_mo)
Ct_mo = J_mo * C_mo * J_mo'
set_mo = sqrt.(diag(Ct_mo))

Δ = [δ[1] δ[4] δ[5] 0;
     δ[4] δ[2] δ[6] 0; 
     δ[5] δ[6] δ[3] 0;
     0 0 0 δ[7]]

var_tot_mo = sum(δ_mo)
sc_mo = diagm(fill(1 / sqrt(var_tot_mo), size(Δ, 1)))
Δ_std_mo = sc_mo * Δ * sc_mo
par_mo = [Δ_std_mo[3, 3],Δ_std_mo[1, 1],0,Δ_std_mo[3, 1],0,0, Δ_std_mo[4, 4]] # switch 2 and 3 here since fathers is the focal individual
round.(par_mo, digits = 3)'
sum(par_mo)

# FO model 
#---------------------------------------------
function VCModels.transform(θ::Vector) 
    δ = Vector{eltype(θ)}(undef, length(θ))
    δ[1] = θ[1]^2    
    δ[2] = θ[2]^2 * θ[3]^2    
    δ[3] = θ[1] * θ[2]  
    δ[4] = θ[4]
    println("worked")
    δ ./ sum(δ)
end

# [rs[2], rs[3], rs[6], rs[7]]
δ_fo = m_fo.δ
δ_std_fo = δ_fo ./sum(δ_fo)
sum(δ_std_fo)

#standard error
J_fo = jacobian(m_fo)
C_fo = vcovvc(m_fo)
Ct_fo = J_fo * C_fo * J_fo'
set_fo = sqrt.(diag(Ct_fo))

Δ = [δ[1] δ[4] δ[5] 0; 
     δ[4] δ[2] δ[6] 0;
     δ[5] δ[6] δ[3] 0;
     0 0 0 δ[7]]

var_tot_fo = sum(δ_fo)
sc_fo = diagm(fill(1 / sqrt(var_tot_fo), size(Δ, 1)))
Δ_std_fo = sc_fo * Δ * sc_fo
par_fo = [Δ_std_fo[3, 3],0, Δ_std_fo[2, 2],0, Δ_std_fo[3, 2],0, Δ_std_fo[4, 4]] # switch 2 and 3 here since fathers is the focal individual
round.(par_fo, digits = 3)'
sum(par_fo)

# Direct model
# --------------------------------------------
function VCModels.transform(θ::Vector)
    δ = Vector{eltype(θ)}(undef, length(θ))
    δ[1] = θ[1]^2
    δ[2] = θ[2]
    println("worked")
    δ ./ sum(δ)
end

#standard error
J_direct = jacobian(m_direct)
C_direct = vcovvc(m_direct)
Ct_direct = J_direct * C_direct * J_direct'
set_direct = sqrt.(diag(Ct_direct))

δ_direct = m_direct.δ
Δ_direct = [δ_direct[1] 0;
            0 δ_direct[2]]

var_tot_direct = sum(δ_direct)
sc_direct = diagm(fill(1 / sqrt(var_tot_direct), size(Δ_direct, 1)))
Δ_std_direct = sc_direct * Δ_direct * sc_direct
par_direct = [Δ_std_direct[1, 1], 0, 0, 0, 0, 0, Δ_std_direct[2, 2]] 
round.(Δ_std_direct, digits = 3)
var_tot_direct = sum(Δ_std_direct)


# Null model
# --------------------------------------------
function VCModels.transform(m::VCModel)
    m.θ
end

δ_null = VCModels.transform(m_null)
sc_null = diagm(fill(1 / sqrt(δ_null[1]), 1))
std_null = sc_null * δ_null * sc_null
par_null = [0, 0, 0, 0, 0, 0, 1.0]
         

# Write results
# ----------------------------------------------
par = vcat(par_full', par_mo', par_fo', par_direct', par_null')
datres = DataFrame(par, ["o", "m", "p", "om", "op", "mp", "e"]) 
datres[!, :model] = [ "full","mo", "fo", "direct", "null"]
datresl = stack(datres)
CSV.write(vari * ".csv", datresl)
round.(datres[:, 1:7], digits = 2)
