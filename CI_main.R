## Preparations
if(!dir.exists('output')) dir.create('output')


source('CI_EstimateScores.R')
#################################################################
# read in observed data from which indicator values are estimated 
#   data/all.reef.RData
#       fields include:
#                 P_CODE- identifies either the AIMS Long-term Monitoring program 'RM' or Marine Monitoring Program 'IN' as the source of data
#                     that facilitates some nessecary data processing due to differeing sampling designs among the projects.
#                 NRM_REGION - is a spatial reporting unit used to aggregate scores.
#                 REEF - sampling locations within NRM_REGIONS.
#                 DEPTH - depth of the sample bewlo lowest astronomic tide.
#                 VISIT_NO - a sequential, P_CODE specific tag used to link samples from a REEF, DEPTH and time for reporting.
#                 Date - the date of observation.
#                 subregion -  subsets the Wet Tropics NRM_REGION for reporting
#                 A - porportional cover of the benthos occupied by Alage
#                 AB - porportional cover of the benthos occupied by sediments
#                 HC - porportional cover of the benthos occupied by Hard corals (Scleractinia)
#                 SC - porportional cover of the benthos occupied by Soft corals (Octocorallia) 
#                 CoralCover - the sum of HC and SC
#                 MA - porportional cover of the benthos occupied by macroalgae
#                 Acr - porportional cover of the benthos occupied by hard corals of the family Acroporidae
#                 DISTURBANCE - categorical field idenifying major disturbances, n=none, f=flood, d=disease, s=cyclone/storm, c=crown-of-thorns, m=multiple, d=disease
#                 juv5.d.nf - density of juvenile corals, exclusive of the genus Fungia.
#
# distibutions of gompertz equation parameter estimates used to predict coral cover increase
#   data/gomp.acr.5.Rdata - esimates for family acroproidae at 5m depth
#   data/gomp.acr.2.Rdata - esimates for family acroproidae at 2m depth
#   data/gomp.oth.5.Rdata - esimates for all other hard corals at 5m depth
#   data/gomp.oth.2.Rdata - esimates for fall other hard corals at 2m depth
#
# estimated limits for scoring Macroalgae indicator
#   data/MA.limits.RData
#       fields include: 
#                 REEF,DEPTH as above.
#                 MAupper - upper threshold for MA scoring based on predicted proportion of MA in algae community given the mean water quality of the reef (Thompson et al. 2014)
#                 MAlower - lower threshold for MA scoring based on predicted proportion of MA in algae community given the mean water quality of the reef (Thompson et al. 2014)
# observed cover of hard corals identified to genus 
#   data/hc.genus.RData
#       fields include: 
#                 P_CODE,NRM_REGION,subregion,REEF,DEPTH,VISIT_NO as above.
#                 Columns for for cover of each genus.
# genus scores on rda WQ axis. These were created from the analysis included as a case study in Thompson et al 2014
#   data/wq.scores.RData:
#       fields include: 
#                 weight - the species score on the constrained (water quality gradient) axis of a redundancy analysis 
#                 genus - hard coral genus
#                 DEPTh -  the depth to which the weight applies.
#
# OUTPUT 
#   output/change.all.RData - observed and predicted coral cover for all observations
#   output/coral.RData
##############################################################################################################

source('coralBoot.R')
##########################################################################################################
# bootstrap scores for individual indicators to provide distribution of index scores from which
# confidence intervals in index scores are derived.
# Input
#   output/coral.RData
# Output
#   output/boot.region.metric.sum.RData
#   output/boot.region.score.sum.RData
##################################################################################################

source('Figure1.R')
#########################################################################################
# plotting and supporting analysis for Figure 1
#Input
#   output/coral.RData
#   data/bom.month.RData - data sourced from the Bureau of Meterology
#       fields include: 
#                 REEF - as above
#                 data - mean of estimates from a cluster of nine pixels adjacent to each reef
#                 variableName -  Nap_MIM_median monthly median concentration of Non-algal particulates
#                                 CDOM_MIN_median... colour dissolved organic material concentration
#                                 Kd_490....... light attenuation coefficient
#                                 Chl_......... Chl a concentration
# Output = Figure 1.
###########################################################################################

source('Figure2.R')
#########################################################################################
# plotting of Figure 2
#Input
#   output/coral.RData
#   output/boot.region.metric.sum.RData
#   output/boot.region.score.sum.RData
#   data/CoralDateRanges.RData - information used for plotting limits
#       
# Output = Figure 2.
###########################################################################################

source('Figure3.R')
#########################################################################################
# plotting and supporting analysis for Figure 3
#Input
#   output/coral.RData
#   
# Output = Figure 3.
#        = Table 4
###########################################################################################

source('Figure4.R')
#########################################################################################
# plotting and supporting analysis for Figure 4 and Table 5
#Input
#   output/coral.RData
#   data/discharge.annual.RData - data sourced from the the Queensland government water monitoring information portal
#                                 https://water-monitoring.information.qld.gov.au/host.htm
#       fields include: 
#                 subregion - as above
#                 Year - the water Year (October 1 to September 31) as at September 31.
#                 discharge.c.annual - combined annual discharge from gauged major rivers discharging into the subregion
#
# Output = Figure 4.
###########################################################################################

