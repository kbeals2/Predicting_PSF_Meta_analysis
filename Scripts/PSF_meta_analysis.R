(.packages())

library(pacman)
p_load("fBasics", "rcompanion", "plyr", "dplyr", "lme4", "car", "ggplot2", "metafor", "grid", "gridExtra", "cowplot")

### Load in master data 
data <- read.csv("TableS1.csv")
str(data)

### create separate datasets for competition, stress, & disturbance studies
competition.data <- filter(data, type == "competition")
stress.data <- filter(data, type == "stress")
disturb.data <- filter(data, type == "disturbance")

### create separate datasets for manipulations of PSF, competition, & stress
live.sterile.data <- filter(data, feedback_manip == "live_sterile")
home.away.data <- filter(data, feedback_manip == "own_foreign")
fungicide.data <- filter(data, feedback_manip == "fungicide")
alone.together.data <- filter(data, type_manip == "alone_together")
con.hetero.data <- filter(data, type_manip == "con_hetero")
drought.data <- filter(data, type_manip == "drought")
fert.data <- filter(data, type_manip == "fertilization")
light.data <- filter(data, type_manip == "light")
mining.data <- filter(data, type_manip == "mining")
graz.herb.data <- filter(data, type_manip == "grazing/herbivory")



########## Effect of PSF on plant growth (Main effect) ##########

# Are the effects of soil on plant growth different from zero? (Supplemental Table S2, row 3)  
PSF.rma <- rma.mv(RII_PSF, RII_PSF_Vi, random = ~ 1 | focal_sp , data = data)
summary(PSF.rma)

# Are the effects of different PSF manipulations different from zero? (Supplemental Table S2, rows 5-7) 
PSF.manip.type.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ feedback_manip-1, random = ~ 1 | focal_sp, data = data)
summary(PSF.manip.type.rma)

# Post-hoc: Do the effects of each feedback manipulation on plant growth differ from each other? (Supplemental Table S2, rows 90-92) 
linearHypothesis(PSF.manip.type.rma, c("feedback_manipfungicide - feedback_maniplive_sterile = 0"))
linearHypothesis(PSF.manip.type.rma, c("feedback_manipfungicide - feedback_manipown_foreign = 0"))
linearHypothesis(PSF.manip.type.rma, c("feedback_maniplive_sterile - feedback_manipown_foreign = 0"))

# Are the effects of soil on different plant growth forms different from zero? (Supplemental Table S2, rows 9-12) 
PSF.plant.growth.form.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ focal_func-1, random = ~ 1 | focal_sp, data = data)
summary(PSF.plant.growth.form.rma)

# Post-hoc: Do plant growth forms differ in their PSFs? (Supplemental Table S2, rows 94-99) 
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcforb - focal_funcgrass = 0"))
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcforb - focal_funcshrub = 0"))
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcforb - focal_functree = 0"))
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcgrass - focal_funcshrub = 0"))
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcgrass - focal_functree = 0"))
linearHypothesis(PSF.overall.plant.growth.form.rma, c("focal_funcshrub - focal_functree = 0"))

# Are the effects of experiment location (gh vs. field) on PSFs different from zero? (Supplemental Table S2, rows 14-15) 
exp.locat.PSF.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = data)
summary(exp.locat.PSF.rma)

# Post-hoc: Do PSFs in field exps differ from PSFs in gh exps? (Supplemental Table S2, row 101) 
linearHypothesis(exp.locat.PSF.rma, c("exp_locationfield - exp_locationgh = 0"))

# Are the effects of experiment location (gh vs. field) on PSFs of away-conditioned vs home-conditioned studies different from zero? (Supplemental Table S2, rows 17-18) 
exp.locat.PSF.home.away.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = home.away.data)
summary(exp.locat.PSF.home.away.rma)

# Post-hoc: Do PSFs in field exps differ from PSFs in gh exps in away-conditioned vs home-conditioned studies? (Supplemental Table S2, row 103) 
linearHypothesis(exp.locat.PSF.home.away.rma, c("exp_locationfield - exp_locationgh = 0"))

# Are the effects of experiment location (gh vs. field) on PSFs of non-fungicide vs fungicide studies different from zero? (Supplemental Table S2, rows 20-21) 
exp.locat.PSF.fungicide.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = fungicide.data)
summary(exp.locat.PSF.fungicide.rma)

# Post-hoc: Do PSFs in field exps differ from PSFs in gh exps in non-fungicide vs. fungicide studies? (Supplemental Table S2, row 105) 
linearHypothesis(exp.locat.PSF.fungicide.rma, c("exp_locationfield - exp_locationgh = 0"))

# Are the effects of field-conditioned inc & gh-conditioned inoc on PSF different from zero? (Supplemental Table S2, rows 23-24) 
PSF.inoc.source.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = data)
summary(PSF.inoc.source.rma)

# Post-hoc: Do the effects of field-conditioned inc & gh-conditioned inoc on PSF differ from each other? (Supplemental Table S2, row 107) 
linearHypothesis(PSF.inoc.source.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc & gh-conditioned inoc on live vs sterile PSFs different from zero? (Supplemental Table S2, rows 26-27) 
PSF.inoc.source.live.sterile.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = live.sterile.data)
summary(PSF.inoc.source.live.sterile.rma)

# Post-hoc: Do the effects of field-conditioned inoc on live vs sterile PSF manipulations differ from the effect of gh-conditioned inoc on live vs sterile PSF manipulations? (Supplemental Table S2, row 109) 
linearHypothesis(PSF.inoc.source.live.sterile.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inc & gh-conditioned inoc on away-conditioned vs home-conditioned PSF manipulations different from zero? Supplemental Table S2, rows 29-30) 
PSF.inoc.source.home.away.rma <- rma.mv(RII_PSF, RII_PSF_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = home.away.data)
summary(PSF.inoc.source.home.away.rma)

# Post-hoc: Do the effects of field-conditioned inoc on away-conditioned vs home-conditioned PSF manipulations differ from the effect of gh-conditioned inoc on home vs away PSF manipulations? (Supplemental Table S2, row 111) 
linearHypothesis(PSF.inoc.source.home.away.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))



########## Effect of COMPETITION on plant growth (Main effect) ##########

# Are the effects of competition on plant growth different from zero? (Supplemental Table S2, row 32) 
competition.rma <- rma.mv(RII_CSD, RII_CSD_Vi, random = ~ 1 | focal_sp , data = competition.data)
summary(competition.rma)

# Are the effects of alone vs. together & intraspecific vs. interspecific competition different from zero? (Supplemental Table S2, rows 34-35) 
comp.manip.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ type_manip-1, random = ~ 1 | focal_sp, data = competition.data)
summary(comp.manip.rma)

# Post-hoc: Do the overall effects of alone/together competition differ from the overall effects of intraspecific/interspecific competition? (Supplemental Table S2, row 113) 
linearHypothesis(comp.manip.rma, c("type_manipalone_together - type_manipcon_hetero = 0"))

# Are the effects of competition on different plant growth forms different from zero? (Supplemental Table S2, rows 37-40) 
CSD.plant.growth.form.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ focal_func-1, random = ~ 1 | focal_sp, data = competition.data)
summary(CSD.plant.growth.form.rma)

# Post-hoc: Do plant growth forms differ in their response to competition? (Supplemental Table S2, rows 115-120) 
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcforb - focal_funcgrass = 0"))
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcforb - focal_funcshrub = 0"))
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcforb - focal_functree = 0"))
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcgrass - focal_funcshrub = 0"))
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcgrass - focal_functree = 0"))
linearHypothesis(CSD.plant.growth.form.rma, c("focal_funcshrub - focal_functree = 0"))

# Are the effects of experiment location on competition different from zero? (Supplemental Table S2, rows 42-43) 
exp.locat.comp.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = competition.data)
summary(exp.locat.comp.rma)

# Post-hoc: Do the effects of competition in field exps differ from the effects of competition in gh exps? (Supplemental Table S2, row 122) 
linearHypothesis(exp.locat.comp.rma, c("exp_locationfield - exp_locationgh = 0"))

# Are the effects of experiment location (gh vs. field) on competition of alone vs together studies different from zero? (Supplemental Table S2, rows 45-46) 
exp.locat.comp.alone.together.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = alone.together.data)
summary(exp.locat.comp.alone.together.rma)

# Post-hoc: Do the effects of competition in field exps differ from the effects of competition in gh exps in alone vs together studies? (Supplemental Table S2, row 124) 
linearHypothesis(exp.locat.comp.alone.together.rma, c("exp_locationfield - exp_locationgh = 0"))


# Are the effects of field-conditioned inoc and gh-conditioned inoc on competition different from zero? (Supplemental Table S2, rows 48-49)     
comp.inoc.source.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = competition.data)
summary(comp.inoc.source.rma)

# Post-hoc: Do the effects of field-conditioned inoculum on competition differ from the effects of gh-conditioned inoculum on competition? (Supplemental Table S2, row 126) 
linearHypothesis(comp.inoc.source.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc & gh-conditioned inoc on alone vs together competition different from zero? (Supplemental Table S2, rows 51-52) 
comp.alone.together.inoc.source.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = alone.together.data)
summary(comp.alone.together.inoc.source.rma)

# Post-hoc: Do the effects of field-conditioned inoculum on alone vs together competition differ from the effects of gh-conditioned inoculum on alone vs together competition? (Supplemental Table S2, row 124) 
linearHypothesis(comp.alone.together.inoc.source.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc & gh-conditioned inoc on intraspecific vs interspecific competition different from zero? (Supplemental Table S2, rows 53-54) 
comp.con.hetero.inoc.source.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = con.hetero.data)
summary(comp.con.hetero.inoc.source.rma)

# Post-hoc: Do the effects of field-conditioned inoculum on intraspecific vs interspecific competition differ from the effects of gh-conditioned inoculum on conspecific-heterospecific competition? (Supplemental Table S2, row 128) 
linearHypothesis(comp.con.hetero.inoc.source.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))



########## Effect of STRESS on plant growth (Main effect) ##########

# Are the effects of stress on plant growth different from zero? (Supplemental Table S2, row 56) 
stress.rma <- rma.mv(RII_CSD, RII_CSD_Vi, random = ~ 1 | focal_sp , data = stress.data)
summary(stress.rma)

# Are the effects of different stress manipulations on plant growth different from zero? (Supplemental Table S2, rows 58-63) 
stress.manip.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ type_manip-1, random = ~ 1 | focal_sp, data = stress.data)
summary(stress.manip.rma)

# Post-hoc: Do the effects of each type of stress on plant growth differ from each other? (Supplemental Table S2, rows 130-148) 
linearHypothesis(stress.manip.rma, c("type_manipdrought - type_manipfertilization = 0"))
linearHypothesis(stress.manip.rma, c("type_manipdrought - type_manipgrazing/herbivory = 0"))
linearHypothesis(stress.manip.rma, c("type_manipdrought - type_maniplight = 0"))
linearHypothesis(stress.manip.rma, c("type_manipdrought - type_manipmining = 0"))
linearHypothesis(stress.manip.rma, c("type_manipdrought - type_maniptemperature = 0"))

linearHypothesis(stress.manip.rma, c("type_manipfertilization - type_manipgrazing/herbivory = 0"))
linearHypothesis(stress.manip.rma, c("type_manipfertilization - type_maniplight = 0"))
linearHypothesis(stress.manip.rma, c("type_manipfertilization - type_manipmining = 0"))
linearHypothesis(stress.manip.rma, c("type_manipfertilization - type_maniptemperature = 0"))

linearHypothesis(stress.manip.rma, c("type_manipgrazing/herbivory - type_maniplight = 0"))
linearHypothesis(stress.manip.rma, c("type_manipgrazing/herbivory - type_manipmining = 0"))
linearHypothesis(stress.manip.rma, c("type_manipgrazing/herbivory - type_maniptemperature = 0"))

linearHypothesis(stress.manip.rma, c("type_maniplight - type_manipmining = 0"))
linearHypothesis(stress.manip.rma, c("type_maniplight - type_maniptemperature = 0"))

linearHypothesis(stress.manip.rma, c("type_manipmining - type_maniptemperature = 0"))

# Are the effects of stress on different plant growth forms different from zero? (Supplemental Table S2, rows 65-68) 
stress.plant.growth.form.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ focal_func-1, random = ~ 1 | focal_sp, data = stress.data)
summary(stress.plant.growth.form.rma)

# Post-hoc: Does stress affect plant growth forms differently? (Supplemental Table S2, rows 150-155)   
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcforb - focal_funcgrass = 0"))
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcforb - focal_funcshrub = 0"))
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcforb - focal_functree = 0"))
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcgrass - focal_funcshrub = 0"))
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcgrass - focal_functree = 0"))
linearHypothesis(stress.plant.growth.form.rma, c("focal_funcshrub - focal_functree = 0"))

# Are the effects of experiment location on grazing/herbivory stress different from zero? (field studies only conducted graz/herb only) (Supplemental Table S2, rows 70-71) 
graz.herb.data <- filter(stress.data, type_manip == "grazing/herbivory")
exp.locat.stress.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = graz.herb.data)
summary(exp.locat.stress.rma)

# Post-hoc: Do the effects of grazing/herbivory in field exps differ from the effects of grazing/herbivory in gh exps? (Supplemental Table S2, row 157) 
linearHypothesis(exp.locat.stress.rma, c("exp_locationfield - exp_locationgh = 0"))


# Are the effects of field-conditioned inoc and gh-conditioned inoc on stress different from zero? (Supplemental Table S2, rows 73-74)  
stress.inoc.source.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = stress.data)
summary(stress.inoc.source.rma)

# Post-hoc: Do the effects of field-conditioned inoc on stress differ from the effects of gh-conditioned inoc on stress? (Supplemental Table S2, row 159) 
linearHypothesis(stress.inoc.source.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc and gh-conditioned inoc on fertilization different from zero? (Supplemental Table S2, rows 76-77) 
sd.inoc.source.fert.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = fert.data)
summary(sd.inoc.source.fert.rma)

# Post-hoc: Do the effects of field-conditioned inoc on fertilization differ from the effects of gh-conditioned inoc on fertilization? (Supplemental Table S2, row 160) 
linearHypothesis(sd.inoc.source.fert.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc and gh-conditioned inoc on shade different from zero? (Supplemental Table S2, rows 79-80)   
sd.inoc.source.light.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = light.data)
summary(sd.inoc.source.light.rma)

# Post-hoc: Do the effects of field-conditioned inoc on shade differ from the effects of gh-conditioned inoc on shade? (Supplemental Table S2, row 161) 
linearHypothesis(sd.inoc.source.light.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of field-conditioned inoc and gh-conditioned inoc on mining different from zero?  (Supplemental Table S2, rows 82-83)  
sd.inoc.source.mining.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = mining.data)
summary(sd.inoc.source.mining.rma)

# Post-hoc: Do the effects of field-conditioned inoc on mining differ from the effects of gh-conditioned inoc on mining? (Supplemental Table S2, row 162) 
linearHypothesis(sd.inoc.source.mining.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))



########## Effect of DISTURBANCE on plant growth (Main effect) ##########

# Are the effects of disturbance on plant growth different from zero? (Supplemental Table S2, row 85) 
disturb.rma <- rma.mv(RII_CSD, RII_CSD_Vi, random = ~ 1 | focal_sp , data = disturb.data)
summary(disturb.rma)

# Are the effects of different disturbance manipulations on plant growth different from zero? (Supplemental Table S2, rows 87-88) 
disturb.manip.rma <- rma.mv(RII_CSD, RII_CSD_Vi, mods = ~ type_manip-1, random = ~ 1 | focal_sp, data = disturb.data)
summary(disturb.manip.rma)

# Post-hoc: Do the effects of each type of disturbance on plant growth differ from each other? (Supplemental Table S2, row 164) 
linearHypothesis(disturb.manip.rma, c("type_manipfire - type_maniptornado = 0"))



########## Effect of PSF AND COMPETITION on plant growth (Interactive effect) ##########

# Are the combined effects of competition and PSF on plant growth different from zero? (Supplemental Table S2, row 166) 
psf.by.comp.overall.rma <- rma.mv(RII_Int, RII_Int_Vi, random = ~ 1 | focal_sp, data = competition.data)
summary(psf.by.comp.overall.rma)

#Are the combined effects of different competition types and PSF on plant growth different from zero? (Supplemental Table S2, rows 167-168) 
psf.by.comp.type.manip.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ type_manip - 1, random = ~ 1 | focal_sp, data = competition.data)
summary(psf.by.comp.type.manip.rma)

# Post-hoc: Do the overall effects of alone-together competition and soil on plant growth differ from the overall effects of conspecific-heterospecific competition and soil on plant growth? (Supplemental Table S2, row 233) 
linearHypothesis(psf.by.comp.type.manip.rma, c("type_manipalone_together - type_manipcon_hetero = 0"))

# Are the overall combined effects of different PSF manipulations and competition on plant growth different from zero? (Supplemental Table S2, rows 170-172) 
psf.by.comp.feedback.manip.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ feedback_manip -1, random = ~ 1 | focal_sp, data = competition.data)
summary(psf.by.comp.feedback.manip.rma)

# Post-hoc: Do the overall effects of different PSF manipulations and competition differ from each other? (Supplemental Table S2, rows 225-227) 
linearHypothesis(psf.by.comp.feedback.manip.rma, c("feedback_manipfungicide - feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.feedback.manip.rma, c("feedback_manipfungicide - feedback_manipown_foreign = 0"))
linearHypothesis(psf.by.comp.feedback.manip.rma, c("feedback_maniplive_sterile - feedback_manipown_foreign = 0"))

# Are the overall effects of competition and PSF manipulations on plant growth different from zero? (Supplemental Table S2, rows 174-179) 
psf.by.comp.type.manip.by.feedback.manip.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ type_manip:feedback_manip - 1, random = ~ 1 | focal_sp, data = competition.data)
summary(psf.by.comp.type.manip.by.feedback.manip.rma)

# Post-hoc: Do the overall effects of different PSF manipulations and different competition treatments differ from each other? (Supplemental Table S2, rows 229-243) 
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipfungicide - type_manipalone_together:feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipfungicide - type_manipalone_together:feedback_manipown_foreign = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_maniplive_sterile - type_manipalone_together:feedback_manipown_foreign = 0"))

linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipcon_hetero:feedback_manipfungicide - type_manipcon_hetero:feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipcon_hetero:feedback_manipfungicide - type_manipcon_hetero:feedback_manipown_foreign = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipcon_hetero:feedback_maniplive_sterile - type_manipcon_hetero:feedback_manipown_foreign = 0"))

linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipfungicide - type_manipcon_hetero:feedback_manipfungicide = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipfungicide - type_manipcon_hetero:feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipfungicide - type_manipcon_hetero:feedback_manipown_foreign = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_maniplive_sterile - type_manipcon_hetero:feedback_manipfungicide = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_maniplive_sterile - type_manipcon_hetero:feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_maniplive_sterile - type_manipcon_hetero:feedback_manipown_foreign = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipown_foreign - type_manipcon_hetero:feedback_manipfungicide = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipown_foreign - type_manipcon_hetero:feedback_maniplive_sterile = 0"))
linearHypothesis(psf.by.comp.type.manip.by.feedback.manip.rma, c("type_manipalone_together:feedback_manipown_foreign - type_manipcon_hetero:feedback_manipown_foreign = 0"))

# Are the effects of experiment location on competition x PSF different from zero? (Supplemental Table S2, rows 180-181) 
exp.locat.psf.by.comp.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = competition.data)
summary(exp.locat.psf.by.comp.rma)

# Post-hoc: Do the effects of competition x PSF in field exps differ from the effects of competition x PSF in gh exps? (Supplemental Table S2, row 245) 
linearHypothesis(exp.locat.psf.by.comp.rma, c("exp_locationfield - exp_locationgh = 0"))



# Are the overall effects of soil inoculum conditioning source on competition x PSF different from zero? (Supplemental Table S2, rows 184-185) 
inoc.source.psf.by.comp.overall.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = competition.data)
summary(inoc.source.psf.by.comp.overall.rma)

# Post-hoc: Do the effects of field-conditioned inoc on competition x PSF differ from the effects of gh-conditioned inoc on competition x PSF? (Supplemental Table S2, row 247) 
linearHypothesis(inoc.source.psf.by.comp.overall.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of inoculum conditioning source on alone vs together competition x PSF different from zero? (Supplemental Table S2, rows 187-188) 
inoc.source.psf.by.alone.together.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = alone.together.data)
summary(inoc.source.psf.by.alone.together.rma)

# Post-hoc: Do the effects of field-conditioned inoc on alone vs together competition x PSF differ from the effects of gh-conditioned inoc on alone vs together competition x PSF? (Supplemental Table S2, row 249) 
linearHypothesis(inoc.source.psf.by.alone.together.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of inoculum conditioning source on interspecific vs intraspecific competition x PSF different from zero? (Supplemental Table S2, rows 190-191) 
inoc.source.psf.by.con.hetero.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = con.hetero.data)
summary(inoc.source.psf.by.con.hetero.rma)

# Post-hoc: Do the effects of field-conditioned inoc on interspecific vs intraspecific competition x PSF differ from the effects of gh-conditioned inoc on interspecific vs intraspecific competition x PSF? (Supplemental Table S2, row 250) 
linearHypothesis(inoc.source.psf.by.con.hetero.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))



########## Effect of PSF AND STRESS on plant growth (Interactive effect) ##########

# Are the effects of stress and soil on plant growth different from zero? (Supplemental Table S2, row 193) 
psf.by.stress.overall.rma <- rma.mv(RII_Int, RII_Int_Vi, random = ~ 1 | focal_sp, data = stress.data)
summary(psf.by.stress.overall.rma)

#Are the effects of different stress types and soil on plant growth different from zero? (Supplemental Table S2, rows 195-200) 
psf.by.stress.type.manip.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ type_manip - 1, random = ~ 1 | focal_sp, data = stress.data)
summary(psf.by.stress.type.manip.rma)

# Do the effects of different stress types x PSF differ from each other? (Supplemental Table S2, rows 252-266) 
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipdrought - type_manipfertilization = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipdrought - type_manipgrazing/herbivory = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipdrought - type_maniplight = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipdrought - type_manipmining = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipdrought - type_maniptemperature = 0"))

linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipfertilization - type_manipgrazing/herbivory = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipfertilization - type_maniplight = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipfertilization - type_manipmining = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipfertilization - type_maniptemperature = 0"))

linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipgrazing/herbivory - type_maniplight = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipgrazing/herbivory - type_manipmining = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipgrazing/herbivory - type_maniptemperature = 0"))

linearHypothesis(psf.by.stress.type.manip.rma, c("type_maniplight - type_manipmining = 0"))
linearHypothesis(psf.by.stress.type.manip.rma, c("type_maniplight - type_maniptemperature = 0"))

linearHypothesis(psf.by.stress.type.manip.rma, c("type_manipmining - type_maniptemperature = 0"))


# Are the effects of experiment location on grazing/herbivory x PSF different from zero? (Supplemental Table S2, rows 202-203) 
exp.locat.psf.by.stress.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ exp_location -1, random = ~ 1 | focal_sp , data = graz.herb.data)
summary(exp.locat.psf.by.stress.rma)

# Post-hoc: Do the effects of grazing/herbivory x PSF in field exps differ from the effects of grazing/herbivory x PSF in gh exps? (Supplemental Table S2, row 268) 
linearHypothesis(exp.locat.psf.by.stress.rma, c("exp_locationfield - exp_locationgh = 0"))


# Are the overall effects of soil inoculum conditioning source on stress x PSF different from zero? (Supplemental Table S2, rows 205-206) 
inoc.source.psf.by.stress.overall.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = stress.data)
summary(inoc.source.psf.by.stress.overall.rma)

# Post-hoc: Do the effects of field-conditioned inoc on stress x PSF differ from the effects of gh-conditioned inoc on stress x PSF? (Supplemental Table S2, row 270) 
linearHypothesis(inoc.source.psf.by.stress.overall.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of different inoculum conditioning sources on drought x PSF different from zero? (Supplemental table rows ) 
inoc.source.psf.by.drought.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = drought.data)
summary(inoc.source.psf.by.drought.rma)

# Post-hoc: Do the effects of field-conditioned inoc on drought x PSF differ from the effects of gh-conditioned inoc on drought x PSF? (Supplemental table row 258) 
linearHypothesis(inoc.source.psf.by.drought.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of different inoculum conditioning sources on fertilization x PSF different from zero? (Supplemental Table S2, rows 208-209) 
inoc.source.psf.by.fert.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = fert.data)
summary(inoc.source.psf.by.fert.rma)

# Post-hoc: Do the effects of field-conditioned inoc on fertilization x PSF differ from the effects of gh-conditioned inoc on fertilization x PSF? (Supplemental Table S2, row 272) 
linearHypothesis(inoc.source.psf.by.fert.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))


# Are the effects of different inoculum conditioning sources on shade stress x PSF different from zero? (Supplemental Table S2, rows 211-212) 
inoc.source.psf.by.light.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = light.data)
summary(inoc.source.psf.by.light.rma)

# Post-hoc: Do the effects of field-conditioned inoc on shade x PSF differ from the effects of gh-conditioned inoc on shade x PSF? (Supplemental Table S2, row 274) 
linearHypothesis(inoc.source.psf.by.light.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))

# Are the effects of different inoculum conditioning sources on mining stress x PSF different from zero? (Supplemental Table S2, rows 214-215) 
inoc.source.psf.by.mining.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ inoculum_source-1, random = ~ 1 | focal_sp, data = mining.data)
summary(inoc.source.psf.by.mining.rma)

# Post-hoc: Do the effects of field-conditioned inoc on mining x PSF differ from the effects of gh-conditioned inoc on mining x PSF? (Supplemental Table S2, row 276) 
linearHypothesis(inoc.source.psf.by.mining.rma, c("inoculum_sourceconditioned_in_field - inoculum_sourceconditioned_in_gh = 0"))



########## Effect of PSF AND DISTURBANCE on plant growth (Interactive effect) ##########

# Are the effects of disturbance and PSF on plant growth different from zero? (Supplemental Table S2, row 217) 
psf.by.disturb.overall.rma <- rma.mv(RII_Int, RII_Int_Vi, random = ~ 1 | focal_sp, data = disturb.data)
summary(psf.by.disturb.overall.rma)

#Are the effects of different disturbance types and PSF on plant growth different from zero? (Supplemental Table S2, rows 219-220) 
psf.by.disturb.type.manip.rma <- rma.mv(RII_Int, RII_Int_Vi, mods = ~ type_manip - 1, random = ~ 1 | focal_sp, data = disturb.data)
summary(psf.by.disturb.type.manip.rma)

# Do the effects of different disturbance types x PSF differ from each other? (Supplemental Table S2, row 278) 
linearHypothesis(psf.by.disturb.type.manip.rma, c("type_manipfire - type_maniptornado = 0"))



