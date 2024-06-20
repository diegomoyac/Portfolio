setwd("~/Fall/Fall B/Analytics design and application/Case 2")

##Download the data
studentsExperiment = read.csv('students_experiment.csv', stringsAsFactors = T)
evalsExperiment = read.csv('evals_experiment.csv', stringsAsFactors = T)

##Merge on studentId
experimentData = merge(studentsExperiment, evalsExperiment, by = 'student_id')

##Define groups
for (i in 1:nrow(experimentData)) {
    if (experimentData$district[i] == 'St. Paul' || experimentData$district[i] == 'Beaverton') {
        experimentData$group[i] = 0
    } else if (experimentData$district[i] == 'Lake Oswego' || experimentData$district[i] == 'Ridgefield') {
        experimentData$group[i] = 1
    } else if (experimentData$district[i] == 'Camas' || experimentData$district[i] == 'Sherwood') {
        experimentData$group[i] = 2
    } else if (experimentData$district[i] == 'Riverdale' || experimentData$district[i] == 'Wilsonville') {
        experimentData$group[i] = 3
    }
}

experimentData$group = factor(experimentData$group)
#Regressions
regressionReading = lm(score_reading ~ program + group + program * group, data = experimentData)
summary(regressionReading)

regressionWriting = lm(score_writing ~ program + group + program * group, data = experimentData)
summary(regressionWriting)

regressionMathNoCalc = lm(score_mathNoCalc ~ program + group + program * group, data = experimentData)
summary(regressionMathNoCalc)

regressionMathCalc = lm(score_mathCalc ~ program + group + program * group, data = experimentData)
summary(regressionMathCalc)

regressionOverall = lm(score_reading+score_writing+score_mathNoCalc+score_mathCalc ~ program + group + program * group, data = experimentData)
summary(regressionOverall)

#Randomization check1
randomData = subset(experimentData, program == 'intake')
randomCheck = aov(score_reading+score_writing+score_mathNoCalc+score_mathCalc ~ group, data = experimentData)
summary(randomCheck)

randomTukeys = TukeyHSD(randomCheck)
print(randomTukeys)

##Randomization check2 IGNORE THIS!
experimentDistricts = cabblestoneData[cabblestoneData$district %in% c('Beaverton', 'St. Paul', 'Lake Oswego', 'Ridgefield', 'Camas', 'Sherwood', 'Riverdale', 'Wilsonville'), ]
randomCheck = aov(score_reading+score_writing+score_mathNoCalc+score_mathCalc ~ district, data = experimentDistricts)
summary(randomCheck)

randomTukeys = TukeyHSD(randomCheck)
print(randomTukeys)

similarMeans = randomTukeys$district[, "p adj"] > 0.05
districtsFinal = randomTukeys$district[similarMeans, ]

#Regression without Beaverton IGNORE THIS!
regressionNoBeaverton = subset(experimentData, district != 'Beaverton')
regressionOverall = lm(score_reading+score_writing+score_mathNoCalc+score_mathCalc ~ program + group + program * group, data = regressionNoBeaverton)
summary(regressionOverall)

group3Data = subset(experimentData, group == 3)


