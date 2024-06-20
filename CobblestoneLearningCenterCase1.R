##Upload data##
setwd("~/Fall/Fall B/Analytics design and application/Case 1")
studentData = read.csv("student_data.csv")
evalsData = read.csv("evals_data.csv")

##Data frame to work with##
cabblestoneData = merge(studentData, evalsData, by = c("student_id"))
summary(cabblestoneData)
colSums(is.na(cabblestoneData))

##Fix the data##
cabblestoneData$date = as.Date(cabblestoneData$date, format = "%Y-%m-%d")
cabblestoneData[is.na(cabblestoneData$location), "location"] = "intake"



#1:
#Comparison between programs
readingP = lm(score_reading ~ program, data = cabblestoneData )
summary(readingP)

writingP = lm(score_writing ~ program, data = cabblestoneData )
summary(writingP)

mathNoCalcP = lm(score_mathNoCalc ~ program, data = cabblestoneData )
summary(mathNoCalcP)

mathCalcP = lm(score_mathCalc ~ program, data = cabblestoneData )
summary(mathCalcP)

#Comparison between districts
readingD = lm(score_reading ~ district, data = cabblestoneData )
summary(readingD)

writingD = lm(score_writing ~ district, data = cabblestoneData )
summary(writingD)

mathNoCalcD = lm(score_mathNoCalc ~ district, data = cabblestoneData )
summary(mathNoCalcD)

mathCalcD = lm(score_mathCalc ~ district, data = cabblestoneData )
summary(mathCalcD)

#Comparison online vs in-person
onlineVScenter = subset(cabblestoneData, location != "intake") #we dont need intake

readingV = lm(score_reading ~ location, data = onlineVScenter )
summary(readingV)

writingV = lm(score_writing ~ location, data = onlineVScenter )
summary(writingV)

mathNoCalcV = lm(score_mathNoCalc ~ location, data = onlineVScenter )
summary(mathNoCalcV)

mathCalcV = lm(score_mathCalc ~ location, data = onlineVScenter )
summary(mathCalcV)

#2:
library(dplyr) #we need this library
pathData = onlineVScenter %>% 
    arrange(student_id, date)

#CHAT GP HELP
#Group data by 'student_id' and add new columns for the first, second, third and fourth programs
studentPath = pathData %>%
    group_by(student_id) %>%
    summarize(
        first_program = nth(program, 1),
        second_program = nth(program, 2),
        third_program = nth(program, 3),
        fourth_program = nth(program, 4)
        
    ) %>%
    ungroup()

#We want to substitute NA's
colSums(is.na(studentPath)) 
studentPath[is.na(studentPath$second_program), "second_program"] = "did not continue"
studentPath[is.na(studentPath$third_program), "third_program"] = "did not continue"
studentPath[is.na(studentPath$fourth_program), "fourth_program"] = "did not continue"

#Count occurrences of each program in each period
skillsCounts = studentPath %>%
    summarise(
        first_program = sum(first_program == "skills"),
        second_program = sum(second_program == "skills"),
        third_program = sum(third_program == "skills"), 
        fourth_program = sum(fourth_program == "skills")
    )

refreshCounts = studentPath %>%
    summarise(
        first_program = sum(first_program == "refresh"),
        second_program = sum(second_program == "refresh"),
        third_program = sum(third_program == "refresh"), 
        fourth_program = sum(fourth_program == "refresh")
    )

tutoringCounts = studentPath %>%
    summarise(
        first_program = sum(first_program == "tutoring"),
        second_program = sum(second_program == "tutoring"),
        third_program = sum(third_program == "tutoring"), 
        fourth_program = sum(fourth_program == "tutoring")
    )

churnCounts = studentPath %>%
    summarise(
        first_program = sum(first_program == "did not continue"),
        second_program = sum(second_program == "did not continue"),
        third_program = sum(third_program == "did not continue"), 
        fourth_program = sum(fourth_program == "did not continue")
    )

#Combine all count summaries into one table
mostPopularPath = bind_rows(
    skillsCounts %>%
        mutate(program = "skills"),
    refreshCounts %>%
        mutate(program = "refresh"),
    tutoringCounts %>%
        mutate(program = "tutoring"),
    churnCounts %>%
        mutate(program = "did not continue")
)

#Re-order the table
mostPopularPath = mostPopularPath %>%
    select(program, first_program, second_program, third_program, fourth_program)

##Group by student and program to see how many times they took each program
repetitionData = pathData %>%
    group_by(student_id, program) %>%
    summarise(repetitionData = n(), .groups = "drop") %>%
    ungroup()
#Group by program to see how many times each program was taken 
mostRepeatedProgram = repetitionData %>%
    group_by(program) %>%
    summarise(totalRepetition = sum(repetitionData))

#3: 
#On-line number of students tutoring vs in-person tutoring since 2018
cabblestoneDatainYears = cabblestoneData %>%
    mutate(date = lubridate::year(date)) ##change date format to only year

cabblestoneDatainYears = subset(cabblestoneDatainYears, program == "tutoring" & date >= 2018)
#Count number of students for each location for tutoring
locationCounts = cabblestoneDatainYears %>%
    summarise(
        online = sum(location == "online"),
        center = sum(location == "center"),
    )



