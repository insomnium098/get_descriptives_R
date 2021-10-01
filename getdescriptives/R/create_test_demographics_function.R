#Load Neuroblu package
library(NeuroBlu)

#The user defines cohort_name
cohort_name = "Cohort For Onboarding Task"

#Get patient IDs from the desired cohort (Note: The cohort must be in cohort builder)
cohort<-getCohort(cohort_name)

#Define
demographics_df<-function(cohort){
  #This function takes in a cohort name and gets the corresponding patient IDs to extract
  ##demographic variables and returns a dataframe ready to use un get_descriptives()

  #Extract demographic variables
  birth_year = getBirthYear(cohort) #Birth year
  gender = getGender(cohort) #Gender
  city = getCity(cohort) #City
  education_years = getEducationYears(cohort) #Education years
  employment = getEmployment(cohort) #Employment
  ethnicity = getEthnicity(cohort) #Ethnicity
  marital = getMarital(cohort) #Marital status
  race = getRace(cohort) #Race
  state = getState(cohort) #State

  #List of variables to merge
  demographics <- list(birth_year, gender, city, education_years, employment, ethnicity,
                       marital, race, state)

  #Merge all remaining variables
  for (i in demographics){
    i <- as.data.frame(i) #Change lists to dataframes
    if(!(exists("df_final"))){
      df_final<-i
    } else {
      df_final<-merge(df_final, i, by="person_id", all=TRUE)
    }
  }
  df_final <- df_final[,-c(1)] #Remove column 'person_id', which is the first in the dataframe
  return(df_final)
}

#Try demographics_df()
x<-demographics_df(cohort)
head(x)

