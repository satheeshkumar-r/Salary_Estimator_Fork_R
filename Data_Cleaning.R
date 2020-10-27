
# install.packages('sjmisc')   # Collection of miscellaneous utility functions, supporting datatransformation tasks like recoding, dichotomizing or grouping variables,setting and replacing missing values. The data transformation functionsalso support labelled data, and all integrate seamlessly into a'tidyverse'-workflow.
# install.packages('stringr')   

library(sjmisc)
library(stringr)

getwd()
setwd('E:/Data Science/01. Projects/02. Salary_Estimator_Fork_R')

df = read.csv('glassdoor_jobs.csv')
#head(df)

### Salary Parsing

df$hourly = lapply(df$Salary.Estimate, function(x){ifelse(str_contains(x,'per hour',ignore.case = TRUE) == TRUE, 1, 0 )}) 
df$employer_provided = lapply(df$Salary.Estimate, function(x){ifelse(str_contains(x,'employer provided salary:',ignore.case = TRUE) == TRUE, 1, 0 )}) 
# nrow(df[df$hourly == 1, ])             
# nrow(df[df$employer_provided == 1, ])  

df = df[df$Salary.Estimate != '-1', ]
# nrow(df)

salary = str_split_fixed(df$Salary.Estimate, '[(]', 2)[,1]
salary = str_replace_all(c(salary),'Employer Provided Salary:','')
salary = str_replace_all(c(salary),'Per Hour','')
salary = str_replace_all(salary, '[$]', '')
salary = str_replace_all(salary, 'K','')
salary =  str_trim(salary)

df$min_salary = str_split_fixed(salary, '[-]', 2)[,1]
df$max_salary = str_split_fixed(salary, '[-]', 2)[,2]

df$min_salary = as.numeric(df$min_salary)
df$max_salary = as.numeric(df$max_salary)
df$avg_salary = apply(df[,18:19], 1, mean) 

### Company Parsing
df$company_txt = str_split_fixed(df$Company.Name, '\n', 2)[,1]

### Location Parsing
df$job_state = str_trim(str_split_fixed(df$Location, ',', 2)[,2])
table(df$job_state)

df$same_state = ifelse(df$Location == df$Headquarters, 1, 0 )

### Age of Company Parsing
df$age = lapply(df$Founded, function(x){ifelse( x < 1 , x , 2020-x ) })

### Job Description Parsing (python, etc.)
df$python_yn = lapply(df$Job.Description, function(x){ifelse(str_contains(x,'python',ignore.case = TRUE) == TRUE, 1, 0 )}) 
table(as.character(df$python_yn))

df$R_yn = lapply(df$Job.Description, function(x){ifelse(str_contains(x,'r studio',ignore.case = TRUE) == TRUE | str_contains(x,'r-studio',ignore.case = TRUE), 1, 0)})
table(as.character(df$R_yn))

df$spark = lapply(df$Job.Description, function(x){ifelse(str_contains(x,'spark',ignore.case = TRUE),1,0)})
table(as.character(df$spark))

df$aws = lapply(df$Job.Description, function(x){ifelse(str_contains(x,'aws',ignore.case = TRUE),1,0)})
table(as.character(df$aws))

df$excel = lapply(df$Job.Description, function(x){ifelse(str_contains(x,'excel',ignore.case = TRUE),1,0)})
table(as.character(df$excel))  

### Deleting Index Column
colnames(df)
df = df[-c(1)]
  
### Title Parsing    
Title_Simplifier = function(x)
  {
    if      (str_contains(x,'data scientist', ignore.case = TRUE) == TRUE) { return ('data scientist')}
    else if (str_contains(x,'data engineer', ignore.case = TRUE) == TRUE) { return ('data engineer')}
    else if (str_contains(x,'analyst', ignore.case = TRUE) == TRUE) { return ('analyst')}
    else if (str_contains(x,'machine learning', ignore.case = TRUE) == TRUE) { return ('mle')}
    else if (str_contains(x,'manager', ignore.case = TRUE) == TRUE) { return ('manager')}
    else if (str_contains(x,'director', ignore.case = TRUE) == TRUE) { return ('director')}
    else    { return ('na')}
  }

df$job_simp = lapply(df$Job.Title, Title_Simplifier) 
table(as.character(df$job_simp))

### Seniority Parsing
Job_Seniority = function(x)
{
  if(  str_contains(x,'senior', ignore.case = TRUE) == TRUE || 
        str_contains(x,'sr', ignore.case = TRUE) == TRUE ||
        str_contains(x,'lead', ignore.case = TRUE) == TRUE ||
        str_contains(x,'principal', ignore.case = TRUE) == TRUE )
  { return ('senior')}
  else if ( str_contains(x,'jr', ignore.case = TRUE) == TRUE || 
            str_contains(x,'jr.', ignore.case = TRUE) == TRUE  )
  { return ('jr')}
  else { return('na') }
}

df$seniority = lapply(df$Job.Title, Job_Seniority) 
table(as.character(df$seniority))

### Fix state Los Angeles 
df$job_state =  str_replace_all(df$job_state,'Los Angeles, ','')
table(df$job_state)

### Job Description Parsing
df$desc_len = str_length(df$Job.Description)

### Competitor Parsing
Comp_Count = function(x)
{
  if      (x == -1) {return(0)}
  else if (str_contains(x,',') != TRUE) {return (1)}
  else {lengths(gregexpr(',', x))+1}
}

df$num_comp = lapply(df$Competitors, Comp_Count) 

### Hourly Wage Parsing
df$min_salary = apply(df, 1,function(x){ ifelse(x$hourly == 1, x$min_salary*2, x$min_salary) })
df$max_salary = apply(df, 1,function(x){ ifelse(x$hourly == 1, x$max_salary*2, x$max_salary) })

df[ df$hourly ==1 , c('hourly','min_salary', 'max_salary')]  

remove(df, salary, Comp_Count, Job_Seniority, Title_Simplifier)
       
       
              