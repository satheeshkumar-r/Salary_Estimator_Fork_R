
# install.packages('pivottabler')

library(ggplot2)
library(pivottabler)

getwd()
setwd('E:/Data Science/01. Projects/02. Salary_Estimator_Fork_R')

df = read.csv('Data_Cleaned.csv')

summary(df)

colnames(df)


### Histogram View
hist(df$Rating)
hist(df$avg_salary)
hist(df$age)
hist(df$desc_len)

### Boxplot  View
boxplot(df$age, df$avg_salary, df$Rating)
boxplot(df$Rating)

### Correlation View
cor(df[,c('age','avg_salary','Rating','desc_len')])

### Heatmap View
col = colorRampPalette(c('pink', 'white', 'red'))(256)
heatmap(cor(df[,c('age','avg_salary','Rating','desc_len','num_comp')]), col = col)

### Barplot View
barplot(sort(table(df$Location), decreasing = TRUE), col = rainbow(length(df$Location)), las = 2, cex.names=0.5 )

df_cat = df[ , c('Location', 'Headquarters', 'Size','Type.of.ownership', 'Industry', 'Sector', 'Revenue', 'company_txt', 'job_state','same_state', 'python_yn', 'R_yn',
             'spark', 'aws', 'excel', 'job_simp', 'seniority')]

colindex = 0
for (col_name in df_cat) 
{
  #barplot(sort(table(col_name), decreasing = TRUE), col = rainbow(length(col_name)), las = 2 )
  
  colindex = colindex + 1  
  xlab_name = colnames(df_cat[colindex])
  df_CharCol = data.frame(sort(table(col_name), decreasing = TRUE))
  
  print( 
    ggplot(data=df_CharCol, aes(x = col_name, y = Freq, fill = col_name )) +
          geom_bar(stat="identity") +
          xlab(xlab_name)+
          ylab('Count')+
          theme(legend.position = "none")+
          theme(axis.text.x = element_text(angle = 90))
      )
}

df_catlong = df[ , c('Location', 'Headquarters', 'company_txt')]

colindex = 0
for (col_name in df_catlong) 
{
  colindex = colindex + 1  
  xlab_name = colnames(df_catlong[colindex])
  df_CharCol = data.frame(sort(table(col_name), decreasing = TRUE))
  df_CharCol  = df_CharCol[1:20 ,]
  
  print( 
    ggplot(data=df_CharCol, aes(x = col_name, y = Freq, fill = col_name )) +
      geom_bar(stat="identity") +
      xlab(xlab_name)+
      ylab('Count')+
      theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = 90))
  )
}

### Pivot table view
pt <- PivotTable$new()
pt$addData(df)
pt$addRowDataGroups('job_simp')
pt$defineCalculation(calculationName='Average salary', summariseExpression='mean(avg_salary)')
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(df)
pt$addRowDataGroups('job_simp')
pt$addRowDataGroups('seniority')
pt$defineCalculation(calculationName='Average salary', summariseExpression='mean(avg_salary)')
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(df)
pt$addRowDataGroups('job_state')
pt$addRowDataGroups('job_simp')
pt$defineCalculation(calculationName='Average salary', summariseExpression='mean(avg_salary)')
pt$sortRowDataGroups(levelNumber = 2,orderBy = "calculation",sortOrder = "desc")
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(df)
pt$addRowDataGroups('job_state')
pt$addRowDataGroups('job_simp')
pt$defineCalculation(calculationName='Average salary', summariseExpression='n()')
pt$sortRowDataGroups(levelNumber = 2,orderBy = "calculation",sortOrder = "desc")
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(df[df$job_simp == 'data scientist',])
pt$addRowDataGroups('job_state')
pt$defineCalculation(calculationName='Average salary', summariseExpression='mean(avg_salary)')
pt$sortRowDataGroups(levelNumber = 1,orderBy = "calculation",sortOrder = "desc")
pt$renderPivot()

pt <- PivotTable$new()
pt$addData(df)
pt$addRowDataGroups('Revenue')
pt$addColumnDataGroups('python_yn')
pt$defineCalculation(calculationName='Average salary', summariseExpression='n()')
pt$renderPivot()

remove(df, df_CharCol, colindex, xlab_name, df_cat, col, col_name, df_catlong, pt )




