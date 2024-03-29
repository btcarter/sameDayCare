# Same Day Care Project

This is a project that was resurrected after David Hedges developed some quite unbelievable models predicting patient readmission with startling accuracy (as in it was never wrong). David was skeptical and reached out to me (a neophyte) for help in discovering why the model was so blindingly accurate (did he stumble upon the holy grail of predictive models in healthcare?). So I have endevoured to reproduce his methods in R, using new data supplied via SQL rather than the hand curated data for the original dataset.

The basic plan is to assess the variables and recreate the same models, with the addition of an natural language processing algorithm to create a new variable representing the reason for admission based upon physician notes. We'll see how this goes.

# Progress Notes

## 2020.10.05

Discovered American Community Survey data. Adding this for geographic inquiries.

## 2020.09.22

Working on EDA.

## 2020.05.18

Pulled the data from servers this morning. Will preprocess and send SDC stuff to David. Had to fix the query again.

## 2020.05.13

Not sure pulling directly from the server will be possible. I may not have permission to do something like that (convenient as that would be).

I have added ICD9 codes, as well as code priorities.

Currently working on adding known pre-existing conditions to the query. At least I am learning shortcut keys quickly. I suppose standing on occasion would be a good idea.

## 2020.05.06

Turns out the data were corrupt. Numerous entries were incomplete. Also, many patients appeared in the dataset multiple times. Some as many as 32 (see nlp.Rmd calculations). So I have begun to rebuild the dataset using SSMS and SQL. This was done using the query found in `2019_query.sql`. I'll find a way to query the database directly via R later.
