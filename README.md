# Random failure analysis

The goal of this project is to make it possible to identify flaky tests by analyzing test failure data from kie-jenkins.
The project consists of 2 parts:
- simple selenium-based java class which crawls all unstable builds of jobs from [PRs folder](https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/) of kie-jenkins and extracts test failure data. For each failure we're saving 5 items: job URL, test class, test method, date of failure and stack trace
- interactive web page report written in elm which enables analyzing data scraped by the above script


## Working with the report

The report has two views:
1. Summary view which shows test failure data grouped by test class & test method.
For each test method the table is showing the following columns (all columns are sortable)
   - Number of failures
   - Standard deviation of failure dates (in days)
   - Number of days since last failure

2. Details view for each test method which shows
   - Basic stats about test method
     - Total failures
     - Number of unique stack traces (including message) = stacktrace & exception message identical
     - Number of unique stack trace (excluding message) = test failed at the same point, but with different message
   - Spread of dates of failure dates
   - Listing of unique stacktraces
