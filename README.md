# Random failure analysis

The goal of this project is to make it possible to identify flaky tests by analyzing test failure data from kie-jenkins.
The project consists of 2 parts:
- simple selenium-based java program which crawls all unstable builds of jobs from [PRs folder](https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/) of kie-jenkins
 and scrapes test failure data. For each failure it's saving 5 items: job URL, test class name, test method name, date of failure and stack trace.
- interactive web page report which enables analyzing data scraped by the above script

## Updating the report

Since kie-jenkins is only archiving last 14 days of job runs, it's necessary to periodically (~ once a week) scrape test failure data.
The process of scraping has been automated, everything can be done by just running `./cli.sh` at the root of this project. The script will
1. build and run the scraper program, which outputs all the failures into `frontend/dist/failures.json`
2. builds and runs the front end report (actually a single-page elm application)
3. copies the contents of the `frontend/dist` to the root directory of this repo at `gh-pages` branch

## Working with the report

The report is deployed at [janhrcek.cz/random-failures/](http://janhrcek.cz/random-failures/).
It has two views:

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
