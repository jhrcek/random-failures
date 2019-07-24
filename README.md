# Random failure analysis

The goal of this project is to make it possible to identify flaky tests by aggregating test failure data from RHBA Jenkins.

The project consists of 2 parts:
- A Haskell program which
    - downloads test results of all unstable builds of jobs from [master pullrequests folder](https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest/) of RHBA Jenkins. For each failure it saves 5 items: job URL, test class and test method name, stack trace and date of build.
    - searches for the path of each test class within local filesystem (starting from folder where all kiegroup repositories are cloned) in order to provide GitHub link functionality.
- A single page Elm application which enables browsing data scraped by the above script. This is deployed at [janhrcek.cz/random-failures/](http://janhrcek.cz/random-failures/) and updated with new data on weekly basis.

## Updating the report

RHBA Jenkins is only archiving last 14 days of builds. So it's necessary to periodically (~ once a week) scrape test failure data. The data are persisted in a JSON file, which is then deployed to the gh-pages branch of this repository together with the interactive HTML application for browsing it.

The process of scraping is automated, everything can be done by just running `./cli.sh` at the root of this project. The script will
1. build and run the scraper program, which outputs all the failures into `frontend/dist/failures.json`
2. build and runs the front end report
3. copy the contents of the `frontend/dist` to the root directory of this repo at `gh-pages` branch
4. push the updated report to the `gh-pages` branch
