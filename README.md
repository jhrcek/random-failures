# Random failure analysis

The goal of this project is to make it possible to identify flaky tests by analyzing test failure data from kie-jenkins.
The project consists of 2 parts:
- simple selenium-based java class which crawls all unstable builds of jobs from [PRs folder](https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/) of kie-jenkins and extracts test failure data. For each failure we're saving 5 items: job URL, test class, test method, date of failure and stack trace
- interactive web page report written in elm which enables analyzing data scraped by the above script


## Working with report

The report shows test failure data grouped by Test class & test method.
The main page shows number of failures and standard deviation of failure Date & Time (in days).
You can *sort table columns* and click Details button to see list of failures of that method sorted by failure date.
Failures time line shows how the failures of given method are spread over date
  - failures whose date of failures are clustered indicate most likely true positive
  - failures whose date of failures are spread over longer period of time are probably random
You can *restrict which data are displayed* by number of failures


# TODO
- visualize spread of failure dates on a time line
- make it possible to filter failures by restricting date of failure
