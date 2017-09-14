# Random failure analysis

The goal of this project is to make it possible to identify flaky tests by analyzing test failure data from kie-jenkins.
The project consists of 2 parts:
- simple selenium-based java class which crawls all unstable builds of jobs from [PRs folder](https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/) of kie-jenkins and extracts test failure data. For each failure we're saving 5 items: job URL, test class, test method, date of failure and stack trace
- interactive web page report written in elm which enables analyzing data scraped by the above script

# TODO
- make it possible to show/hide stack trace
- visualize spread of failure dates on a time line
- make it possible to filter failures by restricting date of failure
