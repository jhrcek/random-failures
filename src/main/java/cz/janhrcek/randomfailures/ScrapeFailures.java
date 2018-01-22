package cz.janhrcek.randomfailures;

import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

import static java.util.stream.Collectors.toList;

public class ScrapeFailures {

    private WebDriver driver;
    private final Set<TestFailure> allFailures = new HashSet<>();
    private static final DateTimeFormatter DATE_TIME_PARSE_FORMAT = DateTimeFormatter.ofPattern("MMM d, yyyy h:mm a");

    public static void main(String[] args) throws IOException {
        new ScrapeFailures().scrapeFailuresAndSaveToFile();
    }

    public void scrapeFailuresAndSaveToFile() throws IOException {
        driver = new ChromeDriver();

        List<String> jobLinks = getMasterPrJobLinks();
        List<UnstableBuild> unstableBuilds = jobLinks.stream()
                .flatMap(this::getUnstableBuilds)
                .collect(toList());

        System.out.println(unstableBuilds.size() + " unstable builds to scrape");

        unstableBuilds.forEach(url -> {
            try {
                collectTestFailures(url);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });

        saveFailuresToFile(allFailures);

        driver.close();
    }

    private List<String> getMasterPrJobLinks() {
        driver.get("https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/");
        return driver.findElements(By.partialLinkText("-pullrequests")).stream()
                .map(element -> element.getAttribute("href"))
                .filter(href -> href.endsWith("-pullrequests/"))
                .collect(toList()); //Ignore non-master jobs
    }

    private Stream<UnstableBuild> getUnstableBuilds(String jobUrl) {
        driver.get(jobUrl);
        return driver.findElements(By.className("build-row-cell")).stream()
                .filter(build -> build.findElements(By.cssSelector("img[alt^='Unstable']")).size() > 0)
                .map(unstableBuild -> {
                    WebElement buildLink = unstableBuild.findElement(By.cssSelector("div[time]>a.build-link"));
                    String url = buildLink.getAttribute("href") + "testReport/";
                    System.out.println("Unstable build: " + url);
                    String date = buildLink.getText();
                    return new UnstableBuild(url, date);
                });
    }

    private void collectTestFailures(UnstableBuild unstableBuild) throws IOException {
        String failedTestJsonUrl = unstableBuild.getUrl() + "api/json?tree=suites[cases[className,errorDetails,errorStackTrace,name,status]]";
        driver.get(failedTestJsonUrl);
        String jsonSource = driver.findElement(By.tagName("pre")).getText();
        extractFailedTests(unstableBuild, jsonSource);
    }

    private void extractFailedTests(UnstableBuild unstableBuild, String inputJson) throws IOException {
        List<TestFailure> failuresInBuild = new ArrayList<>();
        ObjectMapper mapper = new ObjectMapper();
        JsonNode root = mapper.readTree(inputJson);
        JsonNode suitesArray = root.path("suites");
        for (int i = 0; i < suitesArray.size(); i++) {
            JsonNode suite = suitesArray.get(i);
            JsonNode casesArray = suite.path("cases");
            for (int j = 0; j < casesArray.size(); j++) {
                JsonNode test = casesArray.get(j);
                String testStatus = test.path("status").asText();

                // Possible status values seem to be : PASSED, SKIPPED, FAILED, REGRESSION, FIXED
                if ("FAILED".equals(testStatus) || "REGRESSION".equals(testStatus)) {
                    LocalDateTime buildDateTime = LocalDateTime.parse(unstableBuild.getDate(), DATE_TIME_PARSE_FORMAT);
                    failuresInBuild.add(new TestFailure(unstableBuild.getUrl(),
                                                        buildDateTime,
                                                        test.path("className").asText(),
                                                        test.path("name").asText(),
                                                        test.path("errorStackTrace").asText()
                                        )
                    );
                }
            }
        }

        if (failuresInBuild.size() > 50) {
            System.out.printf("WARNING: ignoring build with more than 50 failures (%d) in %s%n", failuresInBuild.size(), unstableBuild.getUrl());
        } else {
            allFailures.addAll(failuresInBuild);
        }
    }

    private void saveFailuresToFile(Set<TestFailure> failures) throws IOException {
        File outputFile = getOutputFileName();
        saveToJson(failures, outputFile);
    }

    private File getOutputFileName() {
        String filename = "results" + LocalDate.now() + ".txt";
        return new File(new Config().getReportsDir(), filename);
    }

    static void saveToJson(Set<TestFailure> failures, File outputFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.writeValue(outputFile, failures);
    }
}
