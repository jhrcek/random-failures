package cz.janhrcek.randomfailures;

import static java.util.stream.Collectors.toList;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;

public class FindRandomFailures {

    private static WebDriver driver;
    private static List<TestFailure> failures = new ArrayList<>();
    private static DateTimeFormatter
            DATE_TIME_PARSE_FORMAT = DateTimeFormatter.ofPattern("MMM d, yyyy h:mm a"),
            DATE_TIME_PRINT_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    public static void main(String[] args) throws IOException {
        driver = new ChromeDriver();

        List<String> jobLinks = getMasterPrJobLinks();
        List<UnstableBuild> unstableBuilds = jobLinks.stream().flatMap(jobLink -> getUnstableBuilds(jobLink)).collect(toList());
        unstableBuilds.forEach(url -> {
            try {
                collectTestFailures(url);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });

        Map<String, List<TestFailure>> failuresGroupedByClassAndMethod = failures.stream().collect(
                Collectors.groupingBy(
                        (TestFailure failure) -> failure.getTestClass() + "#" + failure.getTestMethod(),
                        Collectors.toList()
                )
        );

        try (PrintWriter out = new PrintWriter(new OutputStreamWriter(new FileOutputStream("results" + LocalDate.now() + ".txt")))) {
            out.println("----- Failure defails grouped by TestClass#testMethod -----");
            failuresGroupedByClassAndMethod.entrySet()
                    .forEach(entry -> reportFailure(out, entry));
        }

        driver.close();
    }

    private static void reportFailure(PrintWriter out, Map.Entry<String, List<TestFailure>> entry) {
        List<TestFailure> failures = entry.getValue();
        out.println(failures.size() + " failures " + entry.getKey());
        String sortedFailureDates = failures.stream()
                .map(f -> f.getDate())
                .sorted()
                .map(d -> d.format(DATE_TIME_PRINT_FORMAT))
                .collect(Collectors.joining(";", "[", "]"));
        out.println(sortedFailureDates);
        out.println(failures);
    }

    private static List<String> getMasterPrJobLinks() {
        driver.get("https://kie-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/view/PRs/");
        return driver.findElements(By.partialLinkText("-pullrequests")).stream()
                .map(element -> element.getAttribute("href"))
                .filter(href -> href.endsWith("-pullrequests/"))
                .collect(toList()); //Ignore non-master jobs
    }

    public static Stream<UnstableBuild> getUnstableBuilds(String jobUrl) {
        driver.get(jobUrl);
        return driver.findElements(By.className("build-row-cell")).stream()
                .filter(build -> build.findElements(By.cssSelector("img[alt^='Unstable']")).size() > 0)
                .map(unstableBuild -> {
                    WebElement buildLink = unstableBuild.findElement(By.cssSelector("div[time]>a.build-link"));
                    String url = buildLink.getAttribute("href") + "testReport/";
                    String date = buildLink.getText();
                    return new UnstableBuild(url, date);
                });
    }

    private static void collectTestFailures(UnstableBuild unstableBuild) throws IOException {
        String failedTestJsonUrl = unstableBuild.getUrl() + "api/json?tree=suites[cases[className,errorDetails,errorStackTrace,name,status]]";
        driver.get(failedTestJsonUrl);
        String jsonSource = driver.findElement(By.tagName("pre")).getText();
        extractFailedTests(unstableBuild, jsonSource);
    }

    private static void extractFailedTests(UnstableBuild unstableBuild, String inputJson) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        JsonNode root = mapper.readTree(inputJson);
        JsonNode suitesArray = root.path("suites");
        for (int i = 0; i < suitesArray.size(); i++) {
            JsonNode suite = suitesArray.get(i);
            JsonNode casesArray = suite.path("cases");
            for (int j = 0; j < casesArray.size(); j++) {
                JsonNode test = casesArray.get(j);
                String testStatus = test.path("status").asText();

                if ("FAILED".equals(testStatus) || "REGRESSION".equals(testStatus)) {
                    LocalDateTime buildDateTime = LocalDateTime.parse(unstableBuild.getDate(), DATE_TIME_PARSE_FORMAT);
                    failures.add(new TestFailure(unstableBuild.getUrl(),
                                    buildDateTime,
                                    test.path("className").asText(),
                                    test.path("name").asText(),
                                    test.path("errorStackTrace").asText()
                            )
                    );
                }

                dieIfUnknownStatus(testStatus);
            }
        }
    }

    private static void dieIfUnknownStatus(String testStatus) {
        if (!"PASSED|SKIPPED|FAILED|REGRESSION|FIXED".contains(testStatus)) {
            driver.quit();
            System.out.println("Unknown test status: " + testStatus);
            System.exit(1);
        }
    }
}
