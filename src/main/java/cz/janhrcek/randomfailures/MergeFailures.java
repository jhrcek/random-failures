package cz.janhrcek.randomfailures;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

public class MergeFailures {

    public static void main(String[] args) throws IOException {
        File[] reportFiles = findReports();
        System.out.println("Going to merge " + reportFiles.length + " report files");

        Set<TestFailure> mergedFailures = new HashSet<>();

        for (File reportFile : reportFiles) {
            List<TestFailure> failures = readReport(reportFile);
            System.out.println(failures.size() + " failures found in " + reportFile.getName());
            mergedFailures.addAll(failures);
        }

        System.out.println("Found " + mergedFailures.size() + " unique failures in total");

        Map<String, Long> failureCountsGroupedByClassAndMethod = mergedFailures.stream().collect(
                Collectors.groupingBy(
                        (TestFailure failure) -> failure.getTestClass() + "#" + failure.getTestMethod(),
                        Collectors.counting()
                )
        );

        failureCountsGroupedByClassAndMethod.entrySet()
                //.stream().filter(e -> e.getValue() > 1).collect(toSet())
                .forEach(e -> System.out.println(e.getValue() + " failures " + e.getKey()));

        Map<String, List<TestFailure>> failuresGroupedByClassAndMethod = mergedFailures.stream().collect(
                Collectors.groupingBy(
                        (TestFailure failure) -> failure.getTestClass() + "#" + failure.getTestMethod(),
                        Collectors.toList()
                )
        );

        //System.out.println(failuresGroupedByClassAndMethod);
    }

    private static List<TestFailure> readReport(File resultsFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        TestFailure[] failures = mapper.readValue(resultsFile, TestFailure[].class);
        return Arrays.asList(failures);
    }

    private static File[] findReports() {
        File reportsDir = new File("/home/jhrcek/Dropbox/Projects/randomFailuresAnalysis");
        if (!reportsDir.exists() || !reportsDir.isDirectory()) {
            throw new IllegalArgumentException("reportsDir must be existing directory");
        }

        return reportsDir.listFiles((dir, name) -> name.startsWith("results") && name.endsWith(".txt"));
    }
}
