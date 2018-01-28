package cz.janhrcek.randomfailures;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

public class MergeFailures {

    public static void main(String[] args) throws IOException {
        new MergeFailures().mergeFailures();
    }

    void mergeFailures() throws IOException {
        File[] reportFiles = findReports();
        System.out.println("Merging " + reportFiles.length + " report files");
        Set<TestFailure> mergedFailures = new HashSet<>();

        for (File reportFile : reportFiles) {
            List<TestFailure> failures = readReport(reportFile);
            System.out.println(failures.size() + " failures found in " + reportFile.getName());
            mergedFailures.addAll(failures);
        }

        System.out.println("Found " + mergedFailures.size() + " unique failures in total");

        ScrapeFailures.saveToJson(mergedFailures, new File("elm/dist/failures.json"));
    }

    private List<TestFailure> readReport(File resultsFile) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        TestFailure[] failures = mapper.readValue(resultsFile, TestFailure[].class);
        return Arrays.asList(failures);
    }

    private File[] findReports() {
        File reportsDir = new Config().getReportsDir();
        if (!reportsDir.exists() || !reportsDir.isDirectory()) {
            throw new IllegalArgumentException("reportsDir must be existing directory");
        }

        File[] reports = reportsDir.listFiles((dir, name) -> name.startsWith("results") && name.endsWith(".txt"));
        if (reports == null) {
            throw new IllegalStateException("No reports found in " + reportsDir);
        }
        Arrays.sort(reports); //Sort by failure date thanks to filename format resultsYYYY-MM-DD.txt
        return reports;
    }
}
