package cz.janhrcek.randomfailures;

import java.io.IOException;

public class Main {

    public static void main(String[] args) throws IOException {
        new ScrapeFailures().scrapeFailuresAndSaveToFile();
        new MergeFailures().mergeFailures();
    }
}
