package cz.janhrcek.randomfailures;

import java.time.LocalDateTime;

public class TestFailure {

    private String url;
    private LocalDateTime date;
    private String testClass;
    private String testMethod;
    private String stackTrace;

    public TestFailure(String url,
            LocalDateTime date,
            String testClass,
            String testMethod,
            String stackTrace) {
        this.url = url;
        this.date = date;
        this.testClass = testClass;
        this.testMethod = testMethod;
        this.stackTrace = stackTrace;
    }

    public String getUrl() {
        return url;
    }

    public LocalDateTime getDate() {
        return date;
    }

    public String getTestClass() {
        return testClass;
    }

    public String getTestMethod() {
        return testMethod;
    }

    public String getStackTrace() {
        return stackTrace;
    }

    @Override
    public String toString() {
        return "Failure {" +
                "\n\turl='" + url + '\'' +
                "\n\tdate='" + date + '\'' +
                //"\n\ttestClass='" + testClass + '\'' +
                //"\n\ttestMethod='" + testMethod + '\'' +
                "\n\tstackTrace='" + stackTrace + '\'' +
                "}\n";
    }
}
