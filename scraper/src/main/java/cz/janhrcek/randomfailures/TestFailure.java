package cz.janhrcek.randomfailures;

import java.time.LocalDateTime;
import java.util.Objects;

public class TestFailure {

    private String url;
    private LocalDateTime date;
    private String testClass;
    private String testMethod;
    private String stackTrace;

    public TestFailure() {
    }

    public TestFailure(String url,
            LocalDateTime date,
            String testClass,
            String testMethod,
            String stackTrace) {
        this.url = Objects.requireNonNull(url);
        this.date = Objects.requireNonNull(date);
        this.testClass = Objects.requireNonNull(testClass);
        this.testMethod = Objects.requireNonNull(testMethod);
        this.stackTrace = Objects.requireNonNull(stackTrace);
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
                "\n\ttestClass='" + testClass + '\'' +
                "\n\ttestMethod='" + testMethod + '\'' +
                "\n\tstackTrace='" + stackTrace + '\'' +
                "}\n";
    }

    @Override public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        TestFailure that = (TestFailure) o;

        if (!url.equals(that.url)) {
            return false;
        }
        if (!date.equals(that.date)) {
            return false;
        }
        if (!testClass.equals(that.testClass)) {
            return false;
        }
        if (!testMethod.equals(that.testMethod)) {
            return false;
        }
        return stackTrace.equals(that.stackTrace);
    }

    @Override public int hashCode() {
        int result = url.hashCode();
        result = 31 * result + date.hashCode();
        result = 31 * result + testClass.hashCode();
        result = 31 * result + testMethod.hashCode();
        result = 31 * result + stackTrace.hashCode();
        return result;
    }

    public void setUrl(String url) {
        this.url = Objects.requireNonNull(url);
    }

    public void setDate(LocalDateTime date) {
        this.date = Objects.requireNonNull(date);
    }

    public void setTestClass(String testClass) {
        this.testClass = Objects.requireNonNull(testClass);
    }

    public void setTestMethod(String testMethod) {
        this.testMethod = Objects.requireNonNull(testMethod);
    }

    public void setStackTrace(String stackTrace) {
        this.stackTrace = Objects.requireNonNull(stackTrace);
    }
}
