package cz.janhrcek.randomfailures;

public class UnstableBuild {

    private final String url;
    private final String date;

    public UnstableBuild(String url,
            String date) {
        this.url = url;
        this.date = date;
    }

    public String getUrl() {
        return url;
    }

    public String getDate() {
        return date;
    }
}
