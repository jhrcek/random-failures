package cz.janhrcek.randomfailures;

import java.io.File;

public class Config {

    public File getReportsDir() {
        String userHome = System.getProperty("user.home");
        return new File(userHome + "/Dropbox/Projects/randomFailuresAnalysis");
    }
}
