package de.markusbordihn.easynpc.config;

import java.io.File;
import java.util.Properties;

public class GlobalConfig extends Config {

    public static final String CONFIG_FILE_NAME = "config.cfg";

    public static final boolean ENABLE_LOG_INFO = false;
    public static final boolean ENABLE_LOG_WARN = false;
    public static final boolean ENABLE_LOG_ERROR = true;

    public static final boolean ENABLE_LOG_DEBUG = false;

    public static void registerConfig() {
        registerConfigFile(CONFIG_FILE_NAME, "Global EasyNPC config");
        parseConfigFile();
    }

    public static void parseConfigFile() {
        final File configFile = getConfigFile(CONFIG_FILE_NAME);
        final Properties properties = readConfigFile(configFile);
        final Properties unmodifiedProperties = (Properties) properties.clone();

        // Update config file if needed
        updateConfigFileIfChanged(configFile, "Global EasyNPC config", properties, unmodifiedProperties);
    }
}
