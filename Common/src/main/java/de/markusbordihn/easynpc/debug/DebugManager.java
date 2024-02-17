/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.debug;

import de.markusbordihn.easynpc.Constants;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;

public class DebugManager {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  protected static final String LOG_PREFIX = "[Debug Manager]";
  protected static boolean isDevelopmentEnvironment = false;

  protected DebugManager() {}

  public static void setLogLevel(Logger logger, Level logLevel) {
    if (logLevel == null || logLevel == logger.getLevel()) {
      return;
    }
    String loggerName = logger.getName();
    LoggerContext context = (LoggerContext) LogManager.getContext(false);
    Configuration config = context.getConfiguration();
    LoggerConfig loggerConfig = config.getLoggerConfig(loggerName);
    LoggerConfig specificConfig = loggerConfig;
    if (!loggerConfig.getName().equals(loggerName)) {
      log.info(
          "{} Add new logger config for {} with level {} ...", LOG_PREFIX, loggerName, logLevel);
      specificConfig = new LoggerConfig(loggerName, logLevel, true);
      specificConfig.setParent(loggerConfig);
      config.addLogger(loggerName, specificConfig);
    } else {
      log.info(
          "{} Changing log level for {} from {} to {}",
          LOG_PREFIX,
          loggerName,
          logger.getLevel(),
          logLevel);
    }
    specificConfig.setLevel(logLevel);
    context.updateLoggers();
  }

  public static void enableDebugLevel(boolean enable) {
    enableDebugLevel(Constants.LOG_NAME, enable);
  }

  public static void enableDebugLevel(String loggerName, boolean enable) {
    Logger logger = LogManager.getLogger(loggerName);
    if (enable) {
      DebugManager.setLogLevel(logger, Level.DEBUG);
    } else {
      DebugManager.setLogLevel(logger, Level.INFO);
    }
  }

  public static Level getLogLevel(String loggerName) {
    Logger logger = LogManager.getLogger(loggerName);
    return logger.getLevel();
  }

  public static boolean isDebugLevel(String loggerName) {
    Level level = getLogLevel(loggerName);
    return level == Level.DEBUG || level == Level.TRACE || level == Level.ALL;
  }

  public static void checkForDebugLogging(String loggerName) {
    if (isDebugLevel(loggerName)) {
      Logger logger = LogManager.getLogger(loggerName);
      String logLevelName = logger.getLevel().name();
      logger.warn(
          "{} Detected debug log level for {} with {}!", LOG_PREFIX, loggerName, logLevelName);
      if (isDevelopmentEnvironment()) {
        logger.info(
            "{} Detected DEV environment, will not change log level for {}!",
            LOG_PREFIX,
            loggerName);
      } else {
        logger.warn(
            "{} Adjusting log level for {} from {} to {}, for performance reasons!",
            LOG_PREFIX,
            loggerName,
            logLevelName,
            Level.INFO);
        enableDebugLevel(loggerName, false);
      }
    }
  }

  public static boolean isDevelopmentEnvironment() {
    return DebugManager.isDevelopmentEnvironment;
  }

  public static void setDevelopmentEnvironment(boolean isDevelopmentEnvironment) {
    DebugManager.isDevelopmentEnvironment = isDevelopmentEnvironment;
  }

  public static boolean isDebugMode() {
    return isDevelopmentEnvironment() || isDebugLevel(Constants.LOG_NAME);
  }
}
