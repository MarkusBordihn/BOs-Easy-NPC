package de.markusbordihn.easynpc.debug;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.config.GlobalConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Marker;
import org.apache.logging.log4j.message.Message;

/**
 * Singleton Logger responsible for logging messages and data
 */
public enum Logger {

    INSTANCE;

    public static final org.apache.logging.log4j.Logger LOGGER = LogManager.getLogger(Constants.LOG_NAME);

    public void info(final String message, final Object... args) {
        if (GlobalConfig.ENABLE_LOG_INFO) {
            LOGGER.info(message, args);
        }
    }

    public void warn(final String message, final Object... args) {
        if (GlobalConfig.ENABLE_LOG_WARN) {
            LOGGER.warn(message, args);
        }
    }

    public void warn(final Marker marker, final Message message, final Throwable throwable) {
        if (GlobalConfig.ENABLE_LOG_WARN) {
            LOGGER.warn(marker, message, throwable);
        }
    }

    public void debug(final String message, final Object... args) {
        if (GlobalConfig.ENABLE_LOG_DEBUG) {
            LOGGER.debug(message, args);
        }
    }

    /**
     * Generic error logging method.
     *
     * <p>
     * If one of the arguments is an instance of {@link Throwable}, it is extracted and logged using the appropriate overload.
     * </p>
     *
     * @param message The error message with placeholders if needed (e.g., "Error on {}: {}").
     * @param args    Optional arguments associated with the message.
     */
    public void error(final String message, final Object... args) {
        if (!GlobalConfig.ENABLE_LOG_ERROR) return;

        Throwable throwable = null;
        final Object[] logArgs = new Object[args.length];
        for (int i = 0; i < args.length; i++) {
            if (args[i] instanceof Throwable && throwable == null) {
                throwable = (Throwable) args[i];
                // Replace the argument with an empty string or another placeholder.
                logArgs[i] = "";
            } else {
                logArgs[i] = args[i];
            }
        }
        if (throwable != null) {
            // If an exception is present, log it separately.
            LOGGER.error(message, throwable);
        } else {
            LOGGER.error(message, logArgs);
        }

    }


    public boolean isTraceEnabled() {
        return LOGGER.isTraceEnabled();
    }

    public void trace(final String message, final Object... args) {
        if (LOGGER.isTraceEnabled()) {
            LOGGER.trace(message, args);
        }
    }

    public boolean isDebugEnabled() {
        return LOGGER.isDebugEnabled();
    }
}
