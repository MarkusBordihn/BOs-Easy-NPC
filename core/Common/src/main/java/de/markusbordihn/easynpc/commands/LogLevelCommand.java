/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.debug.DebugManager;
import java.util.List;
import java.util.Locale;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;

public class LogLevelCommand extends Command {

  private static final String LEVEL_ARG = "level";
  private static final List<String> LOG_LEVEL_NAMES =
      List.of("OFF", "ERROR", "WARN", "INFO", "DEBUG", "TRACE");

  private LogLevelCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register(String loggerName, String modName) {
    return Commands.literal("loglevel")
        .then(
            Commands.argument(LEVEL_ARG, StringArgumentType.word())
                .suggests(
                    (context, builder) ->
                        SharedSuggestionProvider.suggest(LOG_LEVEL_NAMES, builder))
                .executes(
                    context ->
                        setLogLevel(
                            context.getSource(),
                            loggerName,
                            modName,
                            StringArgumentType.getString(context, LEVEL_ARG))));
  }

  private static int setLogLevel(
      CommandSourceStack context, String loggerName, String modName, String levelName) {
    String normalizedLevelName = levelName.toUpperCase(Locale.ROOT);
    if (!LOG_LEVEL_NAMES.contains(normalizedLevelName)) {
      return sendFailureMessage(
          context,
          "Unknown log level " + levelName + ", use one of " + String.join(", ", LOG_LEVEL_NAMES));
    }

    DebugManager.setLogLevel(LogManager.getLogger(loggerName), Level.getLevel(normalizedLevelName));
    return sendSuccessMessage(
        context,
        "Log level for " + modName + " set to " + normalizedLevelName + " until the next restart.",
        ChatFormatting.GREEN);
  }
}
