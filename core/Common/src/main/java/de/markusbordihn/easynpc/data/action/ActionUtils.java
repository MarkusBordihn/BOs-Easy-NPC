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

package de.markusbordihn.easynpc.data.action;

import java.util.Map;
import java.util.function.ToIntFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;

public class ActionUtils {

  public static final String COMMAND_DISPLAY_TITLE = "/title @initiator title {\"text\":\"";
  public static final String MACRO_ERROR_MESSAGE = "/error_message";
  public static final String MACRO_INFO_MESSAGE = "/info_message";
  public static final String MACRO_INITIATOR = "@initiator";
  public static final String MACRO_INITIATOR_UUID = "@initiator-uuid";
  public static final String MACRO_NPC = "@npc";
  public static final String MACRO_NPC_UUID = "@npc-uuid";
  public static final String MACRO_SUCCESS_MESSAGE = "/success_message";
  public static final String MACRO_WARN_MESSAGE = "/warn_message";
  private static final Pattern SCORE_PATTERN = Pattern.compile("@score\\(([a-zA-Z0-9_.-]+)\\)");
  private static final Map<String, String> TITLE_MACRO_COLORS =
      Map.of(
          MACRO_ERROR_MESSAGE, "dark_red",
          MACRO_WARN_MESSAGE, "yellow",
          MACRO_INFO_MESSAGE, "aqua",
          MACRO_SUCCESS_MESSAGE, "green");

  private ActionUtils() {}

  public static String parseAction(String command, LivingEntity entity, ServerPlayer player) {
    if (command == null || command.isEmpty()) {
      return "";
    }

    String output = command;

    if (!command.startsWith("/")) {
      command = "/" + command;
    }

    for (Map.Entry<String, String> titleMacroColor : TITLE_MACRO_COLORS.entrySet()) {
      String titleMacro = titleMacroColor.getKey();
      if (command.startsWith(titleMacro)) {
        output =
            COMMAND_DISPLAY_TITLE
                + escapeJson(output.replace(titleMacro, "").trim())
                + "\",\"color\":\""
                + titleMacroColor.getValue()
                + "\"}";
        break;
      }
    }

    return parseMacros(output, entity, player);
  }

  public static String parseMacros(String text, LivingEntity entity, ServerPlayer player) {
    if (text == null || text.isEmpty()) {
      return "";
    }

    String output = text;

    if (entity != null) {
      output = output.replace(MACRO_NPC_UUID, entity.getUUID().toString());
      output = output.replace(MACRO_NPC, entity.getName().getString());
    }

    if (player != null) {
      output = output.replace(MACRO_INITIATOR_UUID, player.getUUID().toString());
      output = output.replace(MACRO_INITIATOR, player.getName().getString());

      output =
          replaceScoreMacros(output, objectiveName -> getScoreboardValue(player, objectiveName));
    }

    return output;
  }

  public static String replaceScoreMacros(String text, ToIntFunction<String> scoreByObjectiveName) {
    Matcher matcher = SCORE_PATTERN.matcher(text);
    StringBuilder replacedText = new StringBuilder();
    while (matcher.find()) {
      int score = scoreByObjectiveName.applyAsInt(matcher.group(1));
      matcher.appendReplacement(replacedText, Matcher.quoteReplacement(String.valueOf(score)));
    }
    matcher.appendTail(replacedText);
    return replacedText.toString();
  }

  private static int getScoreboardValue(ServerPlayer player, String objectiveName) {
    if (objectiveName == null || objectiveName.isEmpty() || objectiveName.length() > 16) {
      return 0;
    }

    Scoreboard scoreboard = player.getScoreboard();
    Objective objective = scoreboard.getObjective(objectiveName);
    if (objective != null && scoreboard.hasPlayerScore(player.getScoreboardName(), objective)) {
      return scoreboard.getOrCreatePlayerScore(player.getScoreboardName(), objective).getScore();
    }

    return 0;
  }

  private static String escapeJson(String text) {
    if (text == null || text.isEmpty()) {
      return text;
    }

    return text.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t");
  }
}
