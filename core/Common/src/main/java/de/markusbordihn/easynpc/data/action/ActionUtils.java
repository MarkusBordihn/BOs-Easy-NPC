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

  private ActionUtils() {}

  public static String parseAction(String command, LivingEntity entity, ServerPlayer player) {
    if (command == null || command.isEmpty()) {
      return "";
    }
    String output = command;

    // Add slash to command if missing.
    if (!command.startsWith("/")) {
      command = "/" + command;
    }

    // Handle specific short-cuts for commands.
    if (command.startsWith(MACRO_ERROR_MESSAGE)) {
      output = output.replace(MACRO_ERROR_MESSAGE, "").trim();
      output = COMMAND_DISPLAY_TITLE + escapeJson(output) + "\",\"color\":\"dark_red\"}";
    } else if (command.startsWith(MACRO_WARN_MESSAGE)) {
      output = output.replace(MACRO_WARN_MESSAGE, "").trim();
      output = COMMAND_DISPLAY_TITLE + escapeJson(output) + "\",\"color\":\"yellow\"}";
    } else if (command.startsWith(MACRO_INFO_MESSAGE)) {
      output = output.replace(MACRO_INFO_MESSAGE, "").trim();
      output = COMMAND_DISPLAY_TITLE + escapeJson(output) + "\",\"color\":\"aqua\"}";
    } else if (command.startsWith(MACRO_SUCCESS_MESSAGE)) {
      output = output.replace(MACRO_SUCCESS_MESSAGE, "").trim();
      output = COMMAND_DISPLAY_TITLE + escapeJson(output) + "\",\"color\":\"green\"}";
    }

    // Replace NPC macros.
    if (entity != null) {
      output = output.replace(MACRO_NPC_UUID, entity.getUUID().toString());
      output = output.replace(MACRO_NPC, entity.getName().getString());
    }

    // Replace player macros.
    if (player != null) {
      output = output.replace(MACRO_INITIATOR_UUID, player.getUUID().toString());
      output = output.replace(MACRO_INITIATOR, player.getName().getString());

      Matcher matcher = SCORE_PATTERN.matcher(output);
      StringBuilder sb = new StringBuilder();
      while (matcher.find()) {
        String objectiveName = matcher.group(1);
        int score = getScoreboardValue(player, objectiveName);
        matcher.appendReplacement(sb, Matcher.quoteReplacement(String.valueOf(score)));
      }
      matcher.appendTail(sb);
      output = sb.toString();
    }

    return output;
  }

  private static int getScoreboardValue(ServerPlayer player, String objectiveName) {
    if (objectiveName == null || objectiveName.isEmpty() || objectiveName.length() > 16) {
      return 0;
    }
    Scoreboard scoreboard = player.getScoreboard();
    Objective objective = scoreboard.getObjective(objectiveName);
    if (objective != null) {
      return scoreboard.getOrCreatePlayerScore(player, objective).get();
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
