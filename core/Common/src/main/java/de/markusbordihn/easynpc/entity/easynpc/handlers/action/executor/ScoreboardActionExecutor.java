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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.scoreboard.ScoreboardOperation;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.ScoreAccess;
import net.minecraft.world.scores.Scoreboard;
import net.minecraft.world.scores.criteria.ObjectiveCriteria;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ScoreboardActionExecutor {

  protected static final Logger log = LogManager.getLogger(ScoreboardActionExecutor.class);

  private ScoreboardActionExecutor() {}

  public static void execute(ActionDataEntry actionDataEntry, ServerPlayer serverPlayer) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      log.warn(
          "Failed to execute scoreboard action for player {}: Invalid action data",
          serverPlayer.getName().getString());
      return;
    }

    String command = actionDataEntry.command();
    if (!ActionValidator.validateCommand(command)) {
      log.warn(
          "Failed to execute scoreboard action for player {}: No command found in action {}",
          serverPlayer.getName().getString(),
          actionDataEntry);
      return;
    }

    String[] parts = command.split(":", 3);
    if (parts.length < 2) {
      log.warn(
          "Failed to execute scoreboard action for player {}: Invalid command format '{}'. Expected format: 'operation:scoreboardName:value'",
          serverPlayer.getName().getString(),
          command);
      return;
    }

    ScoreboardOperation operation = ScoreboardOperation.fromCommandName(parts[0]);
    String scoreboardName = parts[1];
    if (scoreboardName == null || scoreboardName.trim().isEmpty()) {
      log.warn(
          "Failed to execute scoreboard action for player {}: Scoreboard name is empty",
          serverPlayer.getName().getString());
      return;
    }

    int value = 1;
    if (parts.length > 2) {
      try {
        value = Integer.parseInt(parts[2]);
      } catch (NumberFormatException e) {
        log.warn(
            "Failed to execute scoreboard action for player {}: Invalid value '{}'. Value must be an integer",
            serverPlayer.getName().getString(),
            parts[2]);
        return;
      }
    }

    Scoreboard scoreboard = serverPlayer.getScoreboard();
    Objective objective = scoreboard.getObjective(scoreboardName);
    if (objective == null) {
      objective =
          scoreboard.addObjective(
              scoreboardName,
              ObjectiveCriteria.DUMMY,
              Component.literal(scoreboardName),
              ObjectiveCriteria.RenderType.INTEGER,
              false,
              null);
      if (actionDataEntry.enableDebug()) {
        log.debug("Created new scoreboard objective: {}", scoreboardName);
      }
    }

    ScoreAccess scoreAccess = scoreboard.getOrCreatePlayerScore(serverPlayer, objective);
    switch (operation) {
      case INCREASE:
        scoreAccess.add(value);
        if (actionDataEntry.enableDebug()) {
          log.debug(
              "Increased scoreboard {} with {} for player {} by {}",
              scoreboardName,
              scoreAccess.get(),
              serverPlayer.getName().getString(),
              value);
        }
        break;
      case DECREASE:
        scoreAccess.add(-value);
        if (actionDataEntry.enableDebug()) {
          log.debug(
              "Decreased scoreboard {} with {} for player {} by {}",
              scoreboardName,
              scoreAccess.get(),
              serverPlayer.getName().getString(),
              value);
        }
        break;
      case SET:
        scoreAccess.set(value);
        if (actionDataEntry.enableDebug()) {
          log.debug(
              "Set scoreboard {} with {} for player {} to {}",
              scoreboardName,
              scoreAccess.get(),
              serverPlayer.getName().getString(),
              value);
        }
        break;
    }
  }
}
