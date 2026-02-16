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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.progression.ProgressionLevelMap;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ProgressionDataCapable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class ProgressionCommand extends Command {

  private static final String LEVEL_ARG = "level";
  private static final String XP_ARG = "xp";
  private static final String ENABLED_ARG = "enabled";

  private ProgressionCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("progression")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("get")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                getProgression(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.literal("level")
                                .then(
                                    Commands.argument(
                                            LEVEL_ARG,
                                            IntegerArgumentType.integer(
                                                ProgressionLevelMap.MIN_LEVEL,
                                                ProgressionLevelMap.MAX_LEVEL))
                                        .executes(
                                            context ->
                                                setLevel(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    IntegerArgumentType.getInteger(
                                                        context, LEVEL_ARG)))))
                        .then(
                            Commands.literal("xp")
                                .then(
                                    Commands.argument(XP_ARG, IntegerArgumentType.integer(1))
                                        .executes(
                                            context ->
                                                setExperience(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    IntegerArgumentType.getInteger(
                                                        context, XP_ARG)))))))
        .then(
            Commands.literal("add")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.literal("xp")
                                .then(
                                    Commands.argument(XP_ARG, IntegerArgumentType.integer())
                                        .executes(
                                            context ->
                                                addExperience(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    IntegerArgumentType.getInteger(
                                                        context, XP_ARG)))))))
        .then(
            Commands.literal("scaling")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(ENABLED_ARG, BoolArgumentType.bool())
                                .executes(
                                    context ->
                                        setAttributeScaling(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            BoolArgumentType.getBool(context, ENABLED_ARG))))));
  }

  private static int getProgression(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) return 0;

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null) {
      return sendFailureMessage(context, "No progression data available for " + easyNPC);
    }

    return sendSuccessMessage(
        context,
        String.format(
            "%s: Level %d | XP: %d/%d (%d, %.1f%%) | Attribute Scaling: %s",
            easyNPC,
            progressionData.getExperienceLevel(),
            progressionData.getExperience(),
            progressionData.getExperienceForNextLevel(),
            progressionData.getExperienceProgressToNextLevel(),
            progressionData.getProgressPercentageToNextLevel(),
            progressionData.isAttributeScalingEnabled() ? "Enabled" : "Disabled"));
  }

  private static int setLevel(CommandSourceStack context, EasyNPC<?> easyNPC, int level) {
    if (easyNPC == null) return 0;

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null) {
      return sendFailureMessage(context, "No progression data available for " + easyNPC);
    }

    int oldLevel = progressionData.getExperienceLevel();
    progressionData.setExperienceLevel(level);

    return sendSuccessMessage(
        context,
        String.format(
            "%s level changed from %d to %d (XP: %d)",
            easyNPC, oldLevel, level, progressionData.getExperience()));
  }

  private static int setExperience(CommandSourceStack context, EasyNPC<?> easyNPC, int xp) {
    if (easyNPC == null) {
      return sendFailureMessage(context, "No NPC found for the given target");
    }

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null) {
      return sendFailureMessage(context, "No progression data available for " + easyNPC);
    }

    int oldXp = progressionData.getExperience();
    int oldLevel = progressionData.getExperienceLevel();
    progressionData.setExperience(xp);
    int newLevel = progressionData.getExperienceLevel();

    String message = String.format("%s XP changed from %d to %d", easyNPC, oldXp, xp);
    if (oldLevel != newLevel) {
      message += String.format(" (Level: %d → %d)", oldLevel, newLevel);
    }

    return sendSuccessMessage(context, message);
  }

  private static int addExperience(CommandSourceStack context, EasyNPC<?> easyNPC, int xpAmount) {
    if (easyNPC == null) return 0;

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null) {
      return sendFailureMessage(context, "No progression data available for " + easyNPC);
    }

    int oldXp = progressionData.getExperience();
    int oldLevel = progressionData.getExperienceLevel();
    progressionData.addExperience(xpAmount);

    String message =
        String.format(
            "%s %d XP to %s (%d → %d)",
            xpAmount >= 0 ? "Added" : "Removed",
            Math.abs(xpAmount),
            easyNPC,
            oldXp,
            progressionData.getExperience());

    int newLevel = progressionData.getExperienceLevel();
    if (oldLevel != newLevel) {
      message += String.format(" | Level: %d → %d", oldLevel, newLevel);
    }

    return sendSuccessMessage(context, message);
  }

  private static int setAttributeScaling(
      CommandSourceStack context, EasyNPC<?> easyNPC, boolean enabled) {
    if (easyNPC == null) return 0;

    ProgressionDataCapable<?> progressionData = easyNPC.getEasyNPCProgressionData();
    if (progressionData == null) {
      return sendFailureMessage(context, "No progression data available for " + easyNPC);
    }

    progressionData.setAttributeScalingEnabled(enabled);

    return sendSuccessMessage(
        context,
        String.format(
            "%s attribute scaling %s (Level: %d)",
            easyNPC, enabled ? "enabled" : "disabled", progressionData.getExperienceLevel()));
  }
}
