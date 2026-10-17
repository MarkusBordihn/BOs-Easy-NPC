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

package de.markusbordihn.easynpc.server.commands.objectives;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class ShoulderObjective extends Command {

  private static final String START_DISTANCE_ARG = "startDistance";
  private static final String COOLDOWN_TICKS_ARG = "cooldownTicks";
  private static final String ONLY_OUT_OF_COMBAT_ARG = "onlyOutOfCombat";

  private ShoulderObjective() {}

  public static ArgumentBuilder<CommandSourceStack, ?> registerSet() {
    return Commands.literal(ObjectiveType.SIT_ON_OWNER_SHOULDER.getFriendlyName())
        .executes(
            context ->
                setSitOnOwnerShoulder(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                    ObjectiveDataEntry.DEFAULT_START_DISTANCE,
                    ObjectiveDataEntry.DEFAULT_COOLDOWN_TICKS,
                    false))
        .then(
            Commands.argument(START_DISTANCE_ARG, FloatArgumentType.floatArg(0))
                .executes(
                    context ->
                        setSitOnOwnerShoulder(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            FloatArgumentType.getFloat(context, START_DISTANCE_ARG),
                            ObjectiveDataEntry.DEFAULT_COOLDOWN_TICKS,
                            false))
                .then(
                    Commands.argument(COOLDOWN_TICKS_ARG, IntegerArgumentType.integer(0))
                        .executes(
                            context ->
                                setSitOnOwnerShoulder(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    FloatArgumentType.getFloat(context, START_DISTANCE_ARG),
                                    IntegerArgumentType.getInteger(context, COOLDOWN_TICKS_ARG),
                                    false))
                        .then(
                            Commands.argument(ONLY_OUT_OF_COMBAT_ARG, BoolArgumentType.bool())
                                .executes(
                                    context ->
                                        setSitOnOwnerShoulder(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            FloatArgumentType.getFloat(context, START_DISTANCE_ARG),
                                            IntegerArgumentType.getInteger(
                                                context, COOLDOWN_TICKS_ARG),
                                            BoolArgumentType.getBool(
                                                context, ONLY_OUT_OF_COMBAT_ARG))))));
  }

  public static int setSitOnOwnerShoulder(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      float startDistance,
      int cooldownTicks,
      boolean onlyOutOfCombat) {
    ObjectiveDataEntry currentObjectiveDataEntry =
        EasyNPCObjectiveHandler.getObjective(
            easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER.name());
    if (currentObjectiveDataEntry == null) {
      return sendFailureMessage(
          context,
          "Sitting on the owner's shoulder is disabled for "
              + easyNPC
              + ", enable it first with /easy_npc shoulder enable!");
    }

    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER)
            .setPriority(currentObjectiveDataEntry.getPriority())
            .setStartDistance(startDistance)
            .setCooldownTicks(cooldownTicks)
            .setOnlyOutOfCombat(onlyOutOfCombat);
    if (!EasyNPCObjectiveHandler.addObjective(easyNPC, objectiveDataEntry)) {
      return sendFailureMessage(context, "Error updating owner shoulder objective!");
    }

    return sendSuccessMessage(
        context,
        "Owner shoulder objective for "
            + easyNPC
            + " set to start distance "
            + startDistance
            + ", cooldown "
            + cooldownTicks
            + " ticks and only out of combat "
            + onlyOutOfCombat);
  }
}
