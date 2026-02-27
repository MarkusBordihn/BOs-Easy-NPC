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

import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.client.pose.PoseManager;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.suggestion.PoseSuggestions;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.MoveToPositionGoal;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.phys.Vec3;

public class PoseCommand extends Command {

  private static final String ARG_POSITION = "position";

  private PoseCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("pose")
        .requires(commandSource -> commandSource.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("reset")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                resetPose(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(TYPE_ARG, ResourceLocationArgument.id())
                        .suggests(PoseSuggestions::suggest)
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .executes(
                                    context ->
                                        setPose(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            ResourceLocationArgument.getId(context, TYPE_ARG)))
                                .then(
                                    Commands.argument(ARG_POSITION, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, ARG_POSITION);
                                              return setPoseAtPosition(
                                                  context.getSource(),
                                                  EasyNPCArgument.getEntityWithAccess(
                                                      context, NPC_TARGET_ARG),
                                                  ResourceLocationArgument.getId(context, TYPE_ARG),
                                                  coordinates.getPosition(context.getSource()));
                                            })))));
  }

  private static int resetPose(CommandSourceStack context, EasyNPC<?> easyNPC) {
    PoseManager.resetModelPose(easyNPC);
    return sendSuccessMessage(
        context, "Resetting pose for Easy NPC " + easyNPC.getEntityUUID() + " !");
  }

  private static int setPose(
      CommandSourceStack context, EasyNPC<?> easyNPC, ResourceLocation resourceLocation) {
    // Set pose for Easy NPC (looks up animation and stores pose name)
    if (PoseManager.setModelPose(easyNPC, resourceLocation)) {
      return sendSuccessMessage(
          context,
          "Setting pose " + resourceLocation + " for Easy NPC " + easyNPC.getEntityUUID() + " !");
    } else {
      return sendFailureMessage(
          context,
          "Failed to set pose "
              + resourceLocation
              + " for Easy NPC "
              + easyNPC.getEntityUUID()
              + " !");
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends EasyNPC<?>> int setPoseAtPosition(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      ResourceLocation resourceLocation,
      Vec3 position) {
    if (position == null || position.equals(Vec3.ZERO)) {
      return setPose(context, easyNPC, resourceLocation);
    }

    BlockPos targetPos = new BlockPos((int) position.x, (int) position.y, (int) position.z);
    MoveToPositionGoal<E> moveGoal =
        new MoveToPositionGoal<>(
            (E) easyNPC, targetPos, 1.0, () -> PoseManager.setModelPose(easyNPC, resourceLocation));

    easyNPC.getEntityGoalSelector().addGoal(1, moveGoal);

    return sendSuccessMessage(
        context,
        "NPC "
            + easyNPC.getEntityUUID()
            + " moving to "
            + targetPos
            + " then setting pose "
            + resourceLocation
            + " !");
  }
}
