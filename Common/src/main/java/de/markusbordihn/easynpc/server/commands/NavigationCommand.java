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
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.core.BlockPos;
import net.minecraft.world.phys.Vec3;

public class NavigationCommand extends Command {

  private static final String ARG_POSITION = "position";

  private NavigationCommand() {
  }

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("navigation")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("home")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("target", new EasyNPCArgument())
                                .then(
                                    Commands.argument(ARG_POSITION, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, ARG_POSITION);
                                              return setHomePosition(
                                                  context.getSource(),
                                                  EasyNPCArgument.getEntityWithAccess(
                                                      context, "target"),
                                                  coordinates.getPosition(context.getSource()));
                                            }))))
                .then(
                    Commands.literal("pos")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("target", new EasyNPCArgument())
                                .then(
                                    Commands.argument(ARG_POSITION, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, ARG_POSITION);
                                              return setPosition(
                                                  context.getSource(),
                                                  EasyNPCArgument.getEntityWithAccess(
                                                      context, "target"),
                                                  coordinates.getPosition(context.getSource()));
                                            })))))
        .then(
            Commands.literal("reset")
                .requires(
                    commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                .then(
                    Commands.argument("target", new EasyNPCArgument())
                        .executes(
                            context ->
                                reset(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, "target")))));
  }

  private static int setHomePosition(
      CommandSourceStack context, EasyNPC<?> easyNPC, Vec3 position) {
    if (easyNPC == null || position == null || position.equals(Vec3.ZERO)) {
      return Command.FAILURE;
    }

    // Check for navigation data
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      return sendFailureMessageNoNavigationData(context, easyNPC);
    }

    // Set home position for EasyNPC entity by UUID.
    BlockPos blockPos = new BlockPos(position);
    navigationData.setHomePosition(blockPos);
    return sendSuccessMessage(
        context,
        "Set home position for EasyNPC "
            + easyNPC
            + " with UUID "
            + easyNPC.getUUID()
            + " to "
            + blockPos);
  }

  private static int setPosition(CommandSourceStack context, EasyNPC<?> easyNPC, Vec3 position) {
    if (easyNPC == null || position == null || position.equals(Vec3.ZERO)) {
      return 0;
    }

    // Check for navigation data
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      return sendFailureMessageNoNavigationData(context, easyNPC);
    }

    // Set home position for EasyNPC entity by UUID.
    navigationData.setPosition(position);
    return sendSuccessMessage(
        context,
        "Set position for EasyNPC "
            + easyNPC
            + " with UUID "
            + easyNPC.getUUID()
            + " to "
            + position);
  }

  private static int reset(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }

    // Check for navigation data
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      return sendFailureMessageNoNavigationData(context, easyNPC);
    }

    // Reset navigation for EasyNPC entity by UUID.
    navigationData.getGroundPathNavigation().recomputePath();
    return sendSuccessMessage(context, "Reset navigation for EasyNPC " + easyNPC);
  }
}
