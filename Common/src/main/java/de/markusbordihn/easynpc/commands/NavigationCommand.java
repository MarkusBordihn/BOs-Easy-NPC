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

package de.markusbordihn.easynpc.commands;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.commands.arguments.coordinates.Coordinates;
import net.minecraft.commands.arguments.coordinates.Vec3Argument;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.phys.Vec3;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class NavigationCommand {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

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
                            Commands.argument("uuid", UuidArgument.uuid())
                                .suggests(SuggestionProvider::suggestEasyNPCs)
                                .then(
                                    Commands.argument(ARG_POSITION, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, ARG_POSITION);
                                              Vec3 position =
                                                  coordinates.getPosition(context.getSource());
                                              return setHomePosition(
                                                  context.getSource(),
                                                  UuidArgument.getUuid(context, "uuid"),
                                                  position);
                                            }))))
                .then(
                    Commands.literal("pos")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("uuid", UuidArgument.uuid())
                                .suggests(SuggestionProvider::suggestEasyNPCs)
                                .then(
                                    Commands.argument(ARG_POSITION, Vec3Argument.vec3())
                                        .executes(
                                            context -> {
                                              Coordinates coordinates =
                                                  Vec3Argument.getCoordinates(
                                                      context, ARG_POSITION);
                                              Vec3 position =
                                                  coordinates.getPosition(context.getSource());
                                              return setPosition(
                                                  context.getSource(),
                                                  UuidArgument.getUuid(context, "uuid"),
                                                  position);
                                            })))))
        .then(
            Commands.literal("reset")
                .requires(
                    commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                .then(
                    Commands.argument("uuid", UuidArgument.uuid())
                        .suggests(SuggestionProvider::suggestEasyNPCs)
                        .executes(
                            context ->
                                reset(
                                    context.getSource(), UuidArgument.getUuid(context, "uuid")))));
  }

  private static int setHomePosition(CommandSourceStack context, UUID uuid, Vec3 position) {
    if (uuid == null || position == null || position.equals(Vec3.ZERO)) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    // Check for navigation data
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      context.sendFailure(Component.literal("No navigation data available for " + easyNPC));
      return 0;
    }

    // Set home position for EasyNPC entity by UUID.
    BlockPos blockPos = new BlockPos(position);
    log.info("Set home position for EasyNPC {} with UUID {} to {}...", easyNPC, uuid, blockPos);
    navigationData.setHomePosition(blockPos);

    return Command.SINGLE_SUCCESS;
  }

  private static int setPosition(CommandSourceStack context, UUID uuid, Vec3 position) {
    if (uuid == null || position == null || position.equals(Vec3.ZERO)) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    // Check for navigation data
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    NavigationData<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      context.sendFailure(Component.literal("No navigation data available for " + easyNPC));
      return 0;
    }

    // Set home position for EasyNPC entity by UUID.
    log.info("Set position for EasyNPC {} with UUID {} to {}...", easyNPC, uuid, position);
    navigationData.setPosition(position);

    return Command.SINGLE_SUCCESS;
  }

  private static int reset(CommandSourceStack context, UUID uuid) {
    if (uuid == null) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    return Command.SINGLE_SUCCESS;
  }
}
