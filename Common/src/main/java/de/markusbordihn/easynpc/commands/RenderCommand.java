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
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.RenderHandler;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.Component;

public class RenderCommand {

  private RenderCommand() {
  }

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("render")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("type")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("uuid", UuidArgument.uuid())
                                .suggests(SuggestionProvider::suggestEasyNPCs)
                                .then(
                                    Commands.argument("type", StringArgumentType.string())
                                        .suggests(SuggestionProvider::suggestRenderTypes)
                                        .executes(
                                            commandContext ->
                                                setRenderType(
                                                    commandContext.getSource(),
                                                    UuidArgument.getUuid(commandContext, "uuid"),
                                                    StringArgumentType.getString(
                                                        commandContext, "type"))))))
                .then(
                    Commands.literal("entity")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("uuid", UuidArgument.uuid())
                                .suggests(SuggestionProvider::suggestEasyNPCs)
                                .then(
                                    Commands.argument("entity", StringArgumentType.string())
                                        .suggests(SuggestionProvider::suggestEntityTypes)
                                        .executes(
                                            commandContext ->
                                                setRenderEntityType(
                                                    commandContext.getSource(),
                                                    UuidArgument.getUuid(commandContext, "uuid"),
                                                    StringArgumentType.getString(
                                                        commandContext, "entity")))))));
  }

  private static int setRenderType(CommandSourceStack context, UUID uuid, String type) {
    if (uuid == null || type == null || type.isEmpty()) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    // Set render type.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    RenderType renderType = RenderType.get(type);
    if (!RenderHandler.setRenderType(easyNPC, renderType)) {
      context.sendFailure(
          Component.literal(
              "Error setting render type " + type + " for EasyNPC with UUID " + uuid));
      return 0;
    }

    context.sendSuccess(
        Component.literal("Set render type " + type + " for EasyNPC with UUID " + uuid), true);
    return Command.SINGLE_SUCCESS;
  }

  private static int setRenderEntityType(CommandSourceStack context, UUID uuid, String entity) {
    if (uuid == null || entity == null || entity.isEmpty()) {
      return 0;
    }

    // Check if server player has access to the EasyNPC entity.
    if (!AccessManager.hasAccess(context, uuid)) {
      context.sendFailure(Component.literal("You are not allowed to edit this EasyNPC!"));
      return 0;
    }

    // Set render entity.
    EasyNPC<?> easyNPC = LivingEntityManager.getEasyNPCEntityByUUID(uuid, context.getLevel());
    if (!RenderHandler.setRenderEntity(easyNPC, entity)) {
      context.sendFailure(
          Component.literal(
              "Error setting render entity " + entity + " for EasyNPC with UUID " + uuid));
      return 0;
    }

    context.sendSuccess(
        Component.literal("Set render entity " + entity + " for EasyNPC with UUID " + uuid), true);
    return Command.SINGLE_SUCCESS;
  }
}
