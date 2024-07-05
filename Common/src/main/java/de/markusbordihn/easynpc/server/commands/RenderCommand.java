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

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.suggestion.EntityTypeSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.RenderTypeSuggestions;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.RenderHandler;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class RenderCommand extends Command {

  private RenderCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("render")
        .requires(commandSource -> commandSource.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("type")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("target", new EasyNPCArgument())
                                .then(
                                    Commands.argument("type", StringArgumentType.string())
                                        .suggests(RenderTypeSuggestions::suggest)
                                        .executes(
                                            context ->
                                                setRenderType(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    RenderType.get(
                                                        StringArgumentType.getString(
                                                            context, "type")))))))
                .then(
                    Commands.literal("entity")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("target", new EasyNPCArgument())
                                .then(
                                    Commands.argument("entity", StringArgumentType.string())
                                        .suggests(EntityTypeSuggestions::suggest)
                                        .executes(
                                            context ->
                                                setRenderEntityType(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    StringArgumentType.getString(
                                                        context, "entity")))))));
  }

  private static int setRenderType(
      CommandSourceStack context, EasyNPC<?> easyNPC, RenderType renderType) {
    if (easyNPC == null || renderType == null) {
      return 0;
    }

    // Set render type.
    if (!RenderHandler.setRenderType(easyNPC, renderType)) {
      return sendFailureMessage(
          context, "Failed to set render type " + renderType + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(context, "Set render type " + renderType + " for EasyNPC " + easyNPC);
  }

  private static int setRenderEntityType(
      CommandSourceStack context, EasyNPC<?> easyNPC, String entity) {
    if (easyNPC == null || entity == null || entity.isEmpty()) {
      return 0;
    }

    // Set render entity.
    if (!RenderHandler.setRenderEntity(easyNPC, entity)) {
      return sendFailureMessage(
          context, "Failed to set render entity " + entity + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Set render entity " + entity + " for EasyNPC with UUID " + easyNPC.getUUID());
  }
}
