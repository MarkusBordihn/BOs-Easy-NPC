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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.SkinHandler;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.UuidArgument;
import net.minecraft.network.chat.Component;

public class SkinCommand {

  private SkinCommand() {
  }

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("skin")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("variant")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument("uuid", UuidArgument.uuid())
                                .suggests(SuggestionProvider::suggestEasyNPCs)
                                .then(
                                    Commands.argument("variant", StringArgumentType.string())
                                        .suggests(
                                            (context, build) ->
                                                SuggestionProvider.suggestVariants(
                                                    context,
                                                    build,
                                                    UuidArgument.getUuid(context, "uuid")))
                                        .executes(
                                            context ->
                                                setDefaultSkinVariant(
                                                    context.getSource(),
                                                    UuidArgument.getUuid(context, "uuid"),
                                                    StringArgumentType.getString(
                                                        context, "variant")))))));
  }

  private static int setDefaultSkinVariant(CommandSourceStack context, UUID uuid, String variant) {
    if (uuid == null || variant == null || variant.isEmpty()) {
      return 0;
    }

    EasyNPC<?> easyNPC = AccessManager.getEasyNPCEntityByUUID(uuid, context);
    if (easyNPC == null) {
      return 0;
    }

    if (!SkinHandler.setDefaultSkin(easyNPC, variant)) {
      context.sendFailure(
          Component.literal(
              "Error setting skin variant " + variant + " for EasyNPC with UUID " + uuid));
      return 0;
    }

    context.sendSuccess(
        Component.literal("Set skin variant " + variant + " for EasyNPC with UUID " + uuid), true);
    return Command.SINGLE_SUCCESS;
  }
}
