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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.SkinHandler;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class SkinCommand extends Command {

  private SkinCommand() {}

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
                            Commands.argument("target", EasyNPCArgument.npc())
                                .then(
                                    Commands.argument("variant", StringArgumentType.string())
                                        .executes(
                                            context ->
                                                setDefaultSkinVariant(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    StringArgumentType.getString(
                                                        context, "variant")))))));
  }

  private static int setDefaultSkinVariant(
      CommandSourceStack context, EasyNPC<?> easyNPC, String variant) {
    if (easyNPC == null || variant == null || variant.isEmpty()) {
      return 0;
    }

    if (!SkinHandler.setDefaultSkin(easyNPC, variant)) {
      return sendFailureMessage(
          context, "Failed to set skin variant " + variant + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Successfully set skin variant " + variant + " for EasyNPC " + easyNPC);
  }
}
