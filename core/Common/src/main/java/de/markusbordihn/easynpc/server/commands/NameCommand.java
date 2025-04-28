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
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;

public class NameCommand extends Command {

  private NameCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("name")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(NPC_TARGET_ARGUMENT, EasyNPCArgument.npc())
                        .then(
                            Commands.argument("name", StringArgumentType.string())
                                .executes(
                                    context ->
                                        setName(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARGUMENT),
                                            StringArgumentType.getString(context, "name"))))))
        .then(
            Commands.literal("clear")
                .then(
                    Commands.argument(NPC_TARGET_ARGUMENT, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                clearName(
                                    context.getSource(),
                                    EasyNPCArgument.getEntity(context, NPC_TARGET_ARGUMENT)))));
  }

  private static int setName(CommandSourceStack context, EasyNPC<?> easyNPC, String name) {
    if (easyNPC == null || name == null) {
      return 0;
    }

    easyNPC.getEntity().setCustomName(Component.literal(name));
    return sendSuccessMessage(context, "Set name of " + easyNPC + " to " + name);
  }

  private static int clearName(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }

    easyNPC.getEntity().setCustomName(null);
    return sendSuccessMessage(context, "Cleared name of " + easyNPC);
  }
}
