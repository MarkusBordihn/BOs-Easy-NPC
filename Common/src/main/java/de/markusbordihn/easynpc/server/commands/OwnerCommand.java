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
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;

public class OwnerCommand extends Command {
  private static final String EASY_NPC_PREFIX = "EasyNPC ";

  private OwnerCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("owner")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument("target", new EasyNPCArgument())
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(
                                    context ->
                                        setOwner(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(context, "target"),
                                            EntityArgument.getPlayer(context, "player"))))))
        .then(
            Commands.literal("get")
                .then(
                    Commands.argument("target", new EasyNPCArgument())
                        .executes(
                            context ->
                                getOwner(
                                    context.getSource(),
                                    EasyNPCArgument.getEntity(context, "target")))));
  }

  private static int setOwner(
      CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    if (easyNPC == null || serverPlayer == null) {
      return 0;
    }

    // Set owner data for EasyNPC entity.
    if (!OwnerHandler.setOwner(easyNPC, serverPlayer)) {
      sendFailureMessage(context, "Failed to set owner for " + easyNPC);
    }

    return sendSuccessMessage(context, "Owner of " + easyNPC + " was changed to " + serverPlayer);
  }

  private static int getOwner(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return 0;
    }

    // Get owner data for EasyNPC entity.
    OwnerData<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null) {
      return sendFailureMessageNoOwnerData(context, easyNPC);
    }

    context.sendSuccess(
        new TextComponent(
            EASY_NPC_PREFIX + easyNPC.getUUID() + " is owned by " + ownerData.getOwner()),
        false);
    return Command.SINGLE_SUCCESS;
  }
}
