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
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;

public class DialogCommand extends Command {

  private DialogCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("dialog")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .executes(DialogCommand::openDialog)
        .then(
            Commands.literal("open")
                .then(
                    Commands.argument("target", new EasyNPCArgument())
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(DialogCommand::openDialog)
                                .then(
                                    Commands.argument("dialog", StringArgumentType.string())
                                        .executes(DialogCommand::openDialog)))));
  }

  public static int openDialog(CommandContext<CommandSourceStack> context)
      throws CommandSyntaxException {
    CommandSourceStack commandSource = context.getSource();

    ServerPlayer serverPlayer;
    try {
      serverPlayer = EntityArgument.getPlayer(context, "player");
    } catch (Exception e) {
      return sendFailureMessage(
          commandSource, "Failed to get player from context: " + e.getMessage());
    }

    String dialogLabel = "";
    try {
      dialogLabel = StringArgumentType.getString(context, "dialog");
    } catch (Exception ignored) {

    }

    // Verify Player
    if (!serverPlayer.isAlive()) {
      return sendFailureMessage(commandSource, "Player is death!");
    }

    // Verify NPC
    EasyNPC<?> easyNPC = EasyNPCArgument.getEntity(context, "target");

    // Verify dialog data
    if (easyNPC.getEasyNPCDialogData() == null || !easyNPC.getEasyNPCDialogData().hasDialog()) {
      return sendFailureMessage(
          commandSource, "Found no Dialog data for EasyNPC with UUID " + easyNPC.getUUID() + " !");
    }

    // Verify dialog label, if any
    if (!dialogLabel.isEmpty() && !easyNPC.getEasyNPCDialogData().hasDialog(dialogLabel)) {
      return sendFailureMessage(
          commandSource,
          "Found no Dialog with label "
              + dialogLabel
              + " for EasyNPC with UUID "
              + easyNPC.getUUID()
              + "!");
    }

    // Open dialog
    easyNPC.getEasyNPCDialogData().openDialog(serverPlayer, dialogLabel);
    return sendSuccessMessage(
        commandSource,
        "► Open dialog for " + easyNPC + " with " + serverPlayer + " and dialog " + dialogLabel,
        ChatFormatting.GREEN);
  }
}
