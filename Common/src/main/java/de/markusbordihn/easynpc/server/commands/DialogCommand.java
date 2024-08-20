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
import com.mojang.datafixers.util.Pair;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.DialogArgument;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.server.level.ServerPlayer;

public class DialogCommand extends Command {

  private DialogCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("dialog")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("default")
                        .then(
                            Commands.argument("target", EasyNPCArgument.npc())
                                .then(
                                    Commands.argument("dialog", DialogArgument.uuidOrLabel())
                                        .executes(
                                            context ->
                                                setDefaultDialog(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    DialogArgument.getUuidOrLabel(
                                                        context, "dialog")))))))
        .then(
            Commands.literal("open")
                .then(
                    Commands.argument("target", EasyNPCArgument.npc())
                        .then(
                            Commands.argument("player", EntityArgument.player())
                                .executes(
                                    context ->
                                        openDialog(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(context, "target"),
                                            EntityArgument.getPlayer(context, "player")))
                                .then(
                                    Commands.argument("dialog", DialogArgument.uuidOrLabel())
                                        .executes(
                                            context ->
                                                openDialog(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    EntityArgument.getPlayer(context, "player"),
                                                    DialogArgument.getUuidOrLabel(
                                                        context, "dialog")))))));
  }

  public static int setDefaultDialog(
      CommandSourceStack context, EasyNPC<?> easyNPC, Pair<UUID, String> dialogPair) {
    if (dialogPair.getFirst() != null) {
      return setDefaultDialog(context, easyNPC, dialogPair.getFirst());
    } else if (dialogPair.getSecond() != null) {
      return setDefaultDialog(context, easyNPC, dialogPair.getSecond());
    }
    return sendFailureMessage(context, "Invalid dialog UUID or label!");
  }

  public static int setDefaultDialog(
      CommandSourceStack context, EasyNPC<?> easyNPC, String dialogLabel) {
    // Verify dialog label, if any
    if (!dialogLabel.isEmpty() && !easyNPC.getEasyNPCDialogData().hasDialog(dialogLabel)) {
      return sendFailureMessage(
          context,
          "Found no Dialog with label "
              + dialogLabel
              + " for EasyNPC with UUID "
              + easyNPC.getUUID()
              + "!");
    }
    return setDefaultDialog(
        context, easyNPC, easyNPC.getEasyNPCDialogData().getDialogId(dialogLabel));
  }

  public static int setDefaultDialog(
      CommandSourceStack context, EasyNPC<?> easyNPC, UUID dialogUUID) {

    // Verify dialog data
    if (easyNPC.getEasyNPCDialogData() == null
        || !easyNPC.getEasyNPCDialogData().hasDialog(dialogUUID)) {
      return sendFailureMessageNoDialogData(context, easyNPC);
    }

    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    dialogDataSet.setDefaultDialog(dialogUUID);

    return sendSuccessMessage(
        context, "► Set default dialog for " + easyNPC + " to " + dialogUUID, ChatFormatting.GREEN);
  }

  public static int openDialog(
      CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {

    // Verify Player
    if (!serverPlayer.isAlive()) {
      return sendFailureMessage(context, "Player is death!");
    }

    // Verify dialog data
    if (easyNPC.getEasyNPCDialogData() == null || !easyNPC.getEasyNPCDialogData().hasDialog()) {
      return sendFailureMessageNoDialogData(context, easyNPC);
    }

    // Open dialog
    easyNPC.getEasyNPCDialogData().openDefaultDialog(serverPlayer);
    return sendSuccessMessage(
        context, "► Open dialog for " + easyNPC + " with " + serverPlayer, ChatFormatting.GREEN);
  }

  public static int openDialog(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      Pair<UUID, String> dialogPair) {
    if (dialogPair.getFirst() != null) {
      return openDialog(context, easyNPC, serverPlayer, dialogPair.getFirst());
    } else if (dialogPair.getSecond() != null) {
      return openDialog(context, easyNPC, serverPlayer, dialogPair.getSecond());
    }
    return sendFailureMessage(context, "Invalid dialog UUID or label!");
  }

  public static int openDialog(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      String dialogLabel) {

    // Verify dialog label, if any
    if (!dialogLabel.isEmpty() && !easyNPC.getEasyNPCDialogData().hasDialog(dialogLabel)) {
      return sendFailureMessage(
          context,
          "Found no Dialog with label "
              + dialogLabel
              + " for EasyNPC with UUID "
              + easyNPC.getUUID()
              + "!");
    }
    return openDialog(
        context, easyNPC, serverPlayer, easyNPC.getEasyNPCDialogData().getDialogId(dialogLabel));
  }

  public static int openDialog(
      CommandSourceStack context, EasyNPC<?> easyNPC, ServerPlayer serverPlayer, UUID dialogUUID) {

    // Verify Player
    if (!serverPlayer.isAlive()) {
      return sendFailureMessage(context, "Player is death!");
    }

    // Verify dialog data
    if (easyNPC.getEasyNPCDialogData() == null || !easyNPC.getEasyNPCDialogData().hasDialog()) {
      return sendFailureMessageNoDialogData(context, easyNPC);
    }

    // Verify dialog label, if any
    if (!easyNPC.getEasyNPCDialogData().hasDialog(dialogUUID)) {
      return sendFailureMessage(
          context,
          "Found no Dialog with UUID "
              + dialogUUID
              + " for EasyNPC with UUID "
              + easyNPC.getUUID()
              + "!");
    }

    // Open dialog
    easyNPC.getEasyNPCDialogData().openDialog(serverPlayer, dialogUUID);
    return sendSuccessMessage(
        context,
        "► Open dialog for " + easyNPC + " with " + serverPlayer + " and dialog " + dialogUUID,
        ChatFormatting.GREEN);
  }
}
