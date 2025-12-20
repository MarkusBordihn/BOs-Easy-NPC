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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.datafixers.util.Pair;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.DialogArgument;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogPriority;
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
                    Commands.literal("priority")
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(DIALOG_ARG, DialogArgument.uuidOrLabel())
                                        .then(
                                            Commands.argument(
                                                    "priority", IntegerArgumentType.integer())
                                                .executes(
                                                    context ->
                                                        setPriority(
                                                            context.getSource(),
                                                            EasyNPCArgument.getEntityWithAccess(
                                                                context, NPC_TARGET_ARG),
                                                            DialogArgument.getUuidOrLabel(
                                                                context, DIALOG_ARG),
                                                            IntegerArgumentType.getInteger(
                                                                context, "priority"))))))))
        .then(
            Commands.literal("open")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(PLAYER_ARG, EntityArgument.player())
                                .executes(
                                    context ->
                                        openDialog(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            EntityArgument.getPlayer(context, PLAYER_ARG)))
                                .then(
                                    Commands.argument(DIALOG_ARG, DialogArgument.uuidOrLabel())
                                        .executes(
                                            context ->
                                                openDialog(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    EntityArgument.getPlayer(context, PLAYER_ARG),
                                                    DialogArgument.getUuidOrLabel(
                                                        context, DIALOG_ARG)))))))
        .then(
            Commands.literal("close")
                .then(
                    Commands.argument(PLAYER_ARG, EntityArgument.player())
                        .executes(
                            context ->
                                closeDialog(
                                    context.getSource(),
                                    EntityArgument.getPlayer(context, PLAYER_ARG)))));
  }

  public static int setPriority(
      CommandSourceStack context, EasyNPC<?> easyNPC, Pair<UUID, String> dialogPair, int priority) {
    if (dialogPair.getFirst() != null) {
      return setPriority(context, easyNPC, dialogPair.getFirst(), priority);
    } else if (dialogPair.getSecond() != null) {
      return setPriority(context, easyNPC, dialogPair.getSecond(), priority);
    }
    return sendFailureMessage(context, "Invalid dialog UUID or label!");
  }

  public static int setPriority(
      CommandSourceStack context, EasyNPC<?> easyNPC, String dialogLabel, int priority) {
    if (!dialogLabel.isEmpty() && !easyNPC.getEasyNPCDialogData().hasDialog(dialogLabel)) {
      return sendFailureMessage(
          context,
          "Found no Dialog with label "
              + dialogLabel
              + " for EasyNPC with UUID "
              + easyNPC.getEntityUUID()
              + "!");
    }
    return setPriority(
        context, easyNPC, easyNPC.getEasyNPCDialogData().getDialogId(dialogLabel), priority);
  }

  public static int setPriority(
      CommandSourceStack context, EasyNPC<?> easyNPC, UUID dialogUUID, int priority) {

    if (easyNPC.getEasyNPCDialogData() == null
        || !easyNPC.getEasyNPCDialogData().hasDialog(dialogUUID)) {
      return sendFailureMessageNoDialogData(context, easyNPC);
    }

    DialogDataSet dialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    DialogDataEntry dialog = dialogDataSet.getDialog(dialogUUID);
    if (dialog != null) {
      dialog.setPriority(priority);
      return sendSuccessMessage(
          context,
          "► Set priority for dialog "
              + dialog.getLabel()
              + " to "
              + priority
              + " ("
              + DialogPriority.getNameForPriority(priority)
              + ")",
          ChatFormatting.GREEN);
    }

    return sendFailureMessage(context, "Dialog not found!");
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
              + easyNPC.getEntityUUID()
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
              + easyNPC.getEntityUUID()
              + "!");
    }

    // Open dialog
    easyNPC.getEasyNPCDialogData().openDialog(serverPlayer, dialogUUID);
    return sendSuccessMessage(
        context,
        "► Open dialog for " + easyNPC + " with " + serverPlayer + " and dialog " + dialogUUID,
        ChatFormatting.GREEN);
  }

  public static int closeDialog(CommandSourceStack context, ServerPlayer serverPlayer) {
    // Close dialog screen (client side)
    serverPlayer.closeContainer();

    return sendSuccessMessage(
        context, "► Closed dialog screen for player " + serverPlayer, ChatFormatting.YELLOW);
  }
}
