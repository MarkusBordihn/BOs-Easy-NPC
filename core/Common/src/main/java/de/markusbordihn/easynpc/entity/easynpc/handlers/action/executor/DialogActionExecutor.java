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

package de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import java.util.UUID;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogActionExecutor {

  protected static final Logger log = LogManager.getLogger(DialogActionExecutor.class);

  private DialogActionExecutor() {}

  public static void openDefaultDialog(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }
    if (dialogData != null) {
      dialogData.openDefaultDialog(serverPlayer);
    } else {
      log.error("No dialog data found for action {}", actionDataEntry);
      serverPlayer.closeContainer();
    }
  }

  public static void openNamedDialog(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    openNamedDialog(actionDataEntry, serverPlayer, dialogData, false);
  }

  public static void openNamedDialogConditional(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    openNamedDialog(actionDataEntry, serverPlayer, dialogData, true);
  }

  private static void openNamedDialog(
      ActionDataEntry actionDataEntry,
      ServerPlayer serverPlayer,
      DialogDataCapable<?> dialogData,
      boolean checkConditions) {
    if (!ActionValidator.validateActionData(actionDataEntry, serverPlayer)) {
      return;
    }

    DialogDataCapable<?> targetDialogData =
        resolveTargetDialogData(actionDataEntry, serverPlayer, dialogData);
    if (targetDialogData == null) {
      return;
    }

    String dialogLabel = actionDataEntry.command();
    if (!ActionValidator.validateNamedDialog(targetDialogData, dialogLabel)) {
      log.error("Unknown dialog label {} for action {}", dialogLabel, actionDataEntry);
      serverPlayer.closeContainer();
      return;
    }

    UUID dialogId = targetDialogData.getDialogId(dialogLabel);
    if (checkConditions) {
      if (!targetDialogData.openDialogIfConditionsMet(serverPlayer, dialogId)) {
        log.debug("Conditions not met for dialog {} of action {}", dialogLabel, actionDataEntry);
      }
    } else {
      targetDialogData.openDialog(serverPlayer, dialogId);
    }
  }

  private static DialogDataCapable<?> resolveTargetDialogData(
      ActionDataEntry actionDataEntry, ServerPlayer serverPlayer, DialogDataCapable<?> dialogData) {
    if (actionDataEntry.targetUUID() == null) {
      return dialogData;
    }

    EasyNPC<?> targetNpc =
        findEasyNPCByUuid((ServerLevel) serverPlayer.level(), actionDataEntry.targetUUID());
    if (targetNpc == null) {
      log.error("Target NPC with UUID {} not found", actionDataEntry.targetUUID());
      serverPlayer.closeContainer();
      return null;
    }
    DialogDataCapable<?> targetDialogData = targetNpc.getEasyNPCDialogData();
    if (targetDialogData == null) {
      log.error("No dialog data found for NPC {}", actionDataEntry.targetUUID());
      serverPlayer.closeContainer();
    }
    return targetDialogData;
  }

  private static EasyNPC<?> findEasyNPCByUuid(ServerLevel level, UUID uuid) {
    Entity entity = level.getEntity(uuid);
    return entity instanceof EasyNPC<?> easyNPC ? easyNPC : null;
  }
}
