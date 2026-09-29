/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.gametest.ServerMessageAssertions.SurvivalOwnerAccess;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeActionEventMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeTradingOfferActionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveDialogButtonMessage;
import de.markusbordihn.easynpc.configui.network.message.server.RemoveDialogMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ResetExecutionLimitMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogButtonMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogMessage;
import de.markusbordihn.easynpc.configui.network.message.server.SaveDialogSetMessage;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import de.markusbordihn.easynpc.data.saveddata.ActionExecutionTracker;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.gametest.GameTestHelpers;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import io.netty.buffer.Unpooled;
import java.util.LinkedHashSet;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public final class DialogActionMessageTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);
  private static final String DIALOG_LABEL = "test_dialog";
  private static final String SAVED_DIALOG_LABEL = "saved_dialog";
  private static final String DIALOG_NAME = "Test Dialog";
  private static final String ORIGINAL_DIALOG_TEXT = "Original dialog text";
  private static final String CHANGED_DIALOG_TEXT = "Changed dialog text";
  private static final String BUTTON_LABEL = "yes_button";
  private static final String BUTTON_NAME = "Yes";
  private static final String CHANGED_BUTTON_NAME = "Agree";
  private static final String COMMAND = "say hello";
  private static final ActionEventType ACTION_EVENT_TYPE = ActionEventType.ON_HURT;
  private static final int OFFER_INDEX = 0;
  private static final int EXECUTION_LIMIT = 1;
  private static final UUID DIALOG_ID = createDialog(DIALOG_LABEL, ORIGINAL_DIALOG_TEXT).getId();
  private static final UUID BUTTON_ID = UUIDUtils.textToUUID(BUTTON_LABEL);
  private static final UUID ACTION_ID = UUIDUtils.textToUUID("test_action");

  private DialogActionMessageTestHelper() {}

  public static void assertDialogSetSave(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new SaveDialogSetMessage(
                uuid, createDialogSet(createDialog(SAVED_DIALOG_LABEL, ORIGINAL_DIALOG_TEXT))),
        SaveDialogSetMessage::create,
        easyNPC -> dialogDataSet(easyNPC).hasDialog(SAVED_DIALOG_LABEL),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertDialogSave(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        DialogActionMessageTestHelper::addDialogWithButton,
        uuid ->
            new SaveDialogMessage(uuid, DIALOG_ID, createDialog(DIALOG_LABEL, CHANGED_DIALOG_TEXT)),
        SaveDialogMessage::create,
        easyNPC -> hasDialogText(easyNPC, CHANGED_DIALOG_TEXT),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertHarmlessDialogButtonSave(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        DialogActionMessageTestHelper::addDialogWithButton,
        uuid ->
            new SaveDialogButtonMessage(
                uuid,
                DIALOG_ID,
                BUTTON_ID,
                createButton(new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG))
                    .withName(CHANGED_BUTTON_NAME)),
        SaveDialogButtonMessage::create,
        DialogActionMessageTestHelper::hasHarmlessButtonChange,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertCommandDialogButtonSaveRequiresCreative(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        DialogActionMessageTestHelper::addDialogWithButton,
        uuid ->
            new SaveDialogButtonMessage(
                uuid, DIALOG_ID, BUTTON_ID, createButton(createCommandAction())),
        SaveDialogButtonMessage::create,
        easyNPC -> buttonHasAction(easyNPC, ActionDataType.COMMAND),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertDialogRemove(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        DialogActionMessageTestHelper::addDialogWithButton,
        uuid -> new RemoveDialogMessage(uuid, DIALOG_ID),
        RemoveDialogMessage::create,
        easyNPC -> !dialogDataSet(easyNPC).hasDialog(DIALOG_ID),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertDialogButtonRemove(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        DialogActionMessageTestHelper::addDialogWithButton,
        uuid -> new RemoveDialogButtonMessage(uuid, DIALOG_ID, BUTTON_ID),
        RemoveDialogButtonMessage::create,
        easyNPC ->
            dialogDataSet(easyNPC).hasDialog(DIALOG_ID)
                && !dialogDataSet(easyNPC).hasDialogButton(DIALOG_ID, BUTTON_ID),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertHarmlessActionEventChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeActionEventMessage(
                uuid,
                ACTION_EVENT_TYPE,
                createActionDataSet(new ActionDataEntry(ActionDataType.CLOSE_DIALOG))),
        ChangeActionEventMessage::create,
        easyNPC -> actionEventHasAction(easyNPC, ActionDataType.CLOSE_DIALOG),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertCommandActionEventChangeRequiresCreative(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeActionEventMessage(
                uuid, ACTION_EVENT_TYPE, createActionDataSet(createCommandAction())),
        ChangeActionEventMessage::create,
        easyNPC -> actionEventHasAction(easyNPC, ActionDataType.COMMAND),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertCommandTradingOfferActionRequiresCreative(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeTradingOfferActionMessage(
                uuid, OFFER_INDEX, createActionDataSet(createCommandAction())),
        ChangeTradingOfferActionMessage::create,
        easyNPC -> tradingOfferHasAction(easyNPC, ActionDataType.COMMAND),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertOwnExecutionLimitReset(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(
            helper, PLAYER_POSITION, "test-execution-limit-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    UUID npcUUID = easyNPC.getEntityUUID();

    assertOnlyOwnExecutionLimitReset(
        helper,
        owner,
        ExecutionId.action(easyNPC.getEntity(), ACTION_ID),
        ResetExecutionLimitMessage.forAction(npcUUID, ACTION_ID, false));
    assertOnlyOwnExecutionLimitReset(
        helper,
        owner,
        ExecutionId.dialog(easyNPC.getEntity(), DIALOG_ID),
        ResetExecutionLimitMessage.forDialog(npcUUID, DIALOG_ID, false));
    assertOnlyOwnExecutionLimitReset(
        helper,
        owner,
        ExecutionId.dialogButton(easyNPC.getEntity(), DIALOG_ID, BUTTON_ID),
        ResetExecutionLimitMessage.forDialogButton(npcUUID, DIALOG_ID, BUTTON_ID, false));
  }

  public static void assertAllPlayersExecutionLimitResetRequiresPermission(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockSurvivalServerPlayer(
            helper, PLAYER_POSITION, "test-execution-limit-owner");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    ActionExecutionTracker tracker = ActionExecutionTracker.get(owner.serverLevel());
    ExecutionId executionId = ExecutionId.action(easyNPC.getEntity(), ACTION_ID);
    UUID otherPlayerUUID = UUID.randomUUID();
    recordBlockingExecution(helper, tracker, owner.getUUID(), executionId);
    recordBlockingExecution(helper, tracker, otherPlayerUUID, executionId);

    receive(ResetExecutionLimitMessage.forAction(easyNPC.getEntityUUID(), ACTION_ID, true), owner);

    GameTestHelpers.assertTrue(
        helper,
        "Execution limit of the sender was reset without permission",
        !canExecute(tracker, owner.getUUID(), executionId));
    GameTestHelpers.assertTrue(
        helper,
        "Execution limit of another player was reset without permission",
        !canExecute(tracker, otherPlayerUUID, executionId));
  }

  public static void assertExecutionLimitResetRequiresAccess(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer stranger =
        GameTestHelpers.mockSurvivalServerPlayer(
            helper, PLAYER_POSITION, "test-execution-limit-stranger");
    ActionExecutionTracker tracker = ActionExecutionTracker.get(stranger.serverLevel());
    ExecutionId executionId = ExecutionId.action(easyNPC.getEntity(), ACTION_ID);
    recordBlockingExecution(helper, tracker, stranger.getUUID(), executionId);

    receive(
        ResetExecutionLimitMessage.forAction(easyNPC.getEntityUUID(), ACTION_ID, false), stranger);

    GameTestHelpers.assertTrue(
        helper,
        "Execution limit was reset without access to the NPC",
        !canExecute(tracker, stranger.getUUID(), executionId));
  }

  private static void assertOnlyOwnExecutionLimitReset(
      GameTestHelper helper,
      ServerPlayer sender,
      ExecutionId executionId,
      ResetExecutionLimitMessage message) {
    ActionExecutionTracker tracker = ActionExecutionTracker.get(sender.serverLevel());
    UUID otherPlayerUUID = UUID.randomUUID();
    recordBlockingExecution(helper, tracker, sender.getUUID(), executionId);
    recordBlockingExecution(helper, tracker, otherPlayerUUID, executionId);

    receive(message, sender);

    GameTestHelpers.assertTrue(
        helper,
        "Own " + executionId.type() + " execution limit was not reset",
        canExecute(tracker, sender.getUUID(), executionId));
    GameTestHelpers.assertTrue(
        helper,
        executionId.type() + " execution limit of another player was reset",
        !canExecute(tracker, otherPlayerUUID, executionId));
  }

  private static void addDialogWithButton(EasyNPC<?> easyNPC) {
    DialogDataEntry dialogDataEntry = createDialog(DIALOG_LABEL, ORIGINAL_DIALOG_TEXT);
    dialogDataEntry.setDialogButton(createButton(new ActionDataEntry(ActionDataType.CLOSE_DIALOG)));
    easyNPC.getEasyNPCDialogData().setDialogDataSet(createDialogSet(dialogDataEntry));
  }

  private static DialogDataEntry createDialog(String label, String text) {
    return new DialogDataEntry(label, DIALOG_NAME, text, new LinkedHashSet<>());
  }

  private static DialogDataSet createDialogSet(DialogDataEntry dialogDataEntry) {
    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    return dialogDataSet;
  }

  private static DialogButtonEntry createButton(ActionDataEntry actionDataEntry) {
    return new DialogButtonEntry(BUTTON_NAME, BUTTON_LABEL, createActionDataSet(actionDataEntry));
  }

  private static ActionDataEntry createCommandAction() {
    return new ActionDataEntry(ActionDataType.COMMAND, COMMAND);
  }

  private static ActionDataSet createActionDataSet(ActionDataEntry actionDataEntry) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(actionDataEntry);
    return actionDataSet;
  }

  private static DialogDataSet dialogDataSet(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCDialogData().getDialogDataSet();
  }

  private static boolean hasDialogText(EasyNPC<?> easyNPC, String text) {
    DialogDataEntry dialogDataEntry = dialogDataSet(easyNPC).getDialog(DIALOG_ID);
    return dialogDataEntry != null && text.equals(dialogDataEntry.getText());
  }

  private static DialogButtonEntry dialogButton(EasyNPC<?> easyNPC) {
    return dialogDataSet(easyNPC).getDialogButton(DIALOG_ID, BUTTON_ID);
  }

  private static boolean hasHarmlessButtonChange(EasyNPC<?> easyNPC) {
    DialogButtonEntry dialogButtonEntry = dialogButton(easyNPC);
    return dialogButtonEntry != null
        && CHANGED_BUTTON_NAME.equals(dialogButtonEntry.name())
        && dialogButtonEntry.actionDataSet().hasActionDataType(ActionDataType.OPEN_DEFAULT_DIALOG);
  }

  private static boolean buttonHasAction(EasyNPC<?> easyNPC, ActionDataType actionDataType) {
    DialogButtonEntry dialogButtonEntry = dialogButton(easyNPC);
    return dialogButtonEntry != null
        && dialogButtonEntry.actionDataSet().hasActionDataType(actionDataType);
  }

  private static boolean actionEventHasAction(EasyNPC<?> easyNPC, ActionDataType actionDataType) {
    return easyNPC
        .getEasyNPCActionEventData()
        .getActionEventSet()
        .getActionEvents(ACTION_EVENT_TYPE)
        .hasActionDataType(actionDataType);
  }

  private static boolean tradingOfferHasAction(EasyNPC<?> easyNPC, ActionDataType actionDataType) {
    return easyNPC
        .getEasyNPCTradingData()
        .getTradingDataSet()
        .getOfferAction(OFFER_INDEX)
        .hasActionDataType(actionDataType);
  }

  private static void recordBlockingExecution(
      GameTestHelper helper,
      ActionExecutionTracker tracker,
      UUID playerUUID,
      ExecutionId executionId) {
    tracker.recordExecution(playerUUID, executionId, ExecutionInterval.LIFETIME);
    GameTestHelpers.assertTrue(
        helper,
        "Recorded execution does not block the execution limit",
        !canExecute(tracker, playerUUID, executionId));
  }

  private static boolean canExecute(
      ActionExecutionTracker tracker, UUID playerUUID, ExecutionId executionId) {
    return tracker.canExecute(playerUUID, executionId, EXECUTION_LIMIT, ExecutionInterval.LIFETIME);
  }

  private static void receive(ResetExecutionLimitMessage message, ServerPlayer sender) {
    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    message.write(buffer);
    ResetExecutionLimitMessage.create(buffer).handleServer(sender);
  }
}
