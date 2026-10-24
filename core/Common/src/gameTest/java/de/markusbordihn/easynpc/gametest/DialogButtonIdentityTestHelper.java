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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.DurationType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonReference;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.execution.ExecutionId;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetType;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.ActionValidator;
import de.markusbordihn.easynpc.handler.PresetHandler;
import de.markusbordihn.easynpc.handler.ReloadHandler;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;
import net.minecraft.world.scores.criteria.ObjectiveCriteria;

public class DialogButtonIdentityTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final String GAMETEST_OBJECTIVE = "easy_npc_gametest_bank";
  private static final String BANK_OBJECTIVE = "easy_npc_bank";
  private static final Identifier BANK_TELLER_PRESET =
      Identifier.fromNamespaceAndPath(
          Constants.MOD_ID, "default_preset/villager/bank_teller.npc.snbt");

  private DialogButtonIdentityTestHelper() {}

  public static void assertEqualButtonLabelsLockPerDialog(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "lock-gametest-player");
    setScore(helper, serverPlayer, GAMETEST_OBJECTIVE, 0);

    DialogButtonEntry depositButton = scoreboardButton(ConditionOperationType.LESS_THAN, 100);
    DialogButtonEntry withdrawButton =
        scoreboardButton(ConditionOperationType.GREATER_THAN_OR_EQUALS, 5);
    DialogDataEntry depositDialog = dialogEntry("deposit", depositButton);
    DialogDataEntry withdrawDialog = dialogEntry("withdraw", withdrawButton);
    DialogDataSet dialogDataSet = dialogDataSet(depositDialog, withdrawDialog);
    GameTestHelpers.assertEquals(
        helper,
        "Equal labels must still share the button id",
        depositButton.id(),
        withdrawButton.id());

    CompoundTag lockTag = new CompoundTag();
    AdditionalScreenData.addDialogButtonLockData(lockTag, dialogDataSet, serverPlayer, easyNPC);
    Set<DialogButtonReference> lockedButtons = AdditionalScreenData.getLockedDialogButtons(lockTag);

    GameTestHelpers.assertTrue(
        helper,
        "The withdraw button without balance must be locked",
        lockedButtons.contains(reference(withdrawDialog, withdrawButton)));
    GameTestHelpers.assertTrue(
        helper,
        "The deposit button with the same label must stay available",
        !lockedButtons.contains(reference(depositDialog, depositButton)));
  }

  public static void assertEqualButtonLabelsReachExecutionLimitPerDialog(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "limit-gametest-player");

    DialogButtonEntry firstButton = oncePerDayButton();
    DialogButtonEntry secondButton = oncePerDayButton();
    DialogDataEntry firstDialog = dialogEntry("first_dialog", firstButton);
    DialogDataEntry secondDialog = dialogEntry("second_dialog", secondButton);
    ConditionManager.recordExecutions(
        firstButton.conditions(),
        serverPlayer,
        ExecutionId.dialogButton(easyNPC.getEntity(), firstDialog.getId(), firstButton.id()));

    CompoundTag limitTag = new CompoundTag();
    AdditionalScreenData.addExecutionLimitData(
        limitTag, dialogDataSet(firstDialog, secondDialog), serverPlayer, easyNPC);
    Set<DialogButtonReference> exhaustedButtons =
        AdditionalScreenData.getLockedExecutionLimitButtons(limitTag);

    GameTestHelpers.assertTrue(
        helper,
        "The used button must reach its execution limit",
        exhaustedButtons.contains(reference(firstDialog, firstButton)));
    GameTestHelpers.assertTrue(
        helper,
        "The unused button with the same label must stay available",
        !exhaustedButtons.contains(reference(secondDialog, secondButton)));
  }

  public static void assertEqualActionsReachExecutionLimitPerButton(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.assertNotNull(
        helper, "NPC must support dialogs", easyNPC.getEasyNPCDialogData());
    ActionDataEntry oncePerDayAction =
        new ActionDataEntry(ActionDataType.COMMAND, "/say reward")
            .withConditionDataSet(new ConditionDataSet(oncePerDayConditions()));
    easyNPC
        .getEasyNPCDialogData()
        .setDialogDataSet(
            dialogDataSet(
                dialogEntry("first_dialog", actionButton(oncePerDayAction)),
                dialogEntry("second_dialog", actionButton(oncePerDayAction))));

    GameTestHelpers.assertTrue(
        helper, "The NPC must reload", ReloadHandler.reloadNPC(easyNPC, helper.getLevel()));
    DialogDataSet reloadedDialogDataSet = easyNPC.getEasyNPCDialogData().getDialogDataSet();
    ActionDataEntry firstAction = firstActionOf(reloadedDialogDataSet, "first_dialog");
    ActionDataEntry secondAction = firstActionOf(reloadedDialogDataSet, "second_dialog");
    GameTestHelpers.assertNotNull(helper, "The first dialog must keep its action", firstAction);
    GameTestHelpers.assertNotNull(helper, "The second dialog must keep its action", secondAction);

    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "action-limit-gametest-player");
    ConditionManager.recordExecutions(
        firstAction.conditionDataSet().getConditions(),
        serverPlayer,
        ExecutionId.action(easyNPC.getEntity(), firstAction.id()));

    GameTestHelpers.assertTrue(
        helper,
        "The used action must reach its execution limit",
        !ActionValidator.validateActionData(firstAction, serverPlayer, easyNPC.getMob()));
    GameTestHelpers.assertTrue(
        helper,
        "The equal action of the other dialog must stay available",
        ActionValidator.validateActionData(secondAction, serverPlayer, easyNPC.getMob()));
  }

  public static void assertBankTellerLocksWithdrawalsByBalance(GameTestHelper helper) {
    EasyNPC<?> bankTeller = importBankTeller(helper);
    if (bankTeller == null) {
      return;
    }

    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "bank-gametest-player");
    DialogDataSet dialogDataSet = bankTeller.getEasyNPCDialogData().getDialogDataSet();

    setScore(helper, serverPlayer, BANK_OBJECTIVE, 0);
    assertBankButtonLocked(
        helper, dialogDataSet, serverPlayer, bankTeller, "withdraw", "five", true);
    assertBankButtonLocked(
        helper, dialogDataSet, serverPlayer, bankTeller, "deposit", "five", false);

    setScore(helper, serverPlayer, BANK_OBJECTIVE, 5);
    assertBankButtonLocked(
        helper, dialogDataSet, serverPlayer, bankTeller, "withdraw", "five", false);
    assertBankButtonLocked(
        helper, dialogDataSet, serverPlayer, bankTeller, "withdraw", "ten", true);
    assertBankButtonLocked(
        helper, dialogDataSet, serverPlayer, bankTeller, "deposit", "ten", false);
  }

  private static void assertBankButtonLocked(
      GameTestHelper helper,
      DialogDataSet dialogDataSet,
      ServerPlayer serverPlayer,
      EasyNPC<?> bankTeller,
      String dialogLabel,
      String buttonLabel,
      boolean expectedLocked) {
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog(dialogLabel);
    GameTestHelpers.assertNotNull(
        helper, "The bank teller preset has no dialog " + dialogLabel, dialogDataEntry);
    DialogButtonEntry dialogButtonEntry = dialogDataEntry.getDialogButton(buttonLabel);
    GameTestHelpers.assertNotNull(
        helper,
        "The bank teller dialog " + dialogLabel + " has no button " + buttonLabel,
        dialogButtonEntry);

    CompoundTag lockTag = new CompoundTag();
    AdditionalScreenData.addDialogButtonLockData(lockTag, dialogDataSet, serverPlayer, bankTeller);
    GameTestHelpers.assertEquals(
        helper,
        "Lock state of " + dialogLabel + "/" + buttonLabel,
        expectedLocked,
        AdditionalScreenData.getLockedDialogButtons(lockTag)
            .contains(reference(dialogDataEntry, dialogButtonEntry)));
  }

  private static EasyNPC<?> importBankTeller(GameTestHelper helper) {
    CompoundTag presetTag =
        PresetHandler.loadPresetCompoundTag(
            PresetType.DEFAULT, BANK_TELLER_PRESET, helper.getLevel().getServer());
    if (presetTag == null) {
      helper.fail("Unable to load the bank teller preset");
      return null;
    }

    CompoundTag entityData = presetTag.getCompoundOrEmpty(PresetData.DATA_TAG);
    EntityType<?> entityType =
        BuiltInRegistries.ENTITY_TYPE
            .getOptional(Identifier.tryParse(entityData.getStringOr(PresetData.ID_TAG, "")))
            .orElse(null);
    if (entityType == null) {
      helper.fail("Unknown NPC type in the bank teller preset");
      return null;
    }

    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    ((PresetDataCapable<?>) easyNPC).importPresetData(entityData.copy());
    return easyNPC;
  }

  private static void setScore(
      GameTestHelper helper, ServerPlayer serverPlayer, String objectiveName, int score) {
    Scoreboard scoreboard = serverPlayer.level().getScoreboard();
    Objective objective = scoreboard.getObjective(objectiveName);
    if (objective == null) {
      Objective createdObjective =
          scoreboard.addObjective(
              objectiveName,
              ObjectiveCriteria.DUMMY,
              Component.literal(objectiveName),
              ObjectiveCriteria.RenderType.INTEGER,
              false,
              null);
      GameTestHelpers.addCleanup(helper, () -> scoreboard.removeObjective(createdObjective));
      objective = createdObjective;
    }

    scoreboard.getOrCreatePlayerScore(serverPlayer, objective).set(score);
  }

  private static DialogButtonReference reference(
      DialogDataEntry dialogDataEntry, DialogButtonEntry dialogButtonEntry) {
    return new DialogButtonReference(dialogDataEntry.getId(), dialogButtonEntry.id());
  }

  private static DialogButtonEntry scoreboardButton(
      ConditionOperationType operationType, int value) {
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    conditions.add(
        new ConditionDataEntry(ConditionType.SCOREBOARD, operationType, GAMETEST_OBJECTIVE, value));
    return new DialogButtonEntry(
        "5 Emeralds", "five", DialogButtonType.DEFAULT, new ActionDataSet(), conditions);
  }

  private static Set<ConditionDataEntry> oncePerDayConditions() {
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    conditions.add(
        new ConditionDataEntry(
            ConditionType.EXECUTION_LIMIT,
            DurationType.PER_DAY,
            ConditionOperationType.NONE,
            "",
            1));
    return conditions;
  }

  private static DialogButtonEntry oncePerDayButton() {
    return new DialogButtonEntry(
        "Daily Reward",
        "daily_reward",
        DialogButtonType.DEFAULT,
        new ActionDataSet(),
        oncePerDayConditions());
  }

  private static DialogButtonEntry actionButton(ActionDataEntry actionDataEntry) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(actionDataEntry);
    return new DialogButtonEntry("Reward", "reward", DialogButtonType.DEFAULT, actionDataSet);
  }

  private static ActionDataEntry firstActionOf(DialogDataSet dialogDataSet, String dialogLabel) {
    DialogDataEntry dialogDataEntry = dialogDataSet.getDialog(dialogLabel);
    if (dialogDataEntry == null || dialogDataEntry.getNumberOfDialogButtons() == 0) {
      return null;
    }

    List<ActionDataEntry> actionDataEntries =
        dialogDataEntry.getDialogButtons().iterator().next().actionDataSet().getOrderedEntries();
    return actionDataEntries.isEmpty() ? null : actionDataEntries.get(0);
  }

  private static DialogDataSet dialogDataSet(DialogDataEntry... dialogDataEntries) {
    DialogDataSet dialogDataSet = new DialogDataSet();
    for (DialogDataEntry dialogDataEntry : dialogDataEntries) {
      dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    }

    return dialogDataSet;
  }

  private static DialogDataEntry dialogEntry(
      String label, DialogButtonEntry... dialogButtonEntries) {
    Set<DialogButtonEntry> dialogButtons = new LinkedHashSet<>();
    for (DialogButtonEntry dialogButtonEntry : dialogButtonEntries) {
      dialogButtons.add(dialogButtonEntry);
    }

    return new DialogDataEntry(label, label, "Text", dialogButtons);
  }
}
