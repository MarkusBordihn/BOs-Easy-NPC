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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.api.event.DialogEventListener;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;
import net.minecraft.world.scores.criteria.ObjectiveCriteria;

public class ConditionalDialogActionTestHelper {

  private static final String LOW_DIALOG = "low_roll";
  private static final String HIGH_DIALOG = "high_roll";
  private static final String FALLBACK_DIALOG = "fallback";
  private static final int CHANCE_RUNS = 200;
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 2);

  private ConditionalDialogActionTestHelper() {}

  private static ConditionDataEntry scoreCondition(
      String objectiveName, ConditionOperationType operationType, int value) {
    return new ConditionDataEntry(ConditionType.SCOREBOARD, operationType, objectiveName, value);
  }

  private static ConditionDataEntry chanceCondition(int percentage) {
    return new ConditionDataEntry(ConditionType.CHANCE).withValue(percentage);
  }

  private static DialogDataEntry dialog(String label, ConditionDataEntry... conditions) {
    DialogDataEntry dialog = new DialogDataEntry(label, label, "Hello");
    dialog.setConditions(new LinkedHashSet<>(Arrays.asList(conditions)));
    return dialog;
  }

  private static void setDialogs(EasyNPC<?> easyNPC, DialogDataEntry... dialogs) {
    DialogDataSet dialogDataSet = new DialogDataSet();
    for (DialogDataEntry dialog : dialogs) {
      dialogDataSet.addDialog(dialog);
    }
    easyNPC.getEasyNPCDialogData().setDialogDataSet(dialogDataSet);
  }

  private static ActionDataEntry openDialog(ActionDataType actionDataType, String dialogLabel) {
    return new ActionDataEntry(actionDataType, dialogLabel);
  }

  private static ActionDataEntry withCondition(
      ActionDataEntry actionDataEntry, ConditionDataEntry conditionDataEntry) {
    ConditionDataSet conditionDataSet = new ConditionDataSet();
    conditionDataSet.add(conditionDataEntry);

    return actionDataEntry.withConditionDataSet(conditionDataSet);
  }

  private static ActionDataSet actionDataSet(ActionDataEntry... actionDataEntries) {
    ActionDataSet actionDataSet = new ActionDataSet();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      actionDataSet.add(actionDataEntry);
    }

    return actionDataSet;
  }

  private static Objective createObjective(GameTestHelper helper, String objectiveName) {
    Scoreboard scoreboard = helper.getLevel().getScoreboard();
    Objective objective =
        scoreboard.addObjective(
            objectiveName,
            ObjectiveCriteria.DUMMY,
            Component.literal(objectiveName),
            ObjectiveCriteria.RenderType.INTEGER,
            false,
            null);
    GameTestHelpers.addCleanup(helper, () -> scoreboard.removeObjective(objective));
    return objective;
  }

  private static void setScore(ServerPlayer serverPlayer, Objective objective, int score) {
    serverPlayer.level().getScoreboard().getOrCreatePlayerScore(serverPlayer, objective).set(score);
  }

  private static List<String> executeAndCollectOpenedDialogs(
      EasyNPC<?> easyNPC, ServerPlayer serverPlayer, ActionDataSet actionDataSet) {
    List<String> openedDialogLabels = new ArrayList<>();
    DialogEventListener dialogEventListener =
        (openingNPC, openingPlayer, dialogDataEntry) -> {
          if (openingNPC == easyNPC && dialogDataEntry != null) {
            openedDialogLabels.add(dialogDataEntry.getLabel());
          }
        };

    EasyNPCEventRegistry.registerDialogEventListener(dialogEventListener);
    try {
      easyNPC
          .getEasyNPCActionHandler()
          .executeActions(actionDataSet, ActionContext.of(serverPlayer));
    } finally {
      EasyNPCEventRegistry.unregisterDialogEventListener(dialogEventListener);
      serverPlayer.closeContainer();
    }
    return openedDialogLabels;
  }

  public static void assertUnmetConditionalDialogFallsThroughToTheNextOne(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    Objective objective = createObjective(helper, "gt_dialog_fall");
    setDialogs(
        easyNPC,
        dialog(
            LOW_DIALOG,
            scoreCondition(
                objective.getName(), ConditionOperationType.LESS_THAN_OR_EQUALS, 49)),
        dialog(
            HIGH_DIALOG,
            scoreCondition(
                objective.getName(), ConditionOperationType.GREATER_THAN_OR_EQUALS, 50)));
    ActionDataSet actionDataSet =
        actionDataSet(
            openDialog(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL, LOW_DIALOG),
            openDialog(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL, HIGH_DIALOG));

    setScore(serverPlayer, objective, 75);
    GameTestHelpers.assertEquals(
        helper,
        "An unmet conditional dialog must let the next conditional dialog open",
        List.of(HIGH_DIALOG),
        executeAndCollectOpenedDialogs(easyNPC, serverPlayer, actionDataSet));

    setScore(serverPlayer, objective, 25);
    GameTestHelpers.assertEquals(
        helper,
        "A met conditional dialog opens and keeps the following ones out",
        List.of(LOW_DIALOG),
        executeAndCollectOpenedDialogs(easyNPC, serverPlayer, actionDataSet));
  }

  public static void assertMetConditionalDialogBlocksLaterScreenActions(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    Objective objective = createObjective(helper, "gt_dialog_block");
    setDialogs(
        easyNPC,
        dialog(
            LOW_DIALOG,
            scoreCondition(
                objective.getName(), ConditionOperationType.LESS_THAN_OR_EQUALS, 49)),
        dialog(HIGH_DIALOG));
    setScore(serverPlayer, objective, 25);

    GameTestHelpers.assertEquals(
        helper,
        "Only the first screen action may open once its dialog conditions are met",
        List.of(LOW_DIALOG),
        executeAndCollectOpenedDialogs(
            easyNPC,
            serverPlayer,
            actionDataSet(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL, LOW_DIALOG),
                openDialog(ActionDataType.OPEN_NAMED_DIALOG, HIGH_DIALOG))));
  }

  public static void assertFallbackRunsWhenNoConditionalDialogMatches(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    Objective objective = createObjective(helper, "gt_dialog_none");
    setDialogs(
        easyNPC,
        dialog(LOW_DIALOG, scoreCondition(objective.getName(), ConditionOperationType.EQUALS, 7)),
        dialog(FALLBACK_DIALOG));
    setScore(serverPlayer, objective, 1);

    GameTestHelpers.assertEquals(
        helper,
        "The fallback must run when no conditional dialog matched",
        List.of(FALLBACK_DIALOG),
        executeAndCollectOpenedDialogs(
            easyNPC,
            serverPlayer,
            actionDataSet(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL, LOW_DIALOG),
                withCondition(
                    openDialog(ActionDataType.OPEN_NAMED_DIALOG, FALLBACK_DIALOG),
                    new ConditionDataEntry(ConditionType.FALLBACK)))));
  }

  public static void assertActionConditionsSelectTheDialog(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    Objective objective = createObjective(helper, "gt_action_score");
    setDialogs(easyNPC, dialog(LOW_DIALOG), dialog(HIGH_DIALOG));
    ActionDataSet actionDataSet =
        actionDataSet(
            withCondition(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG, LOW_DIALOG),
                scoreCondition(
                    objective.getName(), ConditionOperationType.LESS_THAN_OR_EQUALS, 49)),
            withCondition(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG, HIGH_DIALOG),
                scoreCondition(
                    objective.getName(), ConditionOperationType.GREATER_THAN_OR_EQUALS, 50)));

    setScore(serverPlayer, objective, 75);
    GameTestHelpers.assertEquals(
        helper,
        "The action condition on the score selects the high dialog",
        List.of(HIGH_DIALOG),
        executeAndCollectOpenedDialogs(easyNPC, serverPlayer, actionDataSet));

    setScore(serverPlayer, objective, 25);
    GameTestHelpers.assertEquals(
        helper,
        "The action condition on the score selects the low dialog",
        List.of(LOW_DIALOG),
        executeAndCollectOpenedDialogs(easyNPC, serverPlayer, actionDataSet));
  }

  public static void assertActionChanceIsRolledOnce(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    setDialogs(easyNPC, dialog(LOW_DIALOG), dialog(FALLBACK_DIALOG));
    ActionDataSet actionDataSet =
        actionDataSet(
            withCondition(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG, LOW_DIALOG), chanceCondition(50)),
            withCondition(
                openDialog(ActionDataType.OPEN_NAMED_DIALOG, FALLBACK_DIALOG),
                new ConditionDataEntry(ConditionType.FALLBACK)));

    assertEveryRunOpensExactlyOneOf(
        helper, easyNPC, serverPlayer, actionDataSet, LOW_DIALOG, FALLBACK_DIALOG);
  }

  public static void assertDialogChanceIsRolledOnce(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    setDialogs(easyNPC, dialog(LOW_DIALOG, chanceCondition(50)), dialog(HIGH_DIALOG));
    ActionDataSet actionDataSet =
        actionDataSet(
            openDialog(ActionDataType.OPEN_NAMED_DIALOG_CONDITIONAL, LOW_DIALOG),
            openDialog(ActionDataType.OPEN_NAMED_DIALOG, HIGH_DIALOG));

    assertEveryRunOpensExactlyOneOf(
        helper, easyNPC, serverPlayer, actionDataSet, LOW_DIALOG, HIGH_DIALOG);
  }

  private static void assertEveryRunOpensExactlyOneOf(
      GameTestHelper helper,
      EasyNPC<?> easyNPC,
      ServerPlayer serverPlayer,
      ActionDataSet actionDataSet,
      String firstDialog,
      String secondDialog) {
    int firstDialogCount = 0;
    int secondDialogCount = 0;
    for (int run = 0; run < CHANCE_RUNS; run++) {
      List<String> openedDialogs =
          executeAndCollectOpenedDialogs(easyNPC, serverPlayer, actionDataSet);
      GameTestHelpers.assertEquals(
          helper, "Every run must open exactly one dialog (run " + run + ")", 1, openedDialogs.size());
      if (openedDialogs.get(0).equals(firstDialog)) {
        firstDialogCount++;
      } else if (openedDialogs.get(0).equals(secondDialog)) {
        secondDialogCount++;
      }
    }

    GameTestHelpers.assertEquals(
        helper,
        "Every run must open one of the two dialogs",
        CHANCE_RUNS,
        firstDialogCount + secondDialogCount);
    GameTestHelpers.assertTrue(
        helper,
        "Both dialogs must open over "
            + CHANCE_RUNS
            + " runs, got "
            + firstDialogCount
            + " / "
            + secondDialogCount,
        firstDialogCount > 0 && secondDialogCount > 0);
  }
}
