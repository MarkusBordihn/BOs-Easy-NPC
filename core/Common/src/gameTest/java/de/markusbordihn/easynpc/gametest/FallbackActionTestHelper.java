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

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.RelationshipType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class FallbackActionTestHelper {

  private static final ResourceLocation MAIN_STATE =
      StateIdentifier.parse("gametest_fallback_main");
  private static final ResourceLocation FALLBACK_STATE =
      StateIdentifier.parse("gametest_fallback_fallback");
  private static final ResourceLocation AFTER_SCREEN_STATE =
      StateIdentifier.parse("gametest_fallback_after_screen");
  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private FallbackActionTestHelper() {}

  private static ActionDataEntry increaseState(ResourceLocation stateIdentifier) {
    return new ActionDataEntry(
        ActionDataType.NPC_STATE, "increase " + stateIdentifier.getPath() + " 1");
  }

  private static ActionDataEntry withCondition(
      ActionDataEntry actionDataEntry, ConditionDataEntry conditionDataEntry) {
    ConditionDataSet conditionDataSet = new ConditionDataSet();
    conditionDataSet.add(conditionDataEntry);

    return actionDataEntry.withConditionDataSet(conditionDataSet);
  }

  private static ActionDataEntry fallbackAction() {
    return withCondition(
        increaseState(FALLBACK_STATE), new ConditionDataEntry(ConditionType.FALLBACK));
  }

  private static ActionDataSet actionDataSet(ActionDataEntry... actionDataEntries) {
    ActionDataSet actionDataSet = new ActionDataSet();
    for (ActionDataEntry actionDataEntry : actionDataEntries) {
      actionDataSet.add(actionDataEntry);
    }

    return actionDataSet;
  }

  private static int stateNumber(EasyNPC<?> easyNPC, ResourceLocation stateIdentifier) {
    return easyNPC.getEasyNPCStateData().getStateNumber(stateIdentifier);
  }

  private static void executeOnSpawn(EasyNPC<?> easyNPC, ActionDataSet actionDataSet) {
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(actionDataSet, ActionContext.of(ActionEventType.ON_SPAWN, null, null));
  }

  public static void assertFallbackStaysOutWhenARegularActionFired(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeOnSpawn(easyNPC, actionDataSet(increaseState(MAIN_STATE), fallbackAction()));

    GameTestHelpers.assertEquals(
        helper, "The regular action runs", 1, stateNumber(easyNPC, MAIN_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The fallback must stay out as long as a regular action fired",
        0,
        stateNumber(easyNPC, FALLBACK_STATE));
  }

  public static void assertFallbackRunsWithoutAWait(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeOnSpawn(
        easyNPC,
        actionDataSet(
            withCondition(
                increaseState(MAIN_STATE),
                new ConditionDataEntry(ConditionType.RELATIONSHIP)
                    .withSubType(RelationshipType.OWNER)),
            fallbackAction()));

    GameTestHelpers.assertEquals(
        helper,
        "The action with an unmet condition stays out",
        0,
        stateNumber(easyNPC, MAIN_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "The fallback runs once when no regular action fired",
        1,
        stateNumber(easyNPC, FALLBACK_STATE));
  }

  public static void assertFallbackRunsOnlyOnce(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeOnSpawn(easyNPC, actionDataSet(fallbackAction(), fallbackAction()));

    GameTestHelpers.assertEquals(
        helper,
        "Every fallback of the pass runs, but the pass runs only once",
        2,
        stateNumber(easyNPC, FALLBACK_STATE));
  }

  public static void assertScreenActionWithoutAPlayerIsSkipped(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.addDialog(new DialogDataEntry("default", "Default", "Hello"));
    easyNPC.getEasyNPCDialogData().setDialogDataSet(dialogDataSet);

    executeOnSpawn(
        easyNPC,
        actionDataSet(
            new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG),
            increaseState(AFTER_SCREEN_STATE)));

    GameTestHelpers.assertEquals(
        helper,
        "A screen action without a player must not stop the following actions",
        1,
        stateNumber(easyNPC, AFTER_SCREEN_STATE));
  }
}
