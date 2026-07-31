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

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.event.StateEventListener;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;

public class NpcStateTestHelper {

  private static final ResourceLocation QUEST_STATE = StateIdentifier.parse("gametest_quest");
  private static final ResourceLocation STAGE_STATE = StateIdentifier.parse("gametest_stage");
  private static final String FORGE_KEEPER_PRESET =
      "easy_npc:default_preset/humanoid/forge_keeper.npc.snbt";
  private static final Vec3 NPC_POSITION = new Vec3(1, 1, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 1, 2);

  private NpcStateTestHelper() {}

  public static void assertStateActionAppliesEveryOperation(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
    GameTestHelpers.assertNotNull(helper, "NPC must support states", stateData);

    executeStateAction(easyNPC, "set gametest_quest 5");
    GameTestHelpers.assertEquals(
        helper, "Set must store the value", 5, stateData.getStateNumber(QUEST_STATE));

    executeStateAction(easyNPC, "increase gametest_quest 2");
    GameTestHelpers.assertEquals(
        helper, "Increase must count up", 7, stateData.getStateNumber(QUEST_STATE));

    executeStateAction(easyNPC, "decrease gametest_quest 3");
    GameTestHelpers.assertEquals(
        helper, "Decrease must count down", 4, stateData.getStateNumber(QUEST_STATE));

    executeStateAction(easyNPC, "set gametest_quest intro");
    GameTestHelpers.assertEquals(
        helper, "Set must store a text value", "intro", stateData.getStateText(QUEST_STATE));

    executeStateAction(easyNPC, "toggle gametest_quest");
    GameTestHelpers.assertTrue(
        helper, "Toggle must turn a set state off", !stateData.getStateFlag(QUEST_STATE));

    executeStateAction(easyNPC, "remove gametest_quest");
    GameTestHelpers.assertTrue(
        helper, "Remove must drop the state", !stateData.hasState(QUEST_STATE));
  }

  public static void assertInvalidStateActionIsIgnored(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();

    executeStateAction(easyNPC, "set Gametest_Quest 5");
    executeStateAction(easyNPC, "set");
    executeStateAction(easyNPC, "increase gametest_quest");
    executeStateAction(easyNPC, "sett gametest_quest 5");

    GameTestHelpers.assertTrue(
        helper,
        "An action with an invalid state, operation or a missing value must not write anything",
        stateData.getStateDataSet().isEmpty());
  }

  public static void assertDebugActionStillWritesTheState(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    executeStateAction(easyNPC, "set gametest_quest 2", true);

    GameTestHelpers.assertEquals(
        helper,
        "A state action with debug enabled must still write the state",
        2,
        easyNPC.getEasyNPCStateData().getStateNumber(QUEST_STATE));
  }

  public static void assertStateSurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
    GameTestHelpers.assertNotNull(helper, "NPC must support states", stateData);

    stateData.setState(QUEST_STATE, 3);
    stateData.setState(STAGE_STATE, "intro");

    EasyNPC<?> reloadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, PLAYER_POSITION);
    StateDataCapable<?> reloadedStateData = reloadedNPC.getEasyNPCStateData();
    reloadedStateData.setStateDataSet(new StateDataSet(stateData.getStateDataSet().createTag()));

    GameTestHelpers.assertEquals(
        helper,
        "A number state must survive save and load",
        3,
        reloadedStateData.getStateNumber(QUEST_STATE));
    GameTestHelpers.assertEquals(
        helper,
        "A text state must survive save and load",
        "intro",
        reloadedStateData.getStateText(STAGE_STATE));
  }

  public static void assertStateConditionLocksDialogButton(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);
    DialogButtonEntry dialogButtonEntry = questButton();
    DialogDataSet dialogDataSet = dialogDataSet(dialogButtonEntry);

    assertButtonAvailable(
        helper, dialogDataSet, serverPlayer, easyNPC, dialogButtonEntry.label(), false);

    easyNPC.getEasyNPCStateData().setState(QUEST_STATE, 1);

    assertButtonAvailable(
        helper, dialogDataSet, serverPlayer, easyNPC, dialogButtonEntry.label(), true);
  }

  public static void assertStateIsResetOnPresetImport(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.getEasyNPCStateData().setState(QUEST_STATE, 9);

    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    presetTag.remove(StateDataCapable.DATA_STATE_TAG);
    easyNPC.getEasyNPCPresetData().importPresetData(presetTag);

    GameTestHelpers.assertTrue(
        helper,
        "A preset without states must not keep the states of the previous preset",
        !easyNPC.getEasyNPCStateData().hasState(QUEST_STATE));
  }

  public static void assertStateChangeNotifiesListener(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AtomicInteger reportedChanges = new AtomicInteger();
    StateEventListener stateEventListener =
        (notifiedNPC, stateId, previousStateEntry, currentStateEntry) -> {
          if (notifiedNPC == easyNPC && QUEST_STATE.equals(stateId)) {
            reportedChanges.incrementAndGet();
          }
        };
    EasyNPCEventRegistry.registerStateEventListener(stateEventListener);

    try {
      executeStateAction(easyNPC, "set gametest_quest 1");
      executeStateAction(easyNPC, "set gametest_quest 1");
      executeStateAction(easyNPC, "increase gametest_quest 1");
    } finally {
      EasyNPCEventRegistry.unregisterStateEventListener(stateEventListener);
    }

    GameTestHelpers.assertEquals(
        helper, "Only a real state change must notify a listener", 2, reportedChanges.get());
  }

  public static void assertForgeKeeperPresetUsesStates(GameTestHelper helper) {
    EasyNPC<?> forgeKeeper = importForgeKeeper(helper);
    if (forgeKeeper == null) {
      helper.fail("The forge keeper preset was not imported");
      return;
    }

    StateDataCapable<?> stateData = forgeKeeper.getEasyNPCStateData();
    DialogDataSet dialogDataSet = forgeKeeper.getEasyNPCDialogData().getDialogDataSet();
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "The forge keeper starts with a banked fire and without orders",
        stateData.getStateDataSet().isEmpty());
    assertButtonAvailable(helper, dialogDataSet, serverPlayer, forgeKeeper, "button_light", true);
    assertButtonAvailable(helper, dialogDataSet, serverPlayer, forgeKeeper, "button_order", false);

    executeStateAction(forgeKeeper, "toggle forge_lit");
    assertButtonAvailable(helper, dialogDataSet, serverPlayer, forgeKeeper, "button_light", false);
    assertButtonAvailable(helper, dialogDataSet, serverPlayer, forgeKeeper, "button_order", true);

    executeStateAction(forgeKeeper, "increase orders_taken 3");
    assertButtonAvailable(helper, dialogDataSet, serverPlayer, forgeKeeper, "button_order", false);
    GameTestHelpers.assertEquals(
        helper,
        "A full bench opens the booked out dialog",
        "booked_out",
        dialogDataSet
            .getNextAvailableDialog(serverPlayer, forgeKeeper.getLivingEntity())
            .getLabel());
  }

  private static void assertButtonAvailable(
      GameTestHelper helper,
      DialogDataSet dialogDataSet,
      ServerPlayer serverPlayer,
      EasyNPC<?> easyNPC,
      String buttonLabel,
      boolean expectedAvailable) {
    DialogButtonEntry dialogButtonEntry = findButton(dialogDataSet, buttonLabel);
    if (dialogButtonEntry == null) {
      helper.fail("The forge keeper preset has no button " + buttonLabel);
      return;
    }

    CompoundTag lockedTag = new CompoundTag();
    AdditionalScreenData.addDialogButtonLockData(lockedTag, dialogDataSet, serverPlayer, easyNPC);
    boolean available =
        !AdditionalScreenData.getLockedDialogButtons(lockedTag).contains(dialogButtonEntry.id());

    GameTestHelpers.assertEquals(
        helper, "Availability of the button " + buttonLabel, expectedAvailable, available);
  }

  private static DialogButtonEntry findButton(DialogDataSet dialogDataSet, String buttonLabel) {
    for (DialogDataEntry dialogDataEntry : dialogDataSet.getDialogsByLabel()) {
      for (DialogButtonEntry dialogButtonEntry : dialogDataEntry.getDialogButtons()) {
        if (buttonLabel.equals(dialogButtonEntry.label())) {
          return dialogButtonEntry;
        }
      }
    }

    return null;
  }

  private static EasyNPC<?> importForgeKeeper(GameTestHelper helper) {
    CommandSourceStack commandSourceStack =
        helper
            .getLevel()
            .getServer()
            .createCommandSourceStack()
            .withLevel(helper.getLevel())
            .withPosition(helper.absoluteVec(NPC_POSITION));
    CommandDispatcher<CommandSourceStack> commandDispatcher =
        helper.getLevel().getServer().getCommands().getDispatcher();
    String command = "easy_npc preset import_new default " + FORGE_KEEPER_PRESET + " ~ ~1 ~";

    try {
      if (commandDispatcher.execute(commandDispatcher.parse(command, commandSourceStack)) == 0) {
        helper.fail("Unable to import the forge keeper preset");
        return null;
      }
    } catch (CommandSyntaxException exception) {
      helper.fail("Unable to import the forge keeper preset: " + exception.getMessage());
      return null;
    }

    AABB testBounds = AABB.ofSize(helper.absoluteVec(new Vec3(1.5, 2, 1.5)), 6, 6, 6);
    for (Mob entity : helper.getLevel().getEntitiesOfClass(Mob.class, testBounds)) {
      if (entity instanceof EasyNPC<?> easyNPC
          && "Forge Keeper".equals(entity.getName().getString())) {
        return easyNPC;
      }
    }

    return null;
  }

  private static void executeStateAction(EasyNPC<?> easyNPC, String command) {
    executeStateAction(easyNPC, command, false);
  }

  private static void executeStateAction(EasyNPC<?> easyNPC, String command, boolean enableDebug) {
    easyNPC
        .getEasyNPCActionHandler()
        .executeAction(
            new ActionDataEntry(ActionDataType.NPC_STATE, command, false, enableDebug), null);
  }

  private static DialogButtonEntry questButton() {
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    conditions.add(
        new ConditionDataEntry(
            ConditionType.NPC_STATE, ConditionOperationType.EQUALS, QUEST_STATE.toString(), 1));
    return new DialogButtonEntry(
        "quest_button", "quest_button", DialogButtonType.DEFAULT, new ActionDataSet(), conditions);
  }

  private static DialogDataSet dialogDataSet(DialogButtonEntry dialogButtonEntry) {
    Set<DialogButtonEntry> dialogButtons = new LinkedHashSet<>();
    dialogButtons.add(dialogButtonEntry);
    DialogDataEntry dialogDataEntry =
        new DialogDataEntry("state_dialog", "State Dialog", "Text", dialogButtons);
    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    return dialogDataSet;
  }
}
