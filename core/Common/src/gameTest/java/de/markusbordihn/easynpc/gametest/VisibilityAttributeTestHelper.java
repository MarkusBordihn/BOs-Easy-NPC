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

import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.data.visibility.VisibilityConditionSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import de.markusbordihn.easynpc.handler.VisibilityConditionTracker;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.Objective;
import net.minecraft.world.scores.Scoreboard;
import net.minecraft.world.scores.criteria.ObjectiveCriteria;

public class VisibilityAttributeTestHelper {

  private static final long DAY_TIME = 6000L;
  private static final long NIGHT_TIME = 18000L;

  private VisibilityAttributeTestHelper() {}

  public static void assertNightVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    ServerLevel level = helper.getLevel();
    long previousDayTime = level.getDayTime();

    try {
      level.setDayTime(NIGHT_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be visible at night as long as it is not disabled",
          !easyNPC.getEntity().isInvisibleTo(serverPlayer));

      setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be invisible at night after disabling the night visibility",
          easyNPC.getEntity().isInvisibleTo(serverPlayer));

      level.setDayTime(DAY_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be visible at day after disabling only the night visibility",
          !easyNPC.getEntity().isInvisibleTo(serverPlayer));
    } finally {
      level.setDayTime(previousDayTime);
    }
  }

  public static void assertOwnerDoesNotOverrideNightVisibility(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    easyNPC.getEasyNPCOwnerData().setNPCOwner(serverPlayer);
    ServerLevel level = helper.getLevel();
    long previousDayTime = level.getDayTime();

    try {
      setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);
      level.setDayTime(NIGHT_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "Night visibility must also apply to the owner of the NPC",
          easyNPC.getEntity().isInvisibleTo(serverPlayer));
    } finally {
      level.setDayTime(previousDayTime);
    }
  }

  public static void assertOwnerVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    easyNPC.getEasyNPCOwnerData().setNPCOwner(serverPlayer);

    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible to its owner by default",
        !easyNPC.getEntity().isInvisibleTo(serverPlayer));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_TO_OWNER, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible to its owner after disabling the owner visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertGameModeVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    GameTestHelpers.assertTrue(
        helper,
        "Mocked test player must be in creative mode",
        serverPlayer.isCreative() && !serverPlayer.isSpectator());

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_IN_CREATIVE, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible in creative mode after disabling the creative visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_IN_CREATIVE, true);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible in creative mode after enabling the creative visibility",
        !easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertPreviewResolvesSimulatedDayTime(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    ServerLevel level = helper.getLevel();
    long previousDayTime = level.getDayTime();

    try {
      level.setDayTime(DAY_TIME);
      setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);

      GameTestHelpers.assertTrue(
          helper,
          "Preview must report the NPC as visible for a simulated day time",
          VisibilityHandler.isVisibleToPlayerAtDayTime(easyNPC, serverPlayer, DAY_TIME));
      GameTestHelpers.assertTrue(
          helper,
          "Preview must report the NPC as hidden for a simulated night time",
          !VisibilityHandler.isVisibleToPlayerAtDayTime(easyNPC, serverPlayer, NIGHT_TIME));
      GameTestHelpers.assertTrue(
          helper, "Preview must not change the world time", level.getDayTime() == DAY_TIME);
    } finally {
      level.setDayTime(previousDayTime);
    }
  }

  public static void assertMainVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible after disabling the main visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertVisibilityConditionsAreEvaluatedPerPlayer(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer matchingPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1), "visibility-match");
    ServerPlayer otherPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 2), "visibility-other");
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    Objective objective = createObjective(helper, "vis_per_player");
    setScore(matchingPlayer, objective, 1);

    setVisibilityConditions(
        easyNPC, scoreCondition(objective, ConditionOperationType.GREATER_THAN_OR_EQUALS, 1));
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible to the player matching the visibility condition",
        !easyNPC.getEntity().isInvisibleTo(matchingPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible to the player not matching the visibility condition",
        easyNPC.getEntity().isInvisibleTo(otherPlayer));
    GameTestHelpers.assertTrue(
        helper,
        "Evaluating a visibility condition must not create a score entry",
        helper.getLevel().getScoreboard().getPlayerScoreInfo(otherPlayer, objective) == null);

    setScore(otherPlayer, objective, 1);
    VisibilityConditionTracker.refresh(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must become visible once the player matches the visibility condition",
        !easyNPC.getEntity().isInvisibleTo(otherPlayer));
  }

  public static void assertEmptyVisibilityConditionsKeepNPCVisible(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();

    GameTestHelpers.assertTrue(
        helper,
        "NPC without visibility conditions must be visible",
        !displayAttributeData.hasVisibilityConditions()
            && !easyNPC.getEntity().isInvisibleTo(serverPlayer));

    setVisibilityConditions(
        easyNPC,
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "vis_missing", 1));
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible while its visibility condition is not met",
        displayAttributeData.hasVisibilityConditions()
            && easyNPC.getEntity().isInvisibleTo(serverPlayer));

    displayAttributeData.setVisibilityConditionSet(VisibilityConditionSet.EMPTY);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible again after removing all visibility conditions",
        !displayAttributeData.hasVisibilityConditions()
            && !easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertUnsupportedVisibilityConditionsAreIgnored(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    setVisibilityConditions(
        easyNPC,
        new ConditionDataEntry(ConditionType.CHANCE).withValue(1),
        new ConditionDataEntry(ConditionType.EXECUTION_LIMIT).withValue(1),
        new ConditionDataEntry(ConditionType.FALLBACK));
    GameTestHelpers.assertTrue(
        helper,
        "Chance, execution limit and fallback must not be stored as visibility conditions",
        easyNPC.getEasyNPCDisplayAttributeData().getVisibilityConditionSet().isEmpty()
            && !easyNPC.getEntity().isInvisibleTo(serverPlayer));

    List<ConditionDataEntry> conditions = new ArrayList<>();
    for (int i = 0; i < VisibilityConditionSet.MAXIMUM_CONDITIONS + 4; i++) {
      conditions.add(
          new ConditionDataEntry(
              ConditionType.SCOREBOARD, ConditionOperationType.EQUALS, "vis_cap", i));
    }
    GameTestHelpers.assertTrue(
        helper,
        "Visibility conditions must be limited to the maximum amount",
        new VisibilityConditionSet(conditions).conditions().size()
            == VisibilityConditionSet.MAXIMUM_CONDITIONS);
  }

  public static void assertVisibilityConditionsSurviveSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    EasyNPC<?> loadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 2));
    setVisibilityConditions(
        easyNPC,
        new ConditionDataEntry(
            ConditionType.SCOREBOARD, ConditionOperationType.LESS_THAN, "vis_saved", 5));
    VisibilityConditionSet visibilityConditionSet =
        easyNPC.getEasyNPCDisplayAttributeData().getVisibilityConditionSet();

    CompoundTag compoundTag = new CompoundTag();
    easyNPC.getEasyNPCDisplayAttributeData().addAdditionalDisplayAttributeData(compoundTag);
    GameTestHelpers.assertTrue(
        helper,
        "Saved NPC data must contain the visibility conditions",
        compoundTag.contains(DisplayAttributeDataCapable.DATA_VISIBILITY_CONDITIONS_TAG));

    loadedNPC.getEasyNPCDisplayAttributeData().readAdditionalDisplayAttributeData(compoundTag);
    GameTestHelpers.assertTrue(
        helper,
        "Loaded NPC must restore the visibility conditions",
        visibilityConditionSet.equals(
                loadedNPC.getEasyNPCDisplayAttributeData().getVisibilityConditionSet())
            && loadedNPC.getEasyNPCDisplayAttributeData().hasVisibilityConditions());

    CompoundTag emptyCompoundTag = new CompoundTag();
    GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(0, 2, 2))
        .getEasyNPCDisplayAttributeData()
        .addAdditionalDisplayAttributeData(emptyCompoundTag);
    GameTestHelpers.assertTrue(
        helper,
        "NPC without visibility conditions must not save an empty visibility condition tag",
        !emptyCompoundTag.contains(DisplayAttributeDataCapable.DATA_VISIBILITY_CONDITIONS_TAG));
  }

  public static void assertStopSeenByPlayerDropsCachedVisibility(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    Objective objective = createObjective(helper, "vis_stop_seen");
    setScore(serverPlayer, objective, 1);
    setVisibilityConditions(
        easyNPC, scoreCondition(objective, ConditionOperationType.EQUALS, 1));

    VisibilityConditionTracker.startSeenByPlayer(easyNPC, serverPlayer);
    setScore(serverPlayer, objective, 0);
    GameTestHelpers.assertTrue(
        helper,
        "Tracked player must keep the cached visibility until the next check",
        !easyNPC.getEntity().isInvisibleTo(serverPlayer));

    VisibilityConditionTracker.stopSeenByPlayer(easyNPC, serverPlayer);
    GameTestHelpers.assertTrue(
        helper,
        "Untracked player must get a fresh visibility evaluation",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  private static ConditionDataEntry scoreCondition(
      Objective objective, ConditionOperationType operationType, int value) {
    return new ConditionDataEntry(
        ConditionType.SCOREBOARD, operationType, objective.getName(), value);
  }

  private static void setVisibilityConditions(
      EasyNPC<?> easyNPC, ConditionDataEntry... conditionDataEntries) {
    easyNPC
        .getEasyNPCDisplayAttributeData()
        .setVisibilityConditionSet(new VisibilityConditionSet(List.of(conditionDataEntries)));
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

  private static void setDisplayAttribute(
      EasyNPC<?> easyNPC, DisplayAttributeType displayAttributeType, boolean value) {
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    displayAttributeData.setDisplayAttribute(displayAttributeType, ValueType.BOOLEAN, value);
  }
}
