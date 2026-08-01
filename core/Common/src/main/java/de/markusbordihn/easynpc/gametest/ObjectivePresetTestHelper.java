/*
 * Copyright 2025 Markus Bordihn
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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.event.EasyNPCEventHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.animal.pig.Pig;
import net.minecraft.world.phys.Vec3;

public class ObjectivePresetTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(2, 2, 2);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 2);
  private static final String TARGET_PLAYER_NAME = "objective-target-player";

  private ObjectivePresetTestHelper() {}

  public static void assertObjectiveValuesFromPresetAreClamped(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertNotNull(helper, "NPC must support objectives", objectiveData);

    objectiveData.addOrUpdateCustomObjective(followPlayerObjective());

    CompoundTag presetTag = ((PresetDataCapable<?>) easyNPC).serializePresetData();
    CompoundTag objectiveEntryTag = findObjectiveEntry(presetTag, ObjectiveType.FOLLOW_PLAYER);
    if (objectiveEntryTag == null) {
      helper.fail("The preset does not contain the follow player objective");
      return;
    }

    objectiveEntryTag.putDouble(ObjectiveDataEntry.DATA_SPEED_MODIFIER_TAG, -1.0D);
    objectiveEntryTag.putFloat(ObjectiveDataEntry.DATA_STOP_DISTANCE_TAG, -5.0F);
    objectiveEntryTag.putFloat(ObjectiveDataEntry.DATA_PROBABILITY_TAG, 2.5F);

    EasyNPC<?> importedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    ((PresetDataCapable<?>) importedNPC).importPresetData(presetTag);

    ObjectiveDataEntry importedObjective =
        importedNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.FOLLOW_PLAYER);
    GameTestHelpers.assertNotNull(
        helper, "The imported preset must keep the follow player objective", importedObjective);
    GameTestHelpers.assertEquals(
        helper, "A negative speed modifier is clamped", 0.0D, importedObjective.getSpeedModifier());
    GameTestHelpers.assertEquals(
        helper, "A negative stop distance is clamped", 0.0F, importedObjective.getStopDistance());
    GameTestHelpers.assertEquals(
        helper, "A probability above one is clamped", 1.0F, importedObjective.getProbability());
  }

  public static void assertPlayerTargetObjectiveIsRegisteredOnJoin(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertNotNull(helper, "NPC must support objectives", objectiveData);

    ObjectiveDataEntry objectiveDataEntry = followPlayerObjective();
    GameTestHelpers.assertTrue(
        helper,
        "An objective without its target player must not be registered",
        !objectiveData.addOrUpdateCustomObjective(objectiveDataEntry));

    ServerPlayer targetPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, TARGET_PLAYER_NAME);
    EasyNPCEventHandler.handlePlayerJoinEvent(easyNPC, targetPlayer);

    GameTestHelpers.assertTrue(
        helper,
        "The objective must be registered once its target player joined",
        objectiveData.getObjective(ObjectiveType.FOLLOW_PLAYER).isRegistered());
  }

  public static void assertEntityTargetObjectiveIsReleasedOnLeave(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertNotNull(helper, "NPC must support objectives", objectiveData);

    Pig targetEntity = GameTestHelpers.spawnEntityType(helper, EntityType.PIG);
    GameTestHelpers.assertNotNull(helper, "The target entity must be spawned", targetEntity);
    targetEntity.setPos(helper.absoluteVec(SECOND_NPC_POSITION));

    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.FOLLOW_ENTITY_BY_UUID);
    objectiveDataEntry.setTargetEntityUUID(targetEntity.getUUID());
    GameTestHelpers.assertTrue(
        helper,
        "An objective with a present target entity must be registered",
        objectiveData.addOrUpdateCustomObjective(objectiveDataEntry));

    targetEntity.discard();
    EasyNPCEventHandler.handleLivingEntityLeaveEvent(easyNPC, targetEntity);

    GameTestHelpers.assertTrue(
        helper,
        "The objective must be released once its target entity left",
        !objectiveData.getObjective(ObjectiveType.FOLLOW_ENTITY_BY_UUID).isRegistered());
  }

  private static ObjectiveDataEntry followPlayerObjective() {
    ObjectiveDataEntry objectiveDataEntry = new ObjectiveDataEntry(ObjectiveType.FOLLOW_PLAYER);
    objectiveDataEntry.setTargetPlayerName(TARGET_PLAYER_NAME);
    return objectiveDataEntry;
  }

  private static CompoundTag findObjectiveEntry(
      CompoundTag presetTag, ObjectiveType objectiveType) {
    CompoundTag objectiveDataTag =
        presetTag.getCompoundOrEmpty(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG);
    ListTag objectiveList =
        objectiveDataTag.getListOrEmpty(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG);

    for (int index = 0; index < objectiveList.size(); index++) {
      CompoundTag objectiveEntryTag = objectiveList.getCompoundOrEmpty(index);
      if (objectiveType
          .name()
          .equals(objectiveEntryTag.getString(ObjectiveDataEntry.DATA_TYPE_TAG).orElse(""))) {
        return objectiveEntryTag;
      }
    }

    return null;
  }
}
