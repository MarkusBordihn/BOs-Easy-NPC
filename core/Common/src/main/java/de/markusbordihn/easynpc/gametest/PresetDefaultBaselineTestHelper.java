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

import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class PresetDefaultBaselineTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final String NPC_TYPE_TAG = "id";

  private PresetDefaultBaselineTestHelper() {}

  public static void assertMissingObjectiveDataKeepsDefaultObjectives(
      GameTestHelper helper, EntityType<?> entityType) {
    CompoundTag presetTag = minimalPreset(entityType);

    EasyNPC<?> easyNPC = importPreset(helper, entityType, presetTag);

    GameTestHelpers.assertNotNull(
        helper,
        "A preset without objective data must keep the standard look at player objective",
        easyNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_PLAYER));
    GameTestHelpers.assertNotNull(
        helper,
        "A preset without objective data must keep the standard look at mob objective",
        easyNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_MOB));
  }

  public static void assertEmptyObjectiveDataRemovesAllObjectives(
      GameTestHelper helper, EntityType<?> entityType) {
    CompoundTag presetTag = minimalPreset(entityType);
    CompoundTag objectiveDataTag = new CompoundTag();
    objectiveDataTag.put(ObjectiveDataSet.DATA_OBJECTIVE_DATA_SET_TAG, new ListTag());
    presetTag.put(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG, objectiveDataTag);

    EasyNPC<?> easyNPC = importPreset(helper, entityType, presetTag);

    GameTestHelpers.assertTrue(
        helper,
        "A preset with an empty objective list must remove the standard objectives",
        easyNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_PLAYER) == null);
  }

  public static void assertCustomObjectiveDataReplacesDefaultObjectives(
      GameTestHelper helper, EntityType<?> entityType) {
    CompoundTag presetTag = minimalPreset(entityType);
    ObjectiveDataSet objectiveDataSet = new ObjectiveDataSet();
    objectiveDataSet.addObjective(new ObjectiveDataEntry(ObjectiveType.RANDOM_STROLL));
    CompoundTag objectiveDataTag = new CompoundTag();
    objectiveDataSet.save(objectiveDataTag);
    presetTag.put(ObjectiveDataCapable.DATA_OBJECTIVE_DATA_TAG, objectiveDataTag);

    EasyNPC<?> easyNPC = importPreset(helper, entityType, presetTag);

    GameTestHelpers.assertNotNull(
        helper,
        "A preset with its own objectives must keep them",
        easyNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.RANDOM_STROLL));
    GameTestHelpers.assertTrue(
        helper,
        "A preset with its own objectives must not keep the standard objectives",
        easyNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_PLAYER) == null);
  }

  private static CompoundTag minimalPreset(EntityType<?> entityType) {
    CompoundTag presetTag = new CompoundTag();
    presetTag.putString(NPC_TYPE_TAG, EntityType.getKey(entityType).toString());
    return presetTag;
  }

  private static EasyNPC<?> importPreset(
      GameTestHelper helper, EntityType<?> entityType, CompoundTag presetTag) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    ((PresetDataCapable<?>) easyNPC).importPresetData(presetTag);
    return easyNPC;
  }
}
