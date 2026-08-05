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

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class StoredDataTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(2, 2, 2);

  private static final List<SoundType> EXPECTED_SOUND_TYPES =
      List.of(SoundType.DEATH, SoundType.HURT);

  private StoredDataTestHelper() {}

  public static void assertNpcWithoutStoredSoundsStillHasSounds(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    SoundDataCapable<?> soundData = (SoundDataCapable<?>) easyNPC;

    soundData.clearSoundDataSet();

    for (SoundType soundType : EXPECTED_SOUND_TYPES) {
      GameTestHelpers.assertTrue(
          helper,
          "An NPC without stored sounds must still resolve " + soundType,
          soundData.hasDefaultSound(soundType));
      GameTestHelpers.assertNotNull(
          helper,
          "An NPC without stored sounds must still resolve the sound event of " + soundType,
          soundData.getDefaultSoundEvent(soundType));
    }
  }

  public static void assertUnchangedSoundsAreNotStored(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ((SoundDataCapable<?>) easyNPC).clearSoundDataSet();

    CompoundTag storedData = ((PresetDataCapable<?>) easyNPC).serializePresetData();

    GameTestHelpers.assertTrue(
        helper,
        "An NPC without own sounds must not store the sound data of its variant",
        !storedData.contains(SoundDataCapable.EASY_NPC_DATA_SOUND_DATA_TAG));
  }

  public static void assertObjectivesSurviveWithoutTargetFlags(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag storedData = ((PresetDataCapable<?>) easyNPC).serializePresetData();

    EasyNPC<?> targetNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    ((PresetDataCapable<?>) targetNPC).importPresetData(storedData.copy());

    GameTestHelpers.assertNotNull(
        helper,
        "The standard objectives must survive without the stored target flags",
        targetNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_PLAYER));
    GameTestHelpers.assertTrue(
        helper,
        "The target flags of the objectives must be derived again",
        !targetNPC.getEasyNPCObjectiveData().hasPlayerTargetObjectives());
  }

  public static void assertUnchangedNpcStoresNoBoilerplate(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag storedData = ((PresetDataCapable<?>) easyNPC).serializePresetData();

    for (String unwantedTag :
        List.of("SoundData", "DisplayAttribute", "EntityAttribute", "Progression", "Profession")) {
      GameTestHelpers.assertTrue(
          helper,
          "An unchanged NPC must not store " + unwantedTag,
          !storedData.contains(unwantedTag));
    }
  }
}
