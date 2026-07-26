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

import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributes;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class EnvironmentalAttributeTestHelper {

  private static final String VANILLA_NO_GRAVITY_TAG = "NoGravity";

  private EnvironmentalAttributeTestHelper() {}

  public static void assertNoGravityIsAppliedOnPresetImport(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    importPresetWithNoGravity(easyNPC, true);
    EnvironmentalAttributes environmentalAttributes =
        easyNPC.getEasyNPCAttributeData().getEntityAttributes().getEnvironmentalAttributes();
    GameTestHelpers.assertTrue(
        helper,
        "Imported no gravity attribute must be stored on the NPC",
        environmentalAttributes.noGravity());
    GameTestHelpers.assertTrue(
        helper,
        "Imported no gravity attribute must disable the gravity of the NPC",
        easyNPC.getEntity().isNoGravity());
  }

  public static void assertNoGravityIsClearedOnPresetImport(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    AttributeHandler.setEnvironmentalAttribute(
        easyNPC, EnvironmentalAttributeType.NO_GRAVITY, true);

    importPresetWithNoGravity(easyNPC, false);
    GameTestHelpers.assertTrue(
        helper,
        "Imported preset without no gravity must restore the gravity of the NPC",
        !easyNPC.getEntity().isNoGravity());
  }

  private static void importPresetWithNoGravity(EasyNPC<?> easyNPC, boolean noGravity) {
    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();

    // Presets without the vanilla flag must still be resolved over the stored attribute.
    presetTag.remove(VANILLA_NO_GRAVITY_TAG);
    presetTag
        .getCompound(EntityAttributes.ENTITY_ATTRIBUTE_TAG)
        .putBoolean(EnvironmentalAttributes.NO_GRAVITY_TAG, noGravity);

    easyNPC.getEasyNPCPresetData().importPresetData(presetTag);
  }
}
