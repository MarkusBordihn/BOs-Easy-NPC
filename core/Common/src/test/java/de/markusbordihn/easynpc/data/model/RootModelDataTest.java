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

package de.markusbordihn.easynpc.data.model;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("RootModelData Tests")
class RootModelDataTest {

  @Test
  @DisplayName(
      "Locked rotation round-trips through NBT without data loss (regression for #722-style pose reset bug)")
  void testLockedRotationSurvivesNbtRoundTrip() {
    CustomRotation lockedRotation = new CustomRotation(0f, 90f, 0f, true);
    RootModelData original = new RootModelData(lockedRotation, CustomScale.DEFAULT);

    CompoundTag tag = original.save();
    RootModelData loaded = RootModelData.load(tag);

    assertEquals(0f, loaded.rotation().x());
    assertEquals(90f, loaded.rotation().y());
    assertEquals(0f, loaded.rotation().z());
    assertTrue(loaded.rotation().locked(), "locked flag must survive NBT save/load");
  }

  @Test
  void testScaleSurvivesNbtRoundTrip() {
    RootModelData original = new RootModelData(CustomRotation.DEFAULT, new CustomScale(2f, 2f, 2f));

    RootModelData loaded = RootModelData.load(original.save());

    assertEquals(2f, loaded.scale().x());
    assertEquals(2f, loaded.scale().y());
    assertEquals(2f, loaded.scale().z());
  }

  @Test
  void testHasChangedTrueForLockedZeroRotation() {
    RootModelData data =
        new RootModelData(new CustomRotation(0f, 0f, 0f, true), CustomScale.DEFAULT);
    assertTrue(
        data.hasChanged(),
        "A locked rotation must be treated as changed so it is always written to NBT");
  }

  @Test
  void testHasChangedFalseForDefault() {
    assertFalse(RootModelData.DEFAULT.hasChanged());
  }

  @Test
  void testHasChangedTrueForNonDefaultScale() {
    RootModelData data =
        new RootModelData(CustomRotation.DEFAULT, new CustomScale(1.5f, 1.5f, 1.5f));
    assertTrue(data.hasChanged());
  }

  @Test
  @DisplayName("Default scale is not written to NBT when only rotation changes")
  void testDefaultScaleOmittedFromNbt() {
    RootModelData data =
        new RootModelData(new CustomRotation(0f, 45f, 0f, true), CustomScale.DEFAULT);
    CompoundTag tag = data.save();
    assertFalse(tag.contains("Scale"), "Default scale should be omitted to keep NBT compact");
  }

  @Test
  void testLoadFromEmptyTagReturnsDefault() {
    RootModelData loaded = RootModelData.load(new CompoundTag());
    assertEquals(RootModelData.DEFAULT, loaded);
  }

  @Test
  void testIsRotationLocked() {
    assertTrue(
        new RootModelData(new CustomRotation(0f, 90f, 0f, true), CustomScale.DEFAULT)
            .isRotationLocked());
    assertFalse(
        new RootModelData(new CustomRotation(0f, 90f, 0f, false), CustomScale.DEFAULT)
            .isRotationLocked());
    assertFalse(RootModelData.DEFAULT.isRotationLocked());
  }
}
