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

package de.markusbordihn.easynpc.data.attribute;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.junit.jupiter.api.Test;

class LegacyAttributeConverterTest {

  private static CompoundTag legacyEntry(String name, double base) {
    CompoundTag entry = new CompoundTag();
    entry.putString("Name", name);
    entry.putDouble("Base", base);
    return entry;
  }

  @Test
  void testConvertLegacyAttributes() {
    CompoundTag legacyModifier = new CompoundTag();
    legacyModifier.putString("Name", "Random spawn bonus");
    legacyModifier.putDouble("Amount", -0.06d);
    legacyModifier.putInt("Operation", 1);
    CompoundTag followRange = legacyEntry("minecraft:generic.follow_range", 32.0d);
    ListTag modifiers = new ListTag();
    modifiers.add(legacyModifier);
    followRange.put("Modifiers", modifiers);

    ListTag legacyList = new ListTag();
    legacyList.add(followRange);
    legacyList.add(legacyEntry("minecraft:generic.max_health", 1000.0d));

    CompoundTag entityData = new CompoundTag();
    entityData.put("Attributes", legacyList);

    assertTrue(LegacyAttributeConverter.convertLegacyAttributes(entityData));
    assertFalse(entityData.contains("Attributes"));
    assertTrue(entityData.contains("attributes"));

    ListTag attributes = entityData.getListOrEmpty("attributes");
    assertEquals(2, attributes.size());

    CompoundTag convertedFollowRange = attributes.getCompound(0).orElse(new CompoundTag());
    assertEquals("minecraft:follow_range", convertedFollowRange.getString("id").orElse(""));
    assertEquals(32.0d, convertedFollowRange.getDouble("base").orElse(0.0));
    assertFalse(convertedFollowRange.contains("Name"));
    assertFalse(convertedFollowRange.contains("Base"));
    assertFalse(convertedFollowRange.contains("modifiers"));
    assertFalse(convertedFollowRange.contains("Modifiers"));

    CompoundTag convertedMaxHealth = attributes.getCompound(1).orElse(new CompoundTag());
    assertEquals("minecraft:max_health", convertedMaxHealth.getString("id").orElse(""));
    assertEquals(1000.0d, convertedMaxHealth.getDouble("base").orElse(0.0));
  }

  @Test
  void testLegacyAttributeIdsAreMappedToModernRegistry() {
    assertEquals(
        "minecraft:max_health",
        LegacyAttributeConverter.convertAttributeId("minecraft:generic.max_health"));
    assertEquals(
        "minecraft:movement_speed",
        LegacyAttributeConverter.convertAttributeId("minecraft:generic.movement_speed"));
    assertEquals(
        "minecraft:jump_strength",
        LegacyAttributeConverter.convertAttributeId("minecraft:horse.jump_strength"));
    assertEquals(
        "minecraft:spawn_reinforcements",
        LegacyAttributeConverter.convertAttributeId("minecraft:zombie.spawn_reinforcements"));
    assertEquals(
        "minecraft:gravity", LegacyAttributeConverter.convertAttributeId("forge:entity_gravity"));
    assertEquals(
        "minecraft:step_height",
        LegacyAttributeConverter.convertAttributeId("forge:step_height_addition"));
    // Already-modern and unknown/third-party ids are left untouched.
    assertEquals(
        "minecraft:max_health",
        LegacyAttributeConverter.convertAttributeId("minecraft:max_health"));
    assertEquals(
        "epicfight:stun_armor",
        LegacyAttributeConverter.convertAttributeId("epicfight:stun_armor"));
  }

  @Test
  void testCurrentFormatIsNotTouched() {
    CompoundTag modernEntry = new CompoundTag();
    modernEntry.putString("id", "minecraft:max_health");
    modernEntry.putDouble("base", 1024.0d);
    ListTag modernList = new ListTag();
    modernList.add(modernEntry);
    CompoundTag entityData = new CompoundTag();
    entityData.put("attributes", modernList);

    assertFalse(LegacyAttributeConverter.convertLegacyAttributes(entityData));
    assertEquals(1, entityData.getListOrEmpty("attributes").size());
    assertEquals(
        1024.0d,
        entityData
            .getListOrEmpty("attributes")
            .getCompound(0)
            .orElse(new CompoundTag())
            .getDouble("base")
            .orElse(0.0));
  }

  @Test
  void testModernStructureWithLegacyIdsIsNormalized() {
    CompoundTag legacyIdEntry = new CompoundTag();
    legacyIdEntry.putString("id", "minecraft:generic.max_health");
    legacyIdEntry.putDouble("base", 1000.0d);
    ListTag modernList = new ListTag();
    modernList.add(legacyIdEntry);
    CompoundTag entityData = new CompoundTag();
    entityData.put("attributes", modernList);

    assertTrue(LegacyAttributeConverter.convertLegacyAttributes(entityData));
    CompoundTag normalized =
        entityData.getListOrEmpty("attributes").getCompound(0).orElse(new CompoundTag());
    assertEquals("minecraft:max_health", normalized.getString("id").orElse(""));
    assertEquals(1000.0d, normalized.getDouble("base").orElse(0.0));
  }

  @Test
  void testEntryWithoutNameIsSkipped() {
    CompoundTag namelessEntry = new CompoundTag();
    namelessEntry.putDouble("Base", 5.0d);
    ListTag legacyList = new ListTag();
    legacyList.add(namelessEntry);
    legacyList.add(legacyEntry("minecraft:generic.armor", 2.0d));
    CompoundTag entityData = new CompoundTag();
    entityData.put("Attributes", legacyList);

    assertTrue(LegacyAttributeConverter.convertLegacyAttributes(entityData));
    ListTag attributes = entityData.getListOrEmpty("attributes");
    assertEquals(1, attributes.size());
    assertEquals(
        "minecraft:armor",
        attributes.getCompound(0).orElse(new CompoundTag()).getString("id").orElse(""));
  }

  @Test
  void testNoAttributesIsNoOp() {
    assertFalse(LegacyAttributeConverter.convertLegacyAttributes(new CompoundTag()));
    assertFalse(LegacyAttributeConverter.convertLegacyAttributes(null));
  }
}
