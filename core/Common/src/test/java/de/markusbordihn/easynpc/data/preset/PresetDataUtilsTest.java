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

package de.markusbordihn.easynpc.data.preset;

import static org.junit.jupiter.api.Assertions.*;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("PresetDataUtils Tests")
class PresetDataUtilsTest {

  @Test
  void testCleanupEntityDataRuntimeOnly() {
    CompoundTag dirtyData = new CompoundTag();
    dirtyData.putString("id", "minecraft:zombie");
    dirtyData.putFloat("AbsorptionAmount", 4.0f);
    dirtyData.putInt("Air", 300);
    dirtyData.putInt("DeathTime", 0);
    dirtyData.putDouble("FallDistance", 5.0);
    dirtyData.putInt("Fire", 100);
    dirtyData.putInt("HurtByTimestamp", 12345);
    dirtyData.putInt("HurtTime", 10);
    dirtyData.putBoolean("OnGround", true);

    dirtyData.put("Motion", new ListTag());
    dirtyData.put("Pos", new ListTag());
    dirtyData.put("Rotation", new ListTag());

    PresetDataUtils.cleanupEntityData(dirtyData, PresetDataUtils.CleanupMode.RUNTIME_ONLY);

    assertFalse(dirtyData.contains("AbsorptionAmount"), "AbsorptionAmount should be removed");
    assertFalse(dirtyData.contains("Air"), "Air should be removed");
    assertFalse(dirtyData.contains("DeathTime"), "DeathTime should be removed");
    assertFalse(dirtyData.contains("FallDistance"), "FallDistance should be removed");
    assertFalse(dirtyData.contains("Fire"), "Fire should be removed");
    assertFalse(dirtyData.contains("HurtByTimestamp"), "HurtByTimestamp should be removed");
    assertFalse(dirtyData.contains("HurtTime"), "HurtTime should be removed");
    assertFalse(dirtyData.contains("Motion"), "Motion should be removed");
    assertFalse(dirtyData.contains("OnGround"), "OnGround should be removed");

    assertTrue(dirtyData.contains("Pos"), "Pos should be preserved in RUNTIME_ONLY mode");
    assertTrue(dirtyData.contains("Rotation"), "Rotation should be preserved in RUNTIME_ONLY mode");
    assertTrue(dirtyData.contains("id"), "Entity ID should be preserved");
  }

  @Test
  void testCleanupEntityDataFull() {
    CompoundTag dirtyData = new CompoundTag();
    dirtyData.putString("id", "minecraft:zombie");
    dirtyData.putFloat("AbsorptionAmount", 4.0f);
    dirtyData.putInt("Air", 300);
    dirtyData.putInt("DeathTime", 0);
    dirtyData.putDouble("FallDistance", 5.0);
    dirtyData.putInt("Fire", 100);
    dirtyData.putInt("HurtByTimestamp", 12345);
    dirtyData.putInt("HurtTime", 10);
    dirtyData.putBoolean("OnGround", true);

    dirtyData.put("Motion", new ListTag());
    dirtyData.put("Pos", new ListTag());
    dirtyData.put("Rotation", new ListTag());

    PresetDataUtils.cleanupEntityData(dirtyData, PresetDataUtils.CleanupMode.FULL);

    assertFalse(dirtyData.contains("AbsorptionAmount"), "AbsorptionAmount should be removed");
    assertFalse(dirtyData.contains("Air"), "Air should be removed");
    assertFalse(dirtyData.contains("DeathTime"), "DeathTime should be removed");
    assertFalse(dirtyData.contains("FallDistance"), "FallDistance should be removed");
    assertFalse(dirtyData.contains("Fire"), "Fire should be removed");
    assertFalse(dirtyData.contains("HurtByTimestamp"), "HurtByTimestamp should be removed");
    assertFalse(dirtyData.contains("HurtTime"), "HurtTime should be removed");
    assertFalse(dirtyData.contains("Motion"), "Motion should be removed");
    assertFalse(dirtyData.contains("OnGround"), "OnGround should be removed");
    assertFalse(dirtyData.contains("Pos"), "Pos should be removed in FULL mode");
    assertFalse(dirtyData.contains("Rotation"), "Rotation should be removed in FULL mode");

    assertTrue(dirtyData.contains("id"), "Entity ID should be preserved");
  }

  @Test
  void testCleanupEntityDataDefaultMode() {
    CompoundTag dirtyData = new CompoundTag();
    dirtyData.putInt("Fire", 100);
    dirtyData.putInt("HurtTime", 10);

    dirtyData.put("Pos", new ListTag());

    PresetDataUtils.cleanupEntityData(dirtyData);

    assertFalse(dirtyData.contains("Fire"), "Fire should be removed");
    assertFalse(dirtyData.contains("HurtTime"), "HurtTime should be removed");
    assertTrue(dirtyData.contains("Pos"), "Pos should be preserved with default RUNTIME_ONLY mode");
  }

  @Test
  void testCleanupEntityDataNull() {
    CompoundTag result = PresetDataUtils.cleanupEntityData(null);
    assertNull(result, "Should return null for null input");
  }

  @Test
  void testCleanupEntityDataEmpty() {
    CompoundTag empty = new CompoundTag();
    CompoundTag result = PresetDataUtils.cleanupEntityData(empty);
    assertNotNull(result, "Should return non-null for empty tag");
    assertTrue(result.isEmpty(), "Should remain empty after cleanup");
  }

  @Test
  void testCleanupOnlySpecifiedTags() {
    CompoundTag data = new CompoundTag();
    data.putString("id", "minecraft:zombie");
    data.putString("CustomName", "Test");
    data.putInt("Fire", 100);
    data.putDouble("Health", 20.0);
    data.putInt("HurtTime", 5);

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);

    assertTrue(data.contains("id"), "ID should be preserved");
    assertTrue(data.contains("CustomName"), "CustomName should be preserved");
    assertTrue(data.contains("Health"), "Health should be preserved");
    assertFalse(data.contains("Fire"), "Fire should be removed");
    assertFalse(data.contains("HurtTime"), "HurtTime should be removed");
  }

  @Test
  void testCleanupAllRuntimeTags() {
    CompoundTag data = new CompoundTag();
    data.putFloat("AbsorptionAmount", 4.0f);
    data.putInt("Air", 300);
    data.putInt("DeathTime", 5);
    data.putDouble("FallDistance", 5.0);
    data.putInt("Fire", 100);
    data.putInt("HurtByTimestamp", 12345);
    data.putInt("HurtTime", 10);
    data.putInt("Motion", 1);
    data.putBoolean("OnGround", true);

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);

    assertEquals(
        0, data.size(), "All runtime tags should be removed, resulting in empty CompoundTag");
  }

  @Test
  void testPreserveImportantData() {
    CompoundTag data = new CompoundTag();
    data.putString("id", "minecraft:villager");
    data.putString("CustomName", "{\"text\":\"Trader\"}");
    data.putDouble("Health", 20.0);
    data.putInt("Fire", 100);

    CompoundTag attributes = new CompoundTag();
    attributes.putString("test", "value");
    data.put("Attributes", attributes);

    ListTag inventory = new ListTag();
    data.put("Inventory", inventory);

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);

    assertTrue(data.contains("id"));
    assertTrue(data.contains("CustomName"));
    assertTrue(data.contains("Health"));
    assertTrue(data.contains("Attributes"));
    assertTrue(data.contains("Inventory"));
    assertFalse(data.contains("Fire"));
  }

  @Test
  void testCleanupModifiesInPlace() {
    CompoundTag original = new CompoundTag();
    original.putInt("Fire", 100);

    CompoundTag result = PresetDataUtils.cleanupEntityData(original);

    assertSame(original, result, "Should return the same instance modified in place");
  }

  @Test
  void testIdempotentCleanup() {
    CompoundTag data = new CompoundTag();
    data.putString("id", "minecraft:zombie");
    data.putInt("Fire", 100);
    data.putInt("HurtTime", 10);

    data.put("Pos", new ListTag());

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);
    int sizeAfterFirstCleanup = data.size();

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);
    int sizeAfterSecondCleanup = data.size();

    assertEquals(
        sizeAfterFirstCleanup, sizeAfterSecondCleanup, "Multiple cleanups should be idempotent");
    assertTrue(data.contains("id"));
    assertTrue(data.contains("Pos"));
  }

  @Test
  void testComplexNestedStructures() {
    CompoundTag data = new CompoundTag();
    data.putString("id", "minecraft:zombie");
    data.putInt("Fire", 100);

    CompoundTag nestedData = new CompoundTag();
    nestedData.putInt("Fire", 50); // Fire in nested structure should NOT be removed
    data.put("CustomData", nestedData);

    ListTag listData = new ListTag();
    CompoundTag listItem = new CompoundTag();
    listItem.putInt("Fire", 25); // Fire in list items should NOT be removed
    listData.add(listItem);
    data.put("Items", listData);

    PresetDataUtils.cleanupEntityData(data, PresetDataUtils.CleanupMode.RUNTIME_ONLY);

    assertFalse(data.contains("Fire"), "Top-level Fire should be removed");
    assertTrue(data.contains("CustomData"), "CustomData should be preserved");
    assertTrue(data.contains("Items"), "Items should be preserved");

    CompoundTag preservedNested = data.getCompound("CustomData");
    assertTrue(preservedNested.contains("Fire"), "Fire in nested structures should be preserved");
  }

  @Test
  void testCleanupModeDifferences() {
    CompoundTag runtimeOnlyData = new CompoundTag();
    runtimeOnlyData.putInt("Fire", 100);
    runtimeOnlyData.put("Pos", new ListTag());
    runtimeOnlyData.put("Rotation", new ListTag());

    CompoundTag fullData = new CompoundTag();
    fullData.putInt("Fire", 100);
    fullData.put("Pos", new ListTag());
    fullData.put("Rotation", new ListTag());

    PresetDataUtils.cleanupEntityData(runtimeOnlyData, PresetDataUtils.CleanupMode.RUNTIME_ONLY);
    PresetDataUtils.cleanupEntityData(fullData, PresetDataUtils.CleanupMode.FULL);

    assertEquals(2, runtimeOnlyData.size(), "RUNTIME_ONLY should keep Pos and Rotation");
    assertEquals(0, fullData.size(), "FULL should remove everything including Pos and Rotation");
  }
}
