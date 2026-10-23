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

package de.markusbordihn.easynpc.data.npc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import java.util.Comparator;
import net.minecraft.SharedConstants;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.DoubleTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.attributes.DefaultAttributes;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SavedNPCEntityEntryTest {

  private static final String STATUS_TAG = StatusDataCapable.DATA_STATUS_DATA_TAG;
  private static final String LAST_UPDATE_TAG = StatusDataType.NPC_DATA_LAST_UPDATE.getTagName();
  private static final String LAST_SAVED_TAG = StatusDataType.NPC_DATA_LAST_SAVED.getTagName();

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag createNPCData() {
    CompoundTag statusTag = new CompoundTag();
    statusTag.putLong(LAST_UPDATE_TAG, 1000L);
    statusTag.putLong(LAST_SAVED_TAG, 1000L);
    CompoundTag npcData = new CompoundTag();
    npcData.putString("CustomName", "Storage NPC");
    npcData.putFloat("Health", 20.0f);
    npcData.put(STATUS_TAG, statusTag);
    npcData.put("Pos", createDoubleList(1.0, 2.0, 3.0));
    npcData.put("Motion", createDoubleList(0.0, -0.0784, 0.0));
    npcData.put("Rotation", createFloatList(0.0f, 0.0f));
    npcData.putBoolean("OnGround", true);
    npcData.putDouble("fall_distance", 0.0);
    return npcData;
  }

  private static ListTag createDoubleList(double... values) {
    ListTag listTag = new ListTag();
    for (double value : values) {
      listTag.add(DoubleTag.valueOf(value));
    }
    return listTag;
  }

  private static ListTag createFloatList(float... values) {
    ListTag listTag = new ListTag();
    for (float value : values) {
      listTag.add(FloatTag.valueOf(value));
    }
    return listTag;
  }

  private static CompoundTag createZombieNPCData() {
    CompoundTag npcData = createNPCData();
    npcData.putString("id", "minecraft:zombie");
    npcData.put("attributes", new ListTag());
    return npcData;
  }

  private static CompoundTag createMovementSpeedAttribute(double baseValue) {
    CompoundTag attributeTag = new CompoundTag();
    attributeTag.putString(
        "id", BuiltInRegistries.ATTRIBUTE.getKey(Attributes.MOVEMENT_SPEED.value()).toString());
    attributeTag.putDouble("base", baseValue);
    return attributeTag;
  }

  private static double getZombieDefaultMovementSpeed() {
    return DefaultAttributes.getSupplier(EntityTypes.ZOMBIE).getBaseValue(Attributes.MOVEMENT_SPEED);
  }

  private static CompoundTag withAttribute(CompoundTag npcData, CompoundTag attributeTag) {
    npcData.getListOrEmpty("attributes").add(attributeTag);
    return npcData;
  }

  private static long hash(CompoundTag npcData) {
    return SavedNPCEntityEntry.computeNPCDataHash(npcData);
  }

  @Test
  @DisplayName("Equal NPC data has the same hash")
  void testEqualDataHasSameHash() {
    assertEquals(hash(createNPCData()), hash(createNPCData()));
  }

  @Test
  @DisplayName("Position, motion, rotation and ground state do not change the hash")
  void testMovementDoesNotChangeHash() {
    CompoundTag movedData = createNPCData();
    movedData.put("Pos", createDoubleList(100.0, 64.0, -300.0));
    movedData.put("Motion", createDoubleList(0.2, 0.0, -0.1));
    movedData.put("Rotation", createFloatList(90.0f, 15.0f));
    movedData.putBoolean("OnGround", false);
    movedData.putDouble("fall_distance", 2.5);
    assertEquals(hash(createNPCData()), hash(movedData));
  }

  @Test
  @DisplayName("Last update and last saved timestamps do not change the hash")
  void testSaveTimestampsDoNotChangeHash() {
    CompoundTag savedData = createNPCData();
    CompoundTag statusTag = savedData.getCompoundOrEmpty(STATUS_TAG);
    statusTag.putLong(LAST_UPDATE_TAG, 5000L);
    statusTag.putLong(LAST_SAVED_TAG, 6000L);
    assertEquals(hash(createNPCData()), hash(savedData));
  }

  @Test
  @DisplayName("Other status values change the hash")
  void testOtherStatusValueChangesHash() {
    CompoundTag changedData = createNPCData();
    changedData
        .getCompoundOrEmpty(STATUS_TAG)
        .putLong(StatusDataType.SHOULDER_REMOUNT_ALLOWED_GAME_TIME.getTagName(), 200L);
    assertNotEquals(hash(createNPCData()), hash(changedData));
  }

  @Test
  @DisplayName("A changed custom name changes the hash")
  void testChangedNameChangesHash() {
    CompoundTag changedData = createNPCData();
    changedData.putString("CustomName", "Renamed NPC");
    assertNotEquals(hash(createNPCData()), hash(changedData));
  }

  @Test
  @DisplayName("A vanilla tag changes the hash")
  void testVanillaTagChangesHash() {
    CompoundTag changedData = createNPCData();
    ListTag tags = new ListTag();
    tags.add(StringTag.valueOf("storage_test"));
    changedData.put("Tags", tags);
    assertNotEquals(hash(createNPCData()), hash(changedData));
  }

  @Test
  @DisplayName("Key insertion order does not change the hash")
  void testInsertionOrderDoesNotChangeHash() {
    CompoundTag npcData = createNPCData();
    CompoundTag reorderedData = new CompoundTag();
    npcData.keySet().stream()
        .sorted(Comparator.reverseOrder())
        .forEach(key -> reorderedData.put(key, npcData.get(key).copy()));
    assertEquals(hash(npcData), hash(reorderedData));
  }

  @Test
  @DisplayName("Hashing leaves the given NPC data unchanged")
  void testHashKeepsInputUnchanged() {
    CompoundTag npcData = createNPCData();
    hash(npcData);
    assertEquals(createNPCData(), npcData);
  }

  @Test
  @DisplayName("A lazily created attribute with the entity type default does not change the hash")
  void testDefaultAttributeDoesNotChangeHash() {
    CompoundTag tickedData =
        withAttribute(
            createZombieNPCData(), createMovementSpeedAttribute(getZombieDefaultMovementSpeed()));
    assertEquals(hash(createZombieNPCData()), hash(tickedData));
  }

  @Test
  @DisplayName("An attribute with a changed base value changes the hash")
  void testChangedAttributeBaseChangesHash() {
    CompoundTag changedData =
        withAttribute(
            createZombieNPCData(),
            createMovementSpeedAttribute(getZombieDefaultMovementSpeed() + 0.1));
    assertNotEquals(hash(createZombieNPCData()), hash(changedData));
  }

  @Test
  @DisplayName("An attribute with modifiers changes the hash")
  void testAttributeModifierChangesHash() {
    CompoundTag attributeTag = createMovementSpeedAttribute(getZombieDefaultMovementSpeed());
    attributeTag.put("modifiers", new ListTag());
    CompoundTag changedData = withAttribute(createZombieNPCData(), attributeTag);
    assertNotEquals(hash(createZombieNPCData()), hash(changedData));
  }
}
