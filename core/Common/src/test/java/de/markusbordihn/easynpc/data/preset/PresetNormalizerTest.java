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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import java.util.UUID;
import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class PresetNormalizerTest {

  private static final String ATTRIBUTES_TAG = "attributes";
  private static final String ATTRIBUTE_NAME_TAG = "id";

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static CompoundTag attribute(String name) {
    CompoundTag attribute = new CompoundTag();
    attribute.putString(ATTRIBUTE_NAME_TAG, name);
    attribute.putDouble("base", 1.0);
    return attribute;
  }

  private static CompoundTag modifier(String name) {
    CompoundTag modifier = new CompoundTag();
    modifier.putString("id", name);
    modifier.putDouble("amount", 0.05D);
    modifier.putString("operation", "add_multiplied_base");
    return modifier;
  }

  private static CompoundTag actionEventData() {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG));
    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(ActionEventType.ON_INTERACTION, actionDataSet);

    CompoundTag entityData = new CompoundTag();
    entityData.put(ActionEventDataCapable.DATA_ACTION_DATA_TAG, actionEventSet.createTag());
    return entityData;
  }

  private static UUID firstActionId(CompoundTag entityData) {
    return new ActionEventSet(actionDataTag(entityData))
        .getActionEvents(ActionEventType.ON_INTERACTION)
        .getEntries()
        .iterator()
        .next()
        .id();
  }

  private static CompoundTag actionDataTag(CompoundTag entityData) {
    return entityData.getCompound(ActionEventDataCapable.DATA_ACTION_DATA_TAG);
  }

  private static ListTag actionEntries(CompoundTag entityData) {
    return actionDataTag(entityData)
        .getCompound(ActionEventSet.DATA_ACTION_EVENT_SET_TAG)
        .getList(ActionEventType.ON_INTERACTION.name(), Tag.TAG_COMPOUND);
  }

  @Test
  @DisplayName("The status timestamps are dropped with their now empty status data")
  void testStatusTimestampsAreDropped() {
    CompoundTag statusData = new CompoundTag();
    statusData.putLong(StatusDataType.NPC_DATA_LAST_UPDATE.getTagName(), 1234L);
    statusData.putLong(StatusDataType.NPC_DATA_LAST_SAVED.getTagName(), 5678L);
    CompoundTag entityData = new CompoundTag();
    entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusData);

    PresetNormalizer.normalize(entityData);

    assertFalse(entityData.contains(StatusDataCapable.DATA_STATUS_DATA_TAG));
  }

  @Test
  @DisplayName("The remaining status data is kept without its timestamps")
  void testRemainingStatusDataIsKept() {
    CompoundTag statusData = new CompoundTag();
    statusData.putLong(StatusDataType.NPC_DATA_LAST_UPDATE.getTagName(), 1234L);
    statusData.putBoolean("Finalized", true);
    CompoundTag entityData = new CompoundTag();
    entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusData);

    PresetNormalizer.normalize(entityData);

    CompoundTag normalizedStatusData =
        entityData.getCompound(StatusDataCapable.DATA_STATUS_DATA_TAG);
    assertFalse(normalizedStatusData.contains(StatusDataType.NPC_DATA_LAST_UPDATE.getTagName()));
    assertTrue(normalizedStatusData.getBoolean("Finalized"));
  }

  @Test
  @DisplayName("The fired spawn action of the exported NPC is dropped")
  void testSpawnActionFiredIsDropped() {
    CompoundTag statusData = new CompoundTag();
    statusData.putBoolean(StatusDataType.FINALIZED.getTagName(), true);
    statusData.putBoolean(StatusDataType.SPAWN_ACTION_FIRED.getTagName(), true);
    CompoundTag entityData = new CompoundTag();
    entityData.put(StatusDataCapable.DATA_STATUS_DATA_TAG, statusData);

    PresetNormalizer.normalize(entityData);

    CompoundTag normalizedStatusData =
        entityData.getCompound(StatusDataCapable.DATA_STATUS_DATA_TAG);
    assertFalse(normalizedStatusData.contains(StatusDataType.SPAWN_ACTION_FIRED.getTagName()));
    assertTrue(normalizedStatusData.getBoolean(StatusDataType.FINALIZED.getTagName()));
  }

  @Test
  @DisplayName("The randomized chicken egg timer is dropped")
  void testChickenEggTimerIsDropped() {
    CompoundTag entityData = new CompoundTag();
    entityData.putInt("EggLayTime", 4321);

    PresetNormalizer.normalize(entityData);

    assertFalse(entityData.contains("EggLayTime"));
  }

  @Test
  @DisplayName("The Forge-only entity update flag is dropped")
  void testForgeEntityUpdateFlagIsDropped() {
    CompoundTag entityData = new CompoundTag();
    entityData.putBoolean("CanUpdate", true);

    PresetNormalizer.normalize(entityData);

    assertFalse(entityData.contains("CanUpdate"));
  }

  @Test
  @DisplayName("The skin timestamp is dropped but the skin itself is kept")
  void testSkinTimestampIsDropped() {
    CompoundTag skinData = new CompoundTag();
    skinData.putLong(SkinDataEntry.DATA_TIMESTAMP_TAG, 1234L);
    skinData.putString("Type", "DEFAULT");
    CompoundTag entityData = new CompoundTag();
    entityData.put(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG, skinData);

    PresetNormalizer.normalize(entityData);

    CompoundTag normalizedSkinData =
        entityData.getCompound(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG);
    assertFalse(normalizedSkinData.contains(SkinDataEntry.DATA_TIMESTAMP_TAG));
    assertEquals("DEFAULT", normalizedSkinData.getString("Type"));
  }

  @Test
  @DisplayName("The attributes are sorted by name, so an export stays comparable")
  void testAttributesAreSorted() {
    ListTag attributes = new ListTag();
    attributes.add(attribute("minecraft:generic.movement_speed"));
    attributes.add(attribute("minecraft:generic.max_health"));
    attributes.add(attribute("minecraft:generic.attack_damage"));
    CompoundTag entityData = new CompoundTag();
    entityData.put(ATTRIBUTES_TAG, attributes);

    PresetNormalizer.normalize(entityData);

    ListTag normalizedAttributes = entityData.getList(ATTRIBUTES_TAG, Tag.TAG_COMPOUND);
    assertEquals(
        "minecraft:generic.attack_damage",
        normalizedAttributes.getCompound(0).getString(ATTRIBUTE_NAME_TAG));
    assertEquals(
        "minecraft:generic.max_health",
        normalizedAttributes.getCompound(1).getString(ATTRIBUTE_NAME_TAG));
    assertEquals(
        "minecraft:generic.movement_speed",
        normalizedAttributes.getCompound(2).getString(ATTRIBUTE_NAME_TAG));
  }

  @Test
  @DisplayName("An empty attribute list is dropped")
  void testEmptyAttributeListIsDropped() {
    CompoundTag entityData = new CompoundTag();
    entityData.put(ATTRIBUTES_TAG, new ListTag());

    PresetNormalizer.normalize(entityData);

    assertFalse(entityData.contains(ATTRIBUTES_TAG));
  }

  @Test
  @DisplayName("The action identifiers are dropped, the actions themselves are kept")
  void testActionIdentifiersAreDropped() {
    CompoundTag entityData = actionEventData();

    PresetNormalizer.normalize(entityData);

    ListTag actionEntries = actionEntries(entityData);
    assertEquals(1, actionEntries.size());
    assertFalse(actionEntries.getCompound(0).contains(ActionDataEntry.DATA_ID_TAG));
    assertEquals(
        ActionDataType.OPEN_DEFAULT_DIALOG.name(),
        actionEntries.getCompound(0).getString(ActionDataEntry.DATA_TYPE_TAG));
  }

  @Test
  @DisplayName("An exported action keeps the same identifier on every import")
  void testActionIdentifierStaysStableAfterExport() {
    CompoundTag entityData = actionEventData();

    PresetNormalizer.normalize(entityData);

    assertEquals(
        firstActionId(entityData),
        firstActionId(entityData.copy()),
        "An action without a stored identifier must derive the same identifier again");
  }

  @Test
  @DisplayName("Normalizing twice does not change the result")
  void testNormalizeIsIdempotent() {
    CompoundTag entityData = actionEventData();
    entityData.put(ATTRIBUTES_TAG, new ListTag());
    entityData.put(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG, new CompoundTag());

    PresetNormalizer.normalize(entityData);
    CompoundTag normalizedEntityData = entityData.copy();
    PresetNormalizer.normalize(entityData);

    assertEquals(normalizedEntityData, entityData);
  }

  @Test
  @DisplayName("Normalizing an empty or missing preset does not fail")
  void testEmptyPresetIsHandled() {
    CompoundTag entityData = new CompoundTag();

    PresetNormalizer.normalize(null);
    PresetNormalizer.normalize(entityData);

    assertTrue(entityData.isEmpty());
  }

  @Test
  @DisplayName("A modifier rolled per spawn is removed, an own modifier is kept")
  void testSpawnModifiersAreRemoved() {
    CompoundTag modifiers = new CompoundTag();
    modifiers.putString("id", "minecraft:generic.follow_range");
    modifiers.putDouble("base", 32.0D);
    ListTag modifierList = new ListTag();
    modifierList.add(modifier("minecraft:random_spawn_bonus"));
    modifierList.add(modifier("easy_npc:custom_range_bonus"));
    modifiers.put("modifiers", modifierList);
    ListTag attributeList = new ListTag();
    attributeList.add(modifiers);
    CompoundTag entityData = new CompoundTag();
    entityData.put("attributes", attributeList);

    PresetNormalizer.normalize(entityData);
    ListTag keptModifiers =
        entityData
            .getList("attributes", Tag.TAG_COMPOUND)
            .getCompound(0)
            .getList("modifiers", Tag.TAG_COMPOUND);

    assertEquals(1, keptModifiers.size());
    assertEquals("easy_npc:custom_range_bonus", keptModifiers.getCompound(0).getString("id"));
  }
}
