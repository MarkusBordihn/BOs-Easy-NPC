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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import net.minecraft.core.Holder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.Attribute;

public interface AttributeData<E extends PathfinderMob> extends EasyNPC<E> {

  String EASY_NPC_DATA_ATTRIBUTE_CAN_BE_LEASHED_TAG = "CanBeLeashed";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG = "CanCloseDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG = "CanFloat";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG = "CanOpenDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG = "CanPassDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG = "CanUseNetherPortal";
  String EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG = "Freefall";
  String EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG = "IsAttackable";
  String EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG = "IsPushable";
  String EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG = "LightLevel";
  String EASY_NPC_DATA_ATTRIBUTE_PUSH_ENTITIES_TAG = "PushEntities";
  String EASY_NPC_DATA_ATTRIBUTE_TAG = "EntityAttribute";

  static void registerSyncedAttributeData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Attribute Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.ATTRIBUTE_DATA_LOADED,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_BE_LEASHED,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_FLOAT,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_CLOSE_DOOR,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_OPEN_DOOR,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_PASS_DOOR,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_CAN_USE_NETHER_PORTAL,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_FREEFALL,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_IS_ATTACKABLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_IS_PUSHABLE,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_PUSH_ENTITIES,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.BOOLEAN));
    map.put(
        SynchedDataIndex.ATTRIBUTE_LIGHT_LEVEL,
        SynchedEntityData.defineId(entityClass, EntityDataSerializers.INT));
  }

  default void setBaseAttribute(Holder<Attribute> attribute, double value) {
    if (attribute == null || getLivingEntity().getAttribute(attribute) == null) {
      return;
    }
    getLivingEntity().getAttribute(attribute).setBaseValue(value);
  }

  default double getBaseAttribute(Holder<Attribute> attribute) {
    if (attribute == null || getLivingEntity().getAttribute(attribute) == null) {
      return 0.0;
    }
    return getLivingEntity().getAttribute(attribute).getBaseValue();
  }

  default boolean getAttributeDataLoaded() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_DATA_LOADED);
  }

  default void setAttributeDataLoaded(boolean loaded) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_DATA_LOADED, loaded);
  }

  default boolean getAttributeCanBeLeashed() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_BE_LEASHED);
  }

  default void setAttributeCanBeLeashed(boolean canBeLeashed) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_BE_LEASHED, canBeLeashed);
  }

  default boolean getAttributeCanFloat() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_FLOAT);
  }

  default void setAttributeCanFloat(boolean canFloat) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_FLOAT, canFloat);
  }

  default boolean getAttributeCanCloseDoor() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_CLOSE_DOOR);
  }

  default void setAttributeCanCloseDoor(boolean canCloseDoor) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_CLOSE_DOOR, canCloseDoor);
  }

  default boolean getAttributeCanOpenDoor() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_OPEN_DOOR);
  }

  default void setAttributeCanOpenDoor(boolean canOpenDoor) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_OPEN_DOOR, canOpenDoor);
  }

  default boolean getAttributeCanPassDoor() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_PASS_DOOR);
  }

  default void setAttributeCanPassDoor(boolean canPassDoor) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_PASS_DOOR, canPassDoor);
  }

  default boolean getAttributeCanUseNetherPortal() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_USE_NETHER_PORTAL);
  }

  default void setAttributeCanUseNetherPortal(boolean canUseNetherPortal) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_CAN_USE_NETHER_PORTAL, canUseNetherPortal);
  }

  default boolean getAttributeFreefall() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_FREEFALL);
  }

  default void setAttributeFreefall(boolean freefall) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_FREEFALL, freefall);
  }

  default boolean getAttributeIsAttackable() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_IS_ATTACKABLE);
  }

  default void setAttributeIsAttackable(boolean isAttackable) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_IS_ATTACKABLE, isAttackable);
  }

  default boolean getAttributeIsPushable() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_IS_PUSHABLE);
  }

  default void setAttributeIsPushable(boolean isPushable) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_IS_PUSHABLE, isPushable);
  }

  default boolean getAttributePushEntities() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_PUSH_ENTITIES);
  }

  default void setAttributePushEntities(boolean pushEntities) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_PUSH_ENTITIES, pushEntities);
  }

  default int getAttributeLightLevel() {
    return getSynchedEntityData(SynchedDataIndex.ATTRIBUTE_LIGHT_LEVEL);
  }

  default void setAttributeLightLevel(int lightLevel) {
    setSynchedEntityData(SynchedDataIndex.ATTRIBUTE_LIGHT_LEVEL, lightLevel);
  }

  default boolean getAttributeSilent() {
    return getEntity().isSilent();
  }

  default void setAttributeSilent(boolean silent) {
    getEntity().setSilent(silent);
  }

  default void registerDefaultAttributeData(Enum<?> variant) {
    setAttributeDataLoaded(true);
  }

  default void defineSynchedAttributeData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_DATA_LOADED, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_BE_LEASHED, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_FLOAT, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_CLOSE_DOOR, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_OPEN_DOOR, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_PASS_DOOR, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_CAN_USE_NETHER_PORTAL, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_FREEFALL, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_IS_ATTACKABLE, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_IS_PUSHABLE, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_PUSH_ENTITIES, false);
    defineSynchedEntityData(builder, SynchedDataIndex.ATTRIBUTE_LIGHT_LEVEL, 7);
  }

  default void addAdditionalAttributeData(CompoundTag compoundTag) {
    CompoundTag attributeTag = new CompoundTag();
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_BE_LEASHED_TAG, getAttributeCanBeLeashed());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG, getAttributeCanFloat());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG, getAttributeCanCloseDoor());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG, getAttributeCanOpenDoor());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG, getAttributeCanPassDoor());
    attributeTag.putBoolean(
        EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG, getAttributeCanUseNetherPortal());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG, getAttributeFreefall());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG, getAttributeIsAttackable());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG, getAttributeIsPushable());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_PUSH_ENTITIES_TAG, getAttributePushEntities());
    attributeTag.putInt(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG, getAttributeLightLevel());
    compoundTag.put(EASY_NPC_DATA_ATTRIBUTE_TAG, attributeTag);
  }

  default void readAdditionalAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_ATTRIBUTE_TAG)) {
      return;
    }
    CompoundTag attributeTag = compoundTag.getCompound(EASY_NPC_DATA_ATTRIBUTE_TAG);
    setAttributeCanBeLeashed(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_BE_LEASHED_TAG));
    setAttributeCanFloat(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG));
    setAttributeCanCloseDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG));
    setAttributeCanOpenDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG));
    setAttributeCanPassDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG));
    setAttributeCanUseNetherPortal(
        attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG));
    setAttributeFreefall(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG));
    setAttributeIsAttackable(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG));
    setAttributeIsPushable(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG));
    setAttributePushEntities(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_PUSH_ENTITIES_TAG));
    setAttributeLightLevel(attributeTag.getInt(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG));

    setAttributeDataLoaded(true);
  }
}
