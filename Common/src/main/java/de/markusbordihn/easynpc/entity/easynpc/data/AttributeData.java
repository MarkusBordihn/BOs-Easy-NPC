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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attribute;

public interface AttributeData<T extends LivingEntity> extends EasyNPC<T> {

  // Synced entity data
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_DATA_LOADED =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_FREEFALL =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Integer> EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL =
      SynchedEntityData.defineId(EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.INT);

  // CompoundTags
  String EASY_NPC_DATA_ATTRIBUTE_TAG = "EntityAttribute";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG = "CanFloat";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG = "CanCloseDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG = "CanOpenDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG = "CanPassDoor";
  String EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG = "CanUseNetherPortal";
  String EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG = "Freefall";
  String EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG = "IsAttackable";
  String EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG = "IsPushable";
  String EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG = "LightLevel";

  default void setBaseAttribute(Attribute attribute, double value) {
    if (attribute == null || getEasyNPCEntity().getAttribute(attribute) == null) {
      return;
    }
    getEasyNPCEntity().getAttribute(attribute).setBaseValue(value);
  }

  default double getBaseAttribute(Attribute attribute) {
    if (attribute == null || getEasyNPCEntity().getAttribute(attribute) == null) {
      return 0.0;
    }
    return getEasyNPCEntity().getAttribute(attribute).getBaseValue();
  }

  default boolean getAttributeDataLoaded() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_DATA_LOADED);
  }

  default void setAttributeDataLoaded(boolean loaded) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_DATA_LOADED, loaded);
  }

  default boolean getAttributeCanFloat() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT);
  }

  default void setAttributeCanFloat(boolean canFloat) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT, canFloat);
  }

  default boolean getAttributeCanCloseDoor() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR);
  }

  default void setAttributeCanCloseDoor(boolean canCloseDoor) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR, canCloseDoor);
  }

  default boolean getAttributeCanOpenDoor() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR);
  }

  default void setAttributeCanOpenDoor(boolean canOpenDoor) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR, canOpenDoor);
  }

  default boolean getAttributeCanPassDoor() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR);
  }

  default void setAttributeCanPassDoor(boolean canPassDoor) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR, canPassDoor);
  }

  default boolean getAttributeCanUseNetherPortal() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL);
  }

  default void setAttributeCanUseNetherPortal(boolean canUseNetherPortal) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL, canUseNetherPortal);
  }

  default boolean getAttributeFreefall() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_FREEFALL);
  }

  default void setAttributeFreefall(boolean freefall) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_FREEFALL, freefall);
  }

  default boolean getAttributeIsAttackable() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE);
  }

  default void setAttributeIsAttackable(boolean isAttackable) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE, isAttackable);
  }

  default boolean getAttributeIsPushable() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE);
  }

  default void setAttributeIsPushable(boolean isPushable) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE, isPushable);
  }

  default int getAttributeLightLevel() {
    return getEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL);
  }

  default void setAttributeLightLevel(int lightLevel) {
    setEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL, lightLevel);
  }

  default void defineSynchedAttributeData() {
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_DATA_LOADED, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_FREEFALL, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE, false);
    defineEasyNPCData(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL, 7);
  }

  default void addAdditionalAttributeData(CompoundTag compoundTag) {
    CompoundTag attributeTag = new CompoundTag();
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG, getAttributeCanFloat());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG, getAttributeCanCloseDoor());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG, getAttributeCanOpenDoor());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG, getAttributeCanPassDoor());
    attributeTag.putBoolean(
        EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG, getAttributeCanUseNetherPortal());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG, getAttributeFreefall());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG, getAttributeIsAttackable());
    attributeTag.putBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG, getAttributeIsPushable());
    attributeTag.putInt(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG, getAttributeLightLevel());
    compoundTag.put(EASY_NPC_DATA_ATTRIBUTE_TAG, attributeTag);
  }

  default void readAdditionalAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_ATTRIBUTE_TAG)) {
      return;
    }
    CompoundTag attributeTag = compoundTag.getCompound(EASY_NPC_DATA_ATTRIBUTE_TAG);
    setAttributeCanFloat(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_FLOAT_TAG));
    setAttributeCanCloseDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG));
    setAttributeCanOpenDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG));
    setAttributeCanPassDoor(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG));
    setAttributeCanUseNetherPortal(
        attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_CAN_USE_NETHER_PORTAL_TAG));
    setAttributeFreefall(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_FREEFALL_TAG));
    setAttributeIsAttackable(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_ATTACKABLE_TAG));
    setAttributeIsPushable(attributeTag.getBoolean(EASY_NPC_DATA_ATTRIBUTE_IS_PUSHABLE_TAG));
    setAttributeLightLevel(attributeTag.getInt(EASY_NPC_DATA_ATTRIBUTE_LIGHT_LEVEL_TAG));

    setAttributeDataLoaded(true);
  }
}
