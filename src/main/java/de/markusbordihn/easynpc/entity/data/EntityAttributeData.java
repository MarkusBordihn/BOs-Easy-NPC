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

package de.markusbordihn.easynpc.entity.data;

import de.markusbordihn.easynpc.entity.EasyNPCEntityData;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;

public interface EntityAttributeData extends EntityDataInterface {

  // Synced entity data
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_CAN_FLOAT =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_CAN_CLOSE_DOOR =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_CAN_OPEN_DOOR =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_CAN_PASS_DOOR =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_FREEFALL =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_IS_ATTACKABLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);
  EntityDataAccessor<Boolean> DATA_ATTRIBUTE_IS_PUSHABLE =
      SynchedEntityData.defineId(EasyNPCEntityData.class, EntityDataSerializers.BOOLEAN);

  // CompoundTags
  String DATA_ATTRIBUTE_TAG = "EntityAttribute";
  String DATA_ATTRIBUTE_CAN_FLOAT_TAG = "CanFloat";
  String DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG = "CanCloseDoor";
  String DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG = "CanOpenDoor";
  String DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG = "CanPassDoor";
  String DATA_ATTRIBUTE_FREEFALL_TAG = "Freefall";
  String DATA_ATTRIBUTE_IS_ATTACKABLE_TAG = "IsAttackable";
  String DATA_ATTRIBUTE_IS_PUSHABLE_TAG = "IsPushable";

  default boolean getAttributeCanFloat() {
    return getEntityData(DATA_ATTRIBUTE_CAN_FLOAT);
  }

  default void setAttributeCanFloat(boolean canFloat) {
    setEntityData(DATA_ATTRIBUTE_CAN_FLOAT, canFloat);
  }

  default boolean getAttributeCanCloseDoor() {
    return getEntityData(DATA_ATTRIBUTE_CAN_CLOSE_DOOR);
  }

  default void setAttributeCanCloseDoor(boolean canCloseDoor) {
    setEntityData(DATA_ATTRIBUTE_CAN_CLOSE_DOOR, canCloseDoor);
  }

  default boolean getAttributeCanOpenDoor() {
    return getEntityData(DATA_ATTRIBUTE_CAN_OPEN_DOOR);
  }

  default void setAttributeCanOpenDoor(boolean canOpenDoor) {
    setEntityData(DATA_ATTRIBUTE_CAN_OPEN_DOOR, canOpenDoor);
  }

  default boolean getAttributeCanPassDoor() {
    return getEntityData(DATA_ATTRIBUTE_CAN_PASS_DOOR);
  }

  default void setAttributeCanPassDoor(boolean canPassDoor) {
    setEntityData(DATA_ATTRIBUTE_CAN_PASS_DOOR, canPassDoor);
  }

  default boolean getAttributeFreefall() {
    return getEntityData(DATA_ATTRIBUTE_FREEFALL);
  }

  default void setAttributeFreefall(boolean freefall) {
    setEntityData(DATA_ATTRIBUTE_FREEFALL, freefall);
  }

  default boolean getAttributeIsAttackable() {
    return getEntityData(DATA_ATTRIBUTE_IS_ATTACKABLE);
  }

  default void setAttributeIsAttackable(boolean isAttackable) {
    setEntityData(DATA_ATTRIBUTE_IS_ATTACKABLE, isAttackable);
  }

  default boolean getAttributeIsPushable() {
    return getEntityData(DATA_ATTRIBUTE_IS_PUSHABLE);
  }

  default void setAttributeIsPushable(boolean isPushable) {
    setEntityData(DATA_ATTRIBUTE_IS_PUSHABLE, isPushable);
  }

  default void defineSynchedAttributeData() {
    defineEntityData(DATA_ATTRIBUTE_CAN_FLOAT, true);
    defineEntityData(DATA_ATTRIBUTE_CAN_CLOSE_DOOR, true);
    defineEntityData(DATA_ATTRIBUTE_CAN_OPEN_DOOR, true);
    defineEntityData(DATA_ATTRIBUTE_CAN_PASS_DOOR, true);
    defineEntityData(DATA_ATTRIBUTE_FREEFALL, false);
    defineEntityData(DATA_ATTRIBUTE_IS_ATTACKABLE, false);
    defineEntityData(DATA_ATTRIBUTE_IS_PUSHABLE, false);
  }

  default void addAdditionalAttributeData(CompoundTag compoundTag) {
    CompoundTag attributeTag = new CompoundTag();
    attributeTag.putBoolean(DATA_ATTRIBUTE_CAN_FLOAT_TAG, getAttributeCanFloat());
    attributeTag.putBoolean(DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG, getAttributeCanCloseDoor());
    attributeTag.putBoolean(DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG, getAttributeCanOpenDoor());
    attributeTag.putBoolean(DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG, getAttributeCanPassDoor());
    attributeTag.putBoolean(DATA_ATTRIBUTE_FREEFALL_TAG, getAttributeFreefall());
    attributeTag.putBoolean(DATA_ATTRIBUTE_IS_ATTACKABLE_TAG, getAttributeIsAttackable());
    attributeTag.putBoolean(DATA_ATTRIBUTE_IS_PUSHABLE_TAG, getAttributeIsPushable());
    compoundTag.put(DATA_ATTRIBUTE_TAG, attributeTag);
  }

  default void readAdditionalAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_ATTRIBUTE_TAG)) {
      return;
    }
    CompoundTag attributeTag = compoundTag.getCompound(DATA_ATTRIBUTE_TAG);
    setAttributeCanFloat(attributeTag.getBoolean(DATA_ATTRIBUTE_CAN_FLOAT_TAG));
    setAttributeCanCloseDoor(attributeTag.getBoolean(DATA_ATTRIBUTE_CAN_CLOSE_DOOR_TAG));
    setAttributeCanOpenDoor(attributeTag.getBoolean(DATA_ATTRIBUTE_CAN_OPEN_DOOR_TAG));
    setAttributeCanPassDoor(attributeTag.getBoolean(DATA_ATTRIBUTE_CAN_PASS_DOOR_TAG));
    setAttributeFreefall(attributeTag.getBoolean(DATA_ATTRIBUTE_FREEFALL_TAG));
    setAttributeIsAttackable(attributeTag.getBoolean(DATA_ATTRIBUTE_IS_ATTACKABLE_TAG));
    setAttributeIsPushable(attributeTag.getBoolean(DATA_ATTRIBUTE_IS_PUSHABLE_TAG));
  }
}
