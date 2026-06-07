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

package de.markusbordihn.easynpc.data.condition;

import java.util.UUID;
import net.minecraft.nbt.CompoundTag;

public record ConditionDataEntry(
    ConditionType conditionType,
    ConditionSubTypeEntry subType,
    ConditionOperationType operationType,
    String name,
    int value) {

  public static final ConditionDataEntry EMPTY =
      new ConditionDataEntry(ConditionType.NONE, ConditionOperationType.NONE);
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_SUB_TYPE_TAG = "SubType";
  public static final String DATA_OPERATION_TAG = "Operation";
  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_LEGACY_TEXT_TAG = "Text";
  public static final String DATA_VALUE_TAG = "Value";

  public ConditionDataEntry(CompoundTag compoundTag) {
    this(
        getConditionType(compoundTag),
        getConditionType(compoundTag).getSubType(getSubTypeName(compoundTag)),
        ConditionOperationType.get(compoundTag.getString(DATA_OPERATION_TAG).orElse("")),
        compoundTag.getString(DATA_NAME_TAG).orElse(""),
        compoundTag.getInt(DATA_VALUE_TAG).orElse(0));
  }

  public ConditionDataEntry(ConditionType conditionType) {
    this(conditionType, null, ConditionOperationType.NONE, "", 0);
  }

  public ConditionDataEntry(ConditionType conditionType, ConditionOperationType operationType) {
    this(conditionType, null, operationType, "", 0);
  }

  public ConditionDataEntry(
      ConditionType conditionType, ConditionOperationType operationType, String name, int value) {
    this(conditionType, null, operationType, name, value);
  }

  private static ConditionType getConditionType(CompoundTag compoundTag) {
    return ConditionType.get(compoundTag.getString(DATA_TYPE_TAG).orElse(""));
  }

  private static String getSubTypeName(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_SUB_TYPE_TAG)) {
      return compoundTag.getString(DATA_SUB_TYPE_TAG).orElse("");
    }
    if (compoundTag.contains(DATA_LEGACY_TEXT_TAG)) {
      return compoundTag.getString(DATA_LEGACY_TEXT_TAG).orElse("");
    }
    return "";
  }

  public UUID getId() {
    String idString = DATA_TYPE_TAG + hashCode();
    return UUID.nameUUIDFromBytes(idString.getBytes());
  }

  public boolean hasName() {
    return this.name != null && !this.name.isEmpty();
  }

  public boolean isValid() {
    if (this.conditionType == ConditionType.NONE) {
      return false;
    }
    return switch (this.conditionType) {
      case SCOREBOARD ->
          hasName()
              && this.operationType != null
              && this.operationType != ConditionOperationType.NONE;
      case EXECUTION_LIMIT -> this.value > 0 && this.subType != null;
      case HAS_ITEM_IN_INVENTORY, HAS_ITEM_IN_HAND, ADVANCEMENT, PLAYER_TAG, TEAM, GAMEMODE ->
          hasName();
      case EXPERIENCE_LEVEL, PLAYER_HEALTH ->
          this.operationType != null && this.operationType != ConditionOperationType.NONE;
      case FALLBACK -> true;
      default -> true;
    };
  }

  public ConditionDataEntry withConditionType(ConditionType conditionType) {
    return new ConditionDataEntry(
        conditionType, this.subType, this.operationType, this.name, this.value);
  }

  public ConditionDataEntry withSubType(ConditionSubTypeEntry subType) {
    return new ConditionDataEntry(
        this.conditionType, subType, this.operationType, this.name, this.value);
  }

  public ConditionDataEntry withOperationType(ConditionOperationType operationType) {
    return new ConditionDataEntry(
        this.conditionType, this.subType, operationType, this.name, this.value);
  }

  public ConditionDataEntry withName(String name) {
    return new ConditionDataEntry(
        this.conditionType, this.subType, this.operationType, name, this.value);
  }

  public ConditionDataEntry withValue(int value) {
    return new ConditionDataEntry(
        this.conditionType, this.subType, this.operationType, this.name, value);
  }

  public ConditionDataEntry create(CompoundTag compoundTag) {
    return new ConditionDataEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TYPE_TAG, this.conditionType.name());

    if (this.subType != null) {
      compoundTag.putString(DATA_SUB_TYPE_TAG, ((Enum<?>) this.subType).name());
    }
    if (this.operationType != null && this.operationType != ConditionOperationType.NONE) {
      compoundTag.putString(DATA_OPERATION_TAG, this.operationType.name());
    }
    if (hasName()) {
      compoundTag.putString(DATA_NAME_TAG, this.name.trim());
    }
    if (this.value != 0) {
      compoundTag.putInt(DATA_VALUE_TAG, this.value);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }
}
