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
    UUID id,
    ConditionType conditionType,
    ConditionOperationType operationType,
    String name,
    int value,
    String stringValue) {

  public static final ConditionDataEntry EMPTY =
      new ConditionDataEntry(ConditionType.NONE, ConditionOperationType.NONE);
  public static final String DATA_UUID_TAG = "UUID";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_OPERATION_TAG = "Operation";
  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_VALUE_TAG = "Value";
  public static final String DATA_STRING_VALUE_TAG = "StringValue";

  public ConditionDataEntry(CompoundTag compoundTag) {
    this(
        compoundTag.contains(DATA_UUID_TAG)
            ? compoundTag.getUUID(DATA_UUID_TAG)
            : UUID.randomUUID(),
        ConditionType.get(compoundTag.getString(DATA_TYPE_TAG)),
        ConditionOperationType.get(compoundTag.getString(DATA_OPERATION_TAG)),
        compoundTag.contains(DATA_NAME_TAG) ? compoundTag.getString(DATA_NAME_TAG) : "",
        compoundTag.contains(DATA_VALUE_TAG) ? compoundTag.getInt(DATA_VALUE_TAG) : 0,
        compoundTag.contains(DATA_STRING_VALUE_TAG)
            ? compoundTag.getString(DATA_STRING_VALUE_TAG)
            : "");
  }

  public ConditionDataEntry(ConditionType conditionType) {
    this(conditionType, ConditionOperationType.NONE);
  }

  public ConditionDataEntry(ConditionType conditionType, ConditionOperationType operationType) {
    this(UUID.randomUUID(), conditionType, operationType, "", 0, "");
  }

  public ConditionDataEntry(
      ConditionType conditionType, ConditionOperationType operationType, String name, int value) {
    this(UUID.randomUUID(), conditionType, operationType, name, value, "");
  }

  public UUID getId() {
    return this.id;
  }

  public boolean hasName() {
    return this.name != null && !this.name.isEmpty();
  }

  public boolean hasStringValue() {
    return this.stringValue != null && !this.stringValue.isEmpty();
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
      default -> true;
    };
  }

  public ConditionDataEntry withConditionType(ConditionType conditionType) {
    return new ConditionDataEntry(
        this.id, conditionType, this.operationType, this.name, this.value, this.stringValue);
  }

  public ConditionDataEntry withOperationType(ConditionOperationType operationType) {
    return new ConditionDataEntry(
        this.id, this.conditionType, operationType, this.name, this.value, this.stringValue);
  }

  public ConditionDataEntry withName(String name) {
    return new ConditionDataEntry(
        this.id, this.conditionType, this.operationType, name, this.value, this.stringValue);
  }

  public ConditionDataEntry withValue(int value) {
    return new ConditionDataEntry(
        this.id, this.conditionType, this.operationType, this.name, value, this.stringValue);
  }

  public ConditionDataEntry withStringValue(String stringValue) {
    return new ConditionDataEntry(
        this.id, this.conditionType, this.operationType, this.name, this.value, stringValue);
  }

  public ConditionDataEntry create(CompoundTag compoundTag) {
    return new ConditionDataEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putUUID(DATA_UUID_TAG, this.id);
    compoundTag.putString(DATA_TYPE_TAG, this.conditionType.name());

    if (this.operationType != null && this.operationType != ConditionOperationType.NONE) {
      compoundTag.putString(DATA_OPERATION_TAG, this.operationType.name());
    }
    if (hasName()) {
      compoundTag.putString(DATA_NAME_TAG, this.name.trim());
    }
    if (this.value != 0) {
      compoundTag.putInt(DATA_VALUE_TAG, this.value);
    }
    if (hasStringValue()) {
      compoundTag.putString(DATA_STRING_VALUE_TAG, this.stringValue);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }

  @Override
  public String toString() {
    return "ConditionDataEntry["
        + "id="
        + this.id
        + ", type="
        + this.conditionType
        + ", operation="
        + this.operationType
        + ", name='"
        + this.name
        + "'"
        + ", value="
        + this.value
        + ", stringValue='"
        + this.stringValue
        + "'"
        + "]";
  }
}
