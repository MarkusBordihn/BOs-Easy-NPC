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

import java.nio.charset.StandardCharsets;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

public record ConditionDataEntry(
    ConditionType conditionType,
    ConditionSubTypeEntry subType,
    ConditionOperationType operationType,
    String name,
    int value,
    String customData,
    ResourceLocation customConditionId,
    UUID targetUUID) {

  public static final ConditionDataEntry EMPTY =
      new ConditionDataEntry(ConditionType.NONE, ConditionOperationType.NONE);
  public static final String DATA_TARGET_UUID_TAG = "TargetUUID";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_SUB_TYPE_TAG = "SubType";
  public static final String DATA_OPERATION_TAG = "Operation";
  public static final String DATA_NAME_TAG = "Name";
  public static final String DATA_CUSTOM_CONDITION_ID_TAG = "CustomConditionId";
  public static final String DATA_CUSTOM_DATA_TAG = "CustomData";
  public static final String DATA_LEGACY_TEXT_TAG = "Text";
  public static final String DATA_VALUE_TAG = "Value";
  public static final int MIN_CHANCE_PERCENTAGE = 1;
  public static final int MAX_CHANCE_PERCENTAGE = 100;

  public ConditionDataEntry(CompoundTag compoundTag) {
    this(
        getConditionType(compoundTag),
        getConditionType(compoundTag).getSubType(getSubTypeName(compoundTag)),
        ConditionOperationType.get(compoundTag.getString(DATA_OPERATION_TAG)),
        compoundTag.contains(DATA_NAME_TAG) ? compoundTag.getString(DATA_NAME_TAG) : "",
        compoundTag.contains(DATA_VALUE_TAG) ? compoundTag.getInt(DATA_VALUE_TAG) : 0,
        compoundTag.contains(DATA_CUSTOM_DATA_TAG)
            ? compoundTag.getString(DATA_CUSTOM_DATA_TAG)
            : "",
        compoundTag.contains(DATA_CUSTOM_CONDITION_ID_TAG)
            ? ResourceLocation.tryParse(compoundTag.getString(DATA_CUSTOM_CONDITION_ID_TAG))
            : null,
        compoundTag.contains(DATA_TARGET_UUID_TAG)
            ? compoundTag.getUUID(DATA_TARGET_UUID_TAG)
            : null);
  }

  public ConditionDataEntry(ConditionType conditionType) {
    this(conditionType, null, ConditionOperationType.NONE, "", 0);
  }

  public ConditionDataEntry(ConditionType conditionType, ConditionOperationType operationType) {
    this(conditionType, null, operationType, "", 0);
  }

  public ConditionDataEntry(ResourceLocation customConditionId) {
    this(
        ConditionType.CUSTOM,
        null,
        ConditionOperationType.NONE,
        "",
        0,
        "",
        customConditionId,
        null);
  }

  public ConditionDataEntry(
      ConditionType conditionType, ConditionOperationType operationType, String name, int value) {
    this(conditionType, null, operationType, name, value);
  }

  public ConditionDataEntry(
      ConditionType conditionType,
      ConditionSubTypeEntry subType,
      ConditionOperationType operationType,
      String name,
      int value) {
    this(conditionType, subType, operationType, name, value, "", null, null);
  }

  private static ConditionType getConditionType(CompoundTag compoundTag) {
    return ConditionType.get(compoundTag.getString(DATA_TYPE_TAG));
  }

  private static String getSubTypeName(CompoundTag compoundTag) {
    if (compoundTag.contains(DATA_SUB_TYPE_TAG)) {
      return compoundTag.getString(DATA_SUB_TYPE_TAG);
    }
    if (compoundTag.contains(DATA_LEGACY_TEXT_TAG)) {
      return compoundTag.getString(DATA_LEGACY_TEXT_TAG);
    }
    return "";
  }

  public UUID getId() {
    String identity =
        String.join(
            ":",
            this.conditionType != null ? this.conditionType.name() : "",
            this.subType instanceof Enum<?> subTypeEnum ? subTypeEnum.name() : "",
            this.operationType != null ? this.operationType.name() : "",
            this.name != null ? this.name.trim() : "",
            String.valueOf(this.value),
            this.customData != null ? this.customData.trim() : "",
            this.customConditionId != null ? this.customConditionId.toString() : "",
            this.targetUUID != null ? this.targetUUID.toString() : "");

    return UUID.nameUUIDFromBytes(identity.getBytes(StandardCharsets.UTF_8));
  }

  public boolean hasName() {
    return this.name != null && !this.name.isEmpty();
  }

  public boolean hasCustomConditionId() {
    return this.customConditionId != null;
  }

  public boolean hasCustomData() {
    return this.customData != null && !this.customData.trim().isEmpty();
  }

  public boolean hasTargetUUID() {
    return this.targetUUID != null;
  }

  public boolean hasValidUuidName() {
    if (!hasName()) {
      return false;
    }
    try {
      UUID.fromString(this.name.trim());
      return true;
    } catch (IllegalArgumentException ignored) {
      return false;
    }
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
      case CHANCE -> this.value >= MIN_CHANCE_PERCENTAGE && this.value <= MAX_CHANCE_PERCENTAGE;
      case HAS_ITEM_IN_INVENTORY, HAS_ITEM_IN_HAND, ADVANCEMENT, PLAYER_TAG, TEAM, GAMEMODE ->
          hasName();
      case EXPERIENCE_LEVEL, PLAYER_HEALTH, NPC_HEALTH ->
          this.operationType != null && this.operationType != ConditionOperationType.NONE;
      case ENTITY_HEALTH ->
          this.operationType != null
              && this.operationType != ConditionOperationType.NONE
              && hasValidUuidName();
      case TIME_OF_DAY ->
          this.operationType != null && this.operationType != ConditionOperationType.NONE;
      case WEATHER, RELATIONSHIP -> this.subType != null;
      case NPC_STATE ->
          hasName()
              && this.operationType != null
              && this.operationType != ConditionOperationType.NONE;
      case CUSTOM -> hasCustomConditionId();
      case FALLBACK -> true;
      default -> true;
    };
  }

  public ConditionDataEntry withConditionType(ConditionType conditionType) {
    return new ConditionDataEntry(
        conditionType,
        this.subType,
        this.operationType,
        this.name,
        this.value,
        this.customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withSubType(ConditionSubTypeEntry subType) {
    return new ConditionDataEntry(
        this.conditionType,
        subType,
        this.operationType,
        this.name,
        this.value,
        this.customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withOperationType(ConditionOperationType operationType) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        operationType,
        this.name,
        this.value,
        this.customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withName(String name) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        this.operationType,
        name,
        this.value,
        this.customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withValue(int value) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        this.operationType,
        this.name,
        value,
        this.customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withCustomData(String customData) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        this.operationType,
        this.name,
        this.value,
        customData,
        this.customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withCustomConditionId(ResourceLocation customConditionId) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        this.operationType,
        this.name,
        this.value,
        this.customData,
        customConditionId,
        this.targetUUID);
  }

  public ConditionDataEntry withTargetUUID(UUID targetUUID) {
    return new ConditionDataEntry(
        this.conditionType,
        this.subType,
        this.operationType,
        this.name,
        this.value,
        this.customData,
        this.customConditionId,
        targetUUID);
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
    if (hasCustomConditionId()) {
      compoundTag.putString(DATA_CUSTOM_CONDITION_ID_TAG, this.customConditionId.toString());
    }
    if (hasCustomData()) {
      compoundTag.putString(DATA_CUSTOM_DATA_TAG, this.customData.trim());
    }
    if (hasTargetUUID()) {
      compoundTag.putUUID(DATA_TARGET_UUID_TAG, this.targetUUID);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }
}
