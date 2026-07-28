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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.Mob;

public record NPCEntityMetadata(
    UUID ownerUUID,
    String entityType,
    String dimension,
    UUID presetUUID,
    Identifier customIdentifier,
    NPCRemovalReason removalReason,
    boolean restoreOnOwnerLogin) {

  public static final NPCEntityMetadata DEFAULT =
      new NPCEntityMetadata(null, null, null, null, null, NPCRemovalReason.NONE, false);

  public static final String TAG_DIMENSION = "Dimension";
  public static final String TAG_OWNER = "Owner";
  public static final String TAG_ENTITY_TYPE = "EntityType";
  public static final String TAG_PRESET_UUID = "PresetUUID";
  public static final String TAG_CUSTOM_IDENTIFIER = "CustomIdentifier";
  public static final String TAG_REMOVAL_REASON = "RemovalReason";
  public static final String TAG_RESTORE_ON_OWNER_LOGIN = "RestoreOnOwnerLogin";

  public static <T extends Mob> NPCEntityMetadata fromEasyNPC(EasyNPC<T> easyNPC) {
    if (easyNPC == null || easyNPC.getEntity() == null) {
      return DEFAULT;
    }

    UUID ownerUUID = null;
    if (easyNPC.getEasyNPCOwnerData() != null) {
      ownerUUID = easyNPC.getEasyNPCOwnerData().getOwnerUUID();
    }

    String entityType = easyNPC.getEntityTypeId();

    String dimension = null;
    if (easyNPC.getEntity().level() != null) {
      dimension = easyNPC.getEntity().level().dimension().toString();
    }

    UUID presetUUID = null;
    if (easyNPC.getEasyNPCPresetData() != null && easyNPC.getEasyNPCPresetData().hasPresetUUID()) {
      presetUUID = easyNPC.getEasyNPCPresetData().getPresetUUID();
    }

    Identifier customIdentifier = easyNPC.getCustomNPCIdentifier();

    return new NPCEntityMetadata(
        ownerUUID,
        entityType,
        dimension,
        presetUUID,
        customIdentifier,
        NPCRemovalReason.NONE,
        easyNPC.getEasyNPCPresetData() != null
            && easyNPC.getEasyNPCPresetData().getRestoreOnOwnerLogin());
  }

  public static NPCEntityMetadata fromCompoundTag(CompoundTag tag) {
    if (tag == null || tag.isEmpty()) {
      return DEFAULT;
    }

    UUID ownerUUID = tag.contains(TAG_OWNER) ? CompoundTagUtils.readUUID(tag, TAG_OWNER) : null;
    String entityType =
        tag.contains(TAG_ENTITY_TYPE) ? tag.getString(TAG_ENTITY_TYPE).orElse(null) : null;
    String dimension =
        tag.contains(TAG_DIMENSION) ? tag.getString(TAG_DIMENSION).orElse(null) : null;
    UUID presetUUID =
        tag.contains(TAG_PRESET_UUID) ? CompoundTagUtils.readUUID(tag, TAG_PRESET_UUID) : null;

    Identifier customIdentifier = null;
    if (tag.contains(TAG_CUSTOM_IDENTIFIER)) {
      try {
        customIdentifier = Identifier.parse(tag.getString(TAG_CUSTOM_IDENTIFIER).orElse(""));
      } catch (IllegalArgumentException e) {
        // Invalid resource location format
      }
    }

    NPCRemovalReason removalReason =
        tag.contains(TAG_REMOVAL_REASON)
            ? NPCRemovalReason.fromString(tag.getString(TAG_REMOVAL_REASON).orElse(""))
            : NPCRemovalReason.NONE;

    return new NPCEntityMetadata(
        ownerUUID,
        entityType,
        dimension,
        presetUUID,
        customIdentifier,
        removalReason,
        tag.getBoolean(TAG_RESTORE_ON_OWNER_LOGIN).orElse(false));
  }

  public CompoundTag toCompoundTag() {
    CompoundTag tag = new CompoundTag();
    if (ownerUUID != null) {
      CompoundTagUtils.writeUUID(tag, TAG_OWNER, ownerUUID);
    }
    if (entityType != null && !entityType.isEmpty()) {
      tag.putString(TAG_ENTITY_TYPE, entityType);
    }
    if (dimension != null && !dimension.isEmpty()) {
      tag.putString(TAG_DIMENSION, dimension);
    }
    if (presetUUID != null) {
      CompoundTagUtils.writeUUID(tag, TAG_PRESET_UUID, presetUUID);
    }
    if (customIdentifier != null) {
      tag.putString(TAG_CUSTOM_IDENTIFIER, customIdentifier.toString());
    }
    if (removalReason != null && removalReason != NPCRemovalReason.NONE) {
      tag.putString(TAG_REMOVAL_REASON, removalReason.name());
    }
    if (restoreOnOwnerLogin) {
      tag.putBoolean(TAG_RESTORE_ON_OWNER_LOGIN, true);
    }
    return tag;
  }

  public NPCEntityMetadata withOwnerUUID(UUID newOwnerUUID) {
    return new NPCEntityMetadata(
        newOwnerUUID,
        entityType,
        dimension,
        presetUUID,
        customIdentifier,
        removalReason,
        restoreOnOwnerLogin);
  }

  public NPCEntityMetadata withDimension(String newDimension) {
    return new NPCEntityMetadata(
        ownerUUID,
        entityType,
        newDimension,
        presetUUID,
        customIdentifier,
        removalReason,
        restoreOnOwnerLogin);
  }

  public NPCEntityMetadata withRemovalReason(NPCRemovalReason newRemovalReason) {
    return new NPCEntityMetadata(
        ownerUUID,
        entityType,
        dimension,
        presetUUID,
        customIdentifier,
        newRemovalReason,
        restoreOnOwnerLogin);
  }

  public boolean hasOwner() {
    return ownerUUID != null;
  }

  public boolean hasEntityType() {
    return entityType != null && !entityType.isEmpty();
  }

  public boolean hasDimension() {
    return dimension != null && !dimension.isEmpty();
  }

  public boolean hasPreset() {
    return presetUUID != null;
  }

  public boolean hasCustomIdentifier() {
    return customIdentifier != null;
  }

  public boolean hasRemovalReason() {
    return removalReason != null && removalReason != NPCRemovalReason.NONE;
  }
}
