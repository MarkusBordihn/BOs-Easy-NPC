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

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelRotationDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  CustomRotation DEFAULT_MODEL_PART_ROTATION = new CustomRotation(0, 0, 0);
  String EASY_NPC_DATA_MODEL_ROTATION_TAG = "Rotation";

  StreamCodec<RegistryFriendlyByteBuf, Map<ModelPartType, CustomRotation>>
      MODEL_PART_ROTATION_STREAM_CODEC =
          new StreamCodec<>() {
            @Override
            public Map<ModelPartType, CustomRotation> decode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf) {
              CompoundTag compoundTag = registryFriendlyByteBuf.readNbt();
              Map<ModelPartType, CustomRotation> modelPartMap = new EnumMap<>(ModelPartType.class);
              for (String key : compoundTag.getAllKeys()) {
                ModelPartType modelPartType = ModelPartType.get(key);
                if (modelPartType != null) {
                  modelPartMap.put(modelPartType, new CustomRotation(modelPartType, compoundTag));
                }
              }
              return modelPartMap;
            }

            @Override
            public void encode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf,
                Map<ModelPartType, CustomRotation> modelPartMap) {
              CompoundTag compoundTag = new CompoundTag();
              for (Map.Entry<ModelPartType, CustomRotation> entry : modelPartMap.entrySet()) {
                compoundTag.put(entry.getKey().getTagName(), entry.getValue().save());
              }
              registryFriendlyByteBuf.writeNbt(compoundTag);
            }
          };

  default EnumMap<ModelPartType, CustomRotation> getModelPartRotation() {
    EnumMap<ModelPartType, CustomRotation> modelPartMap =
        getSynchedEntityData(SynchedDataIndex.MODEL_ROTATION);
    if (modelPartMap == null) {
      modelPartMap = new EnumMap<>(ModelPartType.class);
      setModelPartRotation(modelPartMap);
    }
    return modelPartMap;
  }

  default void setModelPartRotation(EnumMap<ModelPartType, CustomRotation> modelPartMap) {
    if (modelPartMap != null) {
      setSynchedEntityData(SynchedDataIndex.MODEL_ROTATION, modelPartMap, true);
    }
  }

  default void setModelPartRotation(ModelPartType modelPartType, CustomRotation rotation) {
    EnumMap<ModelPartType, CustomRotation> modelPartMap = getModelPartRotation();
    if (modelPartType != null) {
      modelPartMap.put(modelPartType, rotation);
      this.setModelPartRotation(new EnumMap<>(modelPartMap));
    }
  }

  default CustomRotation getModelPartRotation(ModelPartType modelPartType) {
    EnumMap<ModelPartType, CustomRotation> modelPartMap = getModelPartRotation();
    return modelPartMap.getOrDefault(modelPartType, DEFAULT_MODEL_PART_ROTATION);
  }

  default void setModelRotation(float y) {
    CustomRotation rotation = getModelPartRotation(ModelPartType.ROOT);
    setModelRotation(rotation.x(), y, rotation.z());
  }

  default void setModelRotation(float x, float y, float z) {
    Entity entity = this.getEntity();
    if (entity != null) {
      entity.setYRot(y);
      entity.setYBodyRot(y);
      entity.setYHeadRot(y);
      entity.yRotO = y;
    }

    LivingEntity livingEntity = this.getLivingEntity();
    if (livingEntity != null) {
      livingEntity.yBodyRotO = y;
      livingEntity.yHeadRotO = y;
    }

    setModelPartRotation(ModelPartType.ROOT, new CustomRotation(x, y, z).withLocked(y != 0));
  }

  default boolean hasChangedModelRotation() {
    EnumMap<ModelPartType, CustomRotation> modelPartMap = getModelPartRotation();
    for (CustomRotation rotation : modelPartMap.values()) {
      if (rotation.hasChanged()
          && !(rotation == modelPartMap.get(ModelPartType.ROOT) && rotation.hasChangedYaw())) {
        return true;
      }
    }
    return false;
  }

  default void defineSynchedModelRotationData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_ROTATION, new EnumMap<>(ModelPartType.class));
  }

  default void addAdditionalModelRotationData(CompoundTag compoundTag) {
    CompoundTag rotationsTag = new CompoundTag();
    EnumMap<ModelPartType, CustomRotation> modelPartMap = getModelPartRotation();
    for (Map.Entry<ModelPartType, CustomRotation> entry : modelPartMap.entrySet()) {
      rotationsTag.put(entry.getKey().getTagName(), entry.getValue().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_ROTATION_TAG, rotationsTag);
  }

  default void readAdditionalModelRotationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_ROTATION_TAG)) {
      return;
    }
    CompoundTag rotationsTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_ROTATION_TAG);
    EnumMap<ModelPartType, CustomRotation> modelPartMap = new EnumMap<>(ModelPartType.class);
    for (String key : rotationsTag.getAllKeys()) {
      ModelPartType modelPartType = ModelPartType.get(key);
      if (modelPartType != null) {
        modelPartMap.put(modelPartType, new CustomRotation(modelPartType, rotationsTag));
      }
    }
    setModelPartRotation(modelPartMap);
  }
}
