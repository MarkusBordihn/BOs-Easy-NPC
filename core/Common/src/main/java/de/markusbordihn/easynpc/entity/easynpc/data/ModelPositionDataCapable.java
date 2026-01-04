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
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelPositionDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  CustomPosition DEFAULT_MODEL_PART_POSITION = new CustomPosition(0, 0, 0);
  String EASY_NPC_DATA_MODEL_POSITION_TAG = "Position";

  StreamCodec<RegistryFriendlyByteBuf, Map<ModelPartType, CustomPosition>>
      MODEL_PART_POSITION_STREAM_CODEC =
          new StreamCodec<>() {
            @Override
            public Map<ModelPartType, CustomPosition> decode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf) {
              CompoundTag compoundTag = registryFriendlyByteBuf.readNbt();
              Map<ModelPartType, CustomPosition> modelPartMap = new EnumMap<>(ModelPartType.class);
              for (String key : compoundTag.getAllKeys()) {
                ModelPartType modelPartType = ModelPartType.get(key);
                if (modelPartType != null) {
                  modelPartMap.put(modelPartType, new CustomPosition(modelPartType, compoundTag));
                }
              }
              return modelPartMap;
            }

            @Override
            public void encode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf,
                Map<ModelPartType, CustomPosition> modelPartMap) {
              CompoundTag compoundTag = new CompoundTag();
              for (Map.Entry<ModelPartType, CustomPosition> entry : modelPartMap.entrySet()) {
                compoundTag.put(entry.getKey().getTagName(), entry.getValue().save());
              }
              registryFriendlyByteBuf.writeNbt(compoundTag);
            }
          };

  default EnumMap<ModelPartType, CustomPosition> getModelPartPosition() {
    EnumMap<ModelPartType, CustomPosition> modelPartMap =
        getSynchedEntityData(SynchedDataIndex.MODEL_POSITION);
    if (modelPartMap == null) {
      modelPartMap = new EnumMap<>(ModelPartType.class);
      setModelPartPosition(modelPartMap);
    }
    return modelPartMap;
  }

  default void setModelPartPosition(EnumMap<ModelPartType, CustomPosition> modelPartMap) {
    if (modelPartMap != null) {
      setSynchedEntityData(SynchedDataIndex.MODEL_POSITION, modelPartMap, true);
    }
  }

  default void setModelPartPosition(ModelPartType modelPartType, CustomPosition Position) {
    EnumMap<ModelPartType, CustomPosition> modelPartMap = getModelPartPosition();
    if (modelPartType != null) {
      modelPartMap.put(modelPartType, Position);
      this.setModelPartPosition(new EnumMap<>(modelPartMap));
    }
  }

  default CustomPosition getModelPartPosition(ModelPartType modelPartType) {
    EnumMap<ModelPartType, CustomPosition> modelPartMap = getModelPartPosition();
    return modelPartMap.getOrDefault(modelPartType, DEFAULT_MODEL_PART_POSITION);
  }

  default boolean hasChangedModelPosition() {
    EnumMap<ModelPartType, CustomPosition> modelPartMap = getModelPartPosition();
    for (Map.Entry<ModelPartType, CustomPosition> entry : modelPartMap.entrySet()) {
      if (entry.getValue().hasChanged()) {
        return true;
      }
    }
    return false;
  }

  default void defineSynchedModelPositionData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_POSITION, new EnumMap<>(ModelPartType.class));
  }

  default void addAdditionalModelPositionData(CompoundTag compoundTag) {
    CompoundTag positionsTag = new CompoundTag();
    EnumMap<ModelPartType, CustomPosition> modelPartMap = getModelPartPosition();
    for (Map.Entry<ModelPartType, CustomPosition> entry : modelPartMap.entrySet()) {
      positionsTag.put(entry.getKey().getTagName(), entry.getValue().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_POSITION_TAG, positionsTag);
  }

  default void readAdditionalModelPositionData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_POSITION_TAG)) {
      return;
    }
    CompoundTag positionTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_POSITION_TAG);
    EnumMap<ModelPartType, CustomPosition> modelPartMap = new EnumMap<>(ModelPartType.class);
    for (String key : positionTag.getAllKeys()) {
      ModelPartType modelPartType = ModelPartType.get(key);
      if (modelPartType != null) {
        modelPartMap.put(modelPartType, new CustomPosition(modelPartType, positionTag));
      }
    }
    setModelPartPosition(modelPartMap);
  }
}
