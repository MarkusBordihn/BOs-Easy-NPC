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
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelScaleDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  CustomScale DEFAULT_MODEL_PART_SCALE = new CustomScale(1, 1, 1);
  CustomScale DEFAULT_MODEL_SCALE = new CustomScale(1, 1, 1);
  String EASY_NPC_DATA_MODEL_SCALE_TAG = "Scale";

  StreamCodec<RegistryFriendlyByteBuf, Map<ModelPartType, CustomScale>>
      MODEL_PART_SCALE_STREAM_CODEC =
          new StreamCodec<>() {
            @Override
            public Map<ModelPartType, CustomScale> decode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf) {
              CompoundTag compoundTag = registryFriendlyByteBuf.readNbt();
              Map<ModelPartType, CustomScale> modelPartMap = new EnumMap<>(ModelPartType.class);
              for (String key : compoundTag.getAllKeys()) {
                ModelPartType modelPartType = ModelPartType.get(key);
                if (modelPartType != null) {
                  modelPartMap.put(modelPartType, new CustomScale(modelPartType, compoundTag));
                }
              }
              return modelPartMap;
            }

            @Override
            public void encode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf,
                Map<ModelPartType, CustomScale> modelPartMap) {
              CompoundTag compoundTag = new CompoundTag();
              for (Map.Entry<ModelPartType, CustomScale> entry : modelPartMap.entrySet()) {
                compoundTag.put(entry.getKey().getTagName(), entry.getValue().save());
              }
              registryFriendlyByteBuf.writeNbt(compoundTag);
            }
          };

  default EnumMap<ModelPartType, CustomScale> getModelPartScale() {
    EnumMap<ModelPartType, CustomScale> modelPartMap =
        getSynchedEntityData(SynchedDataIndex.MODEL_SCALE);
    if (modelPartMap == null) {
      modelPartMap = new EnumMap<>(ModelPartType.class);
      setModelPartScale(modelPartMap);
    }
    return modelPartMap;
  }

  default void setModelPartScale(EnumMap<ModelPartType, CustomScale> modelPartMap) {
    if (modelPartMap != null) {
      setSynchedEntityData(SynchedDataIndex.MODEL_SCALE, modelPartMap, true);
    }
  }

  default void setModelPartScale(ModelPartType modelPartType, CustomScale Scale) {
    EnumMap<ModelPartType, CustomScale> modelPartMap = getModelPartScale();
    if (modelPartType != null) {
      modelPartMap.put(modelPartType, Scale);
      this.setModelPartScale(new EnumMap<>(modelPartMap));

      // Refresh entity dimensions when ROOT scale changes (for hitbox scaling)
      if (modelPartType == ModelPartType.ROOT) {
        this.getEntity().refreshDimensions();
      }
    }
  }

  default CustomScale getModelPartScale(ModelPartType modelPartType) {
    EnumMap<ModelPartType, CustomScale> modelPartMap = getModelPartScale();
    return modelPartMap.getOrDefault(modelPartType, DEFAULT_MODEL_PART_SCALE);
  }

  default boolean hasChangedModelScale() {
    EnumMap<ModelPartType, CustomScale> modelPartMap = getModelPartScale();
    for (Map.Entry<ModelPartType, CustomScale> entry : modelPartMap.entrySet()) {
      if (entry.getValue().hasChanged()) {
        return true;
      }
    }
    return false;
  }

  default CustomScale getDefaultModelScale() {
    return DEFAULT_MODEL_SCALE;
  }

  default void defineSynchedModelScaleData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_SCALE, new EnumMap<>(ModelPartType.class));
  }

  default void addAdditionalModelScaleData(CompoundTag compoundTag) {
    CompoundTag positionsTag = new CompoundTag();
    EnumMap<ModelPartType, CustomScale> modelPartMap = getModelPartScale();
    for (Map.Entry<ModelPartType, CustomScale> entry : modelPartMap.entrySet()) {
      positionsTag.put(entry.getKey().getTagName(), entry.getValue().save());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_SCALE_TAG, positionsTag);
  }

  default void readAdditionalModelScaleData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_SCALE_TAG)) {
      return;
    }
    CompoundTag positionTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_SCALE_TAG);
    EnumMap<ModelPartType, CustomScale> modelPartMap = new EnumMap<>(ModelPartType.class);
    for (String key : positionTag.getAllKeys()) {
      ModelPartType modelPartType = ModelPartType.get(key);
      if (modelPartType != null) {
        modelPartMap.put(modelPartType, new CustomScale(modelPartType, positionTag));
      }
    }
    setModelPartScale(modelPartMap);
  }
}
