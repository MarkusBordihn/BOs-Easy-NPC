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
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.PathfinderMob;

public interface ModelVisibilityDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  String EASY_NPC_DATA_MODEL_VISIBLE_TAG = "Visible";

  StreamCodec<RegistryFriendlyByteBuf, Map<ModelPartType, Boolean>>
      MODEL_PART_VISIBILITY_STREAM_CODEC =
          new StreamCodec<>() {
            @Override
            public Map<ModelPartType, Boolean> decode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf) {
              CompoundTag compoundTag = registryFriendlyByteBuf.readNbt();
              Map<ModelPartType, Boolean> modelPartMap = new EnumMap<>(ModelPartType.class);
              for (String key : compoundTag.getAllKeys()) {
                ModelPartType modelPartType = ModelPartType.get(key);
                if (modelPartType != null) {
                  modelPartMap.put(modelPartType, compoundTag.getBoolean(key));
                }
              }
              return modelPartMap;
            }

            @Override
            public void encode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf,
                Map<ModelPartType, Boolean> modelPartMap) {
              CompoundTag compoundTag = new CompoundTag();
              for (Map.Entry<ModelPartType, Boolean> entry : modelPartMap.entrySet()) {
                compoundTag.putBoolean(entry.getKey().getTagName(), entry.getValue());
              }
              registryFriendlyByteBuf.writeNbt(compoundTag);
            }
          };

  default EnumMap<ModelPartType, Boolean> getModelPartVisibility() {
    EnumMap<ModelPartType, Boolean> modelPartMap =
        getSynchedEntityData(SynchedDataIndex.MODEL_VISIBILITY);
    if (modelPartMap == null) {
      modelPartMap = new EnumMap<>(ModelPartType.class);
      setModelPartVisibility(modelPartMap);
    }
    return modelPartMap;
  }

  default void setModelPartVisibility(EnumMap<ModelPartType, Boolean> modelPartMap) {
    if (modelPartMap != null) {
      setSynchedEntityData(SynchedDataIndex.MODEL_VISIBILITY, modelPartMap, true);
    }
  }

  default void setModelPartVisibility(ModelPartType modelPartType, boolean visible) {
    EnumMap<ModelPartType, Boolean> modelPartMap = getModelPartVisibility();
    if (modelPartType != null) {
      modelPartMap.put(modelPartType, visible);
      this.setModelPartVisibility(new EnumMap<>(modelPartMap));
    }
  }

  default boolean getModelPartVisibility(EquipmentSlot equipmentSlot) {
    return switch (equipmentSlot) {
      case HEAD -> getModelPartVisibility(ModelPartType.HELMET);
      case CHEST -> getModelPartVisibility(ModelPartType.CHESTPLATE);
      case LEGS -> getModelPartVisibility(ModelPartType.LEGGINGS);
      case FEET -> getModelPartVisibility(ModelPartType.BOOTS);
      default -> false;
    };
  }

  default boolean getModelPartVisibility(ModelPartType modelPartType) {
    EnumMap<ModelPartType, Boolean> modelPartMap = getModelPartVisibility();
    return modelPartMap.getOrDefault(modelPartType, true);
  }

  default void setModelPartVisibility(EquipmentSlot equipmentSlot, boolean visible) {
    switch (equipmentSlot) {
      case HEAD:
        setModelPartVisibility(ModelPartType.HELMET, visible);
        break;
      case CHEST:
        setModelPartVisibility(ModelPartType.CHESTPLATE, visible);
        break;
      case LEGS:
        setModelPartVisibility(ModelPartType.LEGGINGS, visible);
        break;
      case FEET:
        setModelPartVisibility(ModelPartType.BOOTS, visible);
        break;
      default:
        break;
    }
  }

  default boolean hasChangedModelVisibility() {
    EnumMap<ModelPartType, Boolean> modelPartMap = getModelPartVisibility();
    for (Map.Entry<ModelPartType, Boolean> entry : modelPartMap.entrySet()) {
      if (entry.getValue() != null) {
        return true;
      }
    }
    return false;
  }

  default void defineSynchedModelVisibilityData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.MODEL_VISIBILITY, new EnumMap<>(ModelPartType.class));
  }

  default void addAdditionalModelVisibilityData(CompoundTag compoundTag) {
    CompoundTag visibilityTag = new CompoundTag();
    EnumMap<ModelPartType, Boolean> modelPartMap = getModelPartVisibility();
    for (Map.Entry<ModelPartType, Boolean> entry : modelPartMap.entrySet()) {
      visibilityTag.putBoolean(entry.getKey().getTagName(), entry.getValue());
    }
    compoundTag.put(EASY_NPC_DATA_MODEL_VISIBLE_TAG, visibilityTag);
  }

  default void readAdditionalModelVisibilityData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_MODEL_VISIBLE_TAG)) {
      return;
    }
    CompoundTag visibilityTag = compoundTag.getCompound(EASY_NPC_DATA_MODEL_VISIBLE_TAG);
    EnumMap<ModelPartType, Boolean> modelPartMap = new EnumMap<>(ModelPartType.class);
    for (String key : visibilityTag.getAllKeys()) {
      ModelPartType modelPartType = ModelPartType.get(key);
      if (modelPartType != null) {
        modelPartMap.put(modelPartType, visibilityTag.getBoolean(key));
      }
    }
    setModelPartVisibility(modelPartMap);
  }
}
