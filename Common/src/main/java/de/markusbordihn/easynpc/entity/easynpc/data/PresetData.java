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

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public interface PresetData<T extends LivingEntity> extends EasyNPC<T> {

  default CompoundTag exportPreset() {
    return this.serializeNBT();
  }

  default void importPreset(CompoundTag compoundTag) {

    // Skip import if no data is or no entity is available.
    if (compoundTag == null || compoundTag.isEmpty() || this.getEasyNPCEntity() == null) {
      return;
    }

    // Reset specific data to avoid side effects
    if (this.getEasyNPCEntity() != null) {
      this.getEasyNPCEntity().setPose(Pose.STANDING);
    }
    if (this.getEasyNPCModelData() != null) {
      this.getEasyNPCModelData().setModelPose(ModelPose.DEFAULT);
    }
    if (this.getEasyNPCActionEventData() != null) {
      this.getEasyNPCActionEventData().clearActionEventSet();
    }
    if (this.getEasyNPCDialogData() != null) {
      this.getEasyNPCDialogData().clearDialogDataSet();
    }

    // If preset contains id and pos then we can import it directly, otherwise we
    // need to merge it with existing data.
    if (!compoundTag.contains("UUID") && !compoundTag.contains("Pos")) {
      CompoundTag existingCompoundTag = this.serializeNBT();

      // Remove existing dialog data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(DialogData.DATA_DIALOG_DATA_TAG)) {
        existingCompoundTag.remove(DialogData.DATA_DIALOG_DATA_TAG);
      }

      // Remove existing model data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(ModelData.EASY_NPC_DATA_MODEL_DATA_TAG)) {
        existingCompoundTag.remove(ModelData.EASY_NPC_DATA_MODEL_DATA_TAG);
      }

      // Remove existing skin data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(SkinData.EASY_NPC_DATA_SKIN_DATA_TAG)) {
        existingCompoundTag.remove(SkinData.EASY_NPC_DATA_SKIN_DATA_TAG);
      }

      // Remove existing action data to allow legacy presets to be imported.
      if (existingCompoundTag.contains(ActionEventData.DATA_ACTION_DATA_TAG)) {
        existingCompoundTag.remove(ActionEventData.DATA_ACTION_DATA_TAG);
      }

      log.debug(
          "Merging preset {} with existing data {} for {}", compoundTag, existingCompoundTag, this);
      compoundTag = existingCompoundTag.merge(compoundTag);
    } else {
      log.debug("Importing full preset {} for {}", compoundTag, this);
    }

    // Remove motion tag to avoid side effects.
    if (compoundTag.contains("Motion")) {
      compoundTag.remove("Motion");
    }

    // Import preset data to entity.
    this.getEasyNPCEntity().load(compoundTag);
  }

  default String getEntityTypeId() {
    if (this.getEasyNPCEntity() == null) {
      return null;
    }
    EntityType<?> entitytype = this.getEasyNPCEntity().getType();
    ResourceLocation resourcelocation = EntityType.getKey(entitytype);
    return entitytype.canSerialize() ? resourcelocation.toString() : null;
  }

  default CompoundTag serializeNBT() {
    CompoundTag compoundTag = new CompoundTag();
    if (this.getEasyNPCEntity() == null) {
      return compoundTag;
    }
    String entityTypeId = this.getEntityTypeId();
    if (entityTypeId != null) {
      compoundTag.putString("id", entityTypeId);
    }
    return this.getEasyNPCEntity().saveWithoutId(compoundTag);
  }
}
