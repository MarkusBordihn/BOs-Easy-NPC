/*
 * Copyright 2025 Markus Bordihn
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

import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.model.ModelAnimationRequest;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Mob;

public interface ModelAnimationDataCapable<T extends Mob> extends EasyNPC<T> {

  String EASY_NPC_DATA_ANIMATION_DATA_TAG = "AnimationData";

  default ModelAnimationData getModelAnimationData() {
    ModelAnimationData animationData = this.getSynchedEntityData(SynchedDataIndex.MODEL_ANIMATION);
    if (animationData == null) {
      animationData = new ModelAnimationData();
      this.setModelAnimationData(animationData);
    }
    return animationData;
  }

  default void setModelAnimationData(ModelAnimationData animationData) {
    if (animationData != null) {
      this.setSynchedEntityData(SynchedDataIndex.MODEL_ANIMATION, animationData);
    }
  }

  default ModelAnimationBehavior getModelAnimationBehavior() {
    return this.getModelAnimationData().behavior();
  }

  default void setModelAnimationBehavior(ModelAnimationBehavior behavior) {
    this.setModelAnimationData(
        new ModelAnimationData(behavior, this.getModelAnimationData().playbackRequest()));
  }

  default ModelAnimationRequest getModelAnimationRequest() {
    return this.getModelAnimationData().playbackRequest();
  }

  default void setModelAnimationRequest(ModelAnimationRequest request) {
    this.setModelAnimationData(new ModelAnimationData(this.getModelAnimationBehavior(), request));
  }

  default void defineSynchedModelAnimationData() {
    this.defineSynchedEntityData(SynchedDataIndex.MODEL_ANIMATION, new ModelAnimationData());
  }

  default void addAdditionalModelAnimationData(CompoundTag compoundTag) {
    ModelAnimationData animationData = this.getModelAnimationData();
    if (animationData != null && animationData.hasChanged()) {
      compoundTag.put(EASY_NPC_DATA_ANIMATION_DATA_TAG, animationData.save());
    }
  }

  default void readAdditionalModelAnimationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(EASY_NPC_DATA_ANIMATION_DATA_TAG)) {
      return;
    }

    CompoundTag animationDataTag = compoundTag.getCompound(EASY_NPC_DATA_ANIMATION_DATA_TAG);
    ModelAnimationData animationData = new ModelAnimationData(animationDataTag);
    this.setModelAnimationData(animationData);
  }
}
