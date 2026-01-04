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

import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Optional;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;

public interface RenderDataCapable<E extends PathfinderMob> extends EasyNPC<E> {

  String DATA_RENDER_DATA_TAG = "RenderData";

  default void defineSynchedRenderData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.RENDER_DATA, new RenderDataEntry());
  }

  default RenderDataEntry getRenderDataEntry() {
    return this.getSynchedEntityData(SynchedDataIndex.RENDER_DATA);
  }

  default void setRenderData(RenderDataEntry renderData) {
    this.setSynchedEntityData(SynchedDataIndex.RENDER_DATA, renderData, true);
  }

  default void addAdditionalRenderData(ValueOutput valueOutput) {
    CompoundTag renderTag = new CompoundTag();

    RenderDataEntry renderData = this.getRenderDataEntry();
    if (renderData != null) {
      renderData.write(renderTag);
    }

    valueOutput.store(DATA_RENDER_DATA_TAG, CompoundTag.CODEC, renderTag);
  }

  default void readAdditionalRenderData(ValueInput valueInput) {

    // Early exit if no render data is available
    Optional<CompoundTag> compoundTagData =
        valueInput.read(DATA_RENDER_DATA_TAG, CompoundTag.CODEC);
    if (compoundTagData.isEmpty()) {
      return;
    }

    // Read render data
    RenderDataEntry renderData = new RenderDataEntry(compoundTagData.get());
    this.setRenderData(renderData);
  }
}
