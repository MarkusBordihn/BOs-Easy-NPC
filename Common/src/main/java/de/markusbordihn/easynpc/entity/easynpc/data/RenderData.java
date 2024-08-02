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

import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface RenderData<E extends PathfinderMob> extends EasyNPC<E> {

  String DATA_RENDER_DATA_TAG = "RenderData";

  EntityDataSerializer<RenderDataSet> RENDER_DATA_SET =
      new EntityDataSerializer<>() {
        public void write(FriendlyByteBuf buffer, RenderDataSet value) {
          buffer.writeNbt(value.createTag());
        }

        public RenderDataSet read(FriendlyByteBuf buffer) {
          return new RenderDataSet(buffer.readNbt());
        }

        public RenderDataSet copy(RenderDataSet value) {
          return value;
        }
      };

  static void registerRenderDataSerializer() {
    EntityDataSerializers.registerSerializer(RENDER_DATA_SET);
  }

  static void registerSyncedRenderData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Render Data for {}.", entityClass.getSimpleName());
    map.put(SynchedDataIndex.RENDER_DATA, SynchedEntityData.defineId(entityClass, RENDER_DATA_SET));
  }

  default void defineSynchedRenderData() {
    defineSynchedEntityData(SynchedDataIndex.RENDER_DATA, new RenderDataSet());
  }

  default RenderDataSet getRenderDataSet() {
    return this.getSynchedEntityData(SynchedDataIndex.RENDER_DATA);
  }

  default void setRenderData(RenderDataSet renderData) {
    this.setSynchedEntityData(SynchedDataIndex.RENDER_DATA, renderData);
  }

  default void updateRenderData() {
    RenderDataSet renderDataSet = this.getRenderDataSet();
    this.setRenderData(new RenderDataSet());
    this.setRenderData(renderDataSet);
  }

  default void addAdditionalRenderData(CompoundTag compoundTag) {
    CompoundTag renderTag = new CompoundTag();

    RenderDataSet renderData = this.getRenderDataSet();
    if (renderData != null) {
      renderData.save(renderTag);
    }

    compoundTag.put(DATA_RENDER_DATA_TAG, renderTag);
  }

  default void readAdditionalRenderData(CompoundTag compoundTag) {

    // Early exit if no dialog data is available.
    if (!compoundTag.contains(DATA_RENDER_DATA_TAG)) {
      return;
    }

    // Read dialog data
    RenderDataSet renderData = new RenderDataSet(compoundTag.getCompound(DATA_RENDER_DATA_TAG));
    this.setRenderData(renderData);
  }
}
