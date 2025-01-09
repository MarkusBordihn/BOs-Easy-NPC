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

import de.markusbordihn.easynpc.data.attribute.CustomAttributes;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.EnumMap;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;

public interface CustomAttributeData<E extends PathfinderMob> extends EasyNPC<E> {

  static void registerSyncedCustomAttributeData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Custom Attribute Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.CUSTOM_ATTRIBUTES,
        SynchedEntityData.defineId(entityClass, EntityDataSerializersManager.CUSTOM_ATTRIBUTES));
  }

  default void defineSynchedCustomAttributeData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.CUSTOM_ATTRIBUTES, new CustomAttributes());
  }

  default CustomAttributes getCustomAttributes() {
    return this.getSynchedEntityData(SynchedDataIndex.CUSTOM_ATTRIBUTES);
  }

  default void setCustomAttributes(CustomAttributes customAttributes) {
    this.setSynchedEntityData(SynchedDataIndex.CUSTOM_ATTRIBUTES, customAttributes);
  }

  default void addAdditionalCustomAttributeData(CompoundTag compoundTag) {
    CustomAttributes customAttributes = this.getCustomAttributes();
    customAttributes.save(compoundTag);
  }

  default void readAdditionalCustomAttributeData(CompoundTag compoundTag) {
    this.setCustomAttributes(new CustomAttributes(compoundTag));
  }
}
