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

import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
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
import net.minecraft.world.entity.ai.attributes.Attribute;

public interface AttributeData<E extends PathfinderMob> extends EasyNPC<E> {

  EntityDataSerializer<EntityAttributes> ENTITY_ATTRIBUTES_SERIALIZER =
      new EntityDataSerializer<>() {
        @Override
        public void write(FriendlyByteBuf buffer, EntityAttributes value) {
          buffer.writeNbt(value.createTag());
        }

        @Override
        public EntityAttributes read(FriendlyByteBuf buffer) {
          return new EntityAttributes(buffer.readNbt());
        }

        @Override
        public EntityAttributes copy(EntityAttributes value) {
          return value;
        }
      };

  static void registerAttributeDataSerializer() {
    EntityDataSerializers.registerSerializer(ENTITY_ATTRIBUTES_SERIALIZER);
  }

  static void registerSyncedAttributeData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    log.info("- Registering Synched Attribute Data for {}.", entityClass.getSimpleName());
    map.put(
        SynchedDataIndex.ENTITY_ATTRIBUTES,
        SynchedEntityData.defineId(entityClass, ENTITY_ATTRIBUTES_SERIALIZER));
  }

  default void setBaseAttribute(Attribute attribute, double value) {
    if (attribute == null || getLivingEntity().getAttribute(attribute) == null) {
      return;
    }
    getLivingEntity().getAttribute(attribute).setBaseValue(value);
  }

  default double getBaseAttribute(Attribute attribute) {
    if (attribute == null || getLivingEntity().getAttribute(attribute) == null) {
      return 0.0;
    }
    return getLivingEntity().getAttribute(attribute).getBaseValue();
  }

  default EntityAttributes getEntityAttributes() {
    return getSynchedEntityData(SynchedDataIndex.ENTITY_ATTRIBUTES);
  }

  default void setEntityAttributes(EntityAttributes entityAttributes) {
    setSynchedEntityData(SynchedDataIndex.ENTITY_ATTRIBUTES, entityAttributes);
  }

  default void refreshEntityAttributes() {
    EntityAttributes entityAttributes = getEntityAttributes();
    if (entityAttributes != null) {
      setEntityAttributes(new EntityAttributes());
      setEntityAttributes(entityAttributes);
    }
  }

  default boolean getAttributeSilent() {
    return getEntity().isSilent();
  }

  default void setAttributeSilent(boolean silent) {
    getEntity().setSilent(silent);
  }

  default void defineSynchedAttributeData() {
    defineSynchedEntityData(SynchedDataIndex.ENTITY_ATTRIBUTES, new EntityAttributes());
  }

  default void addAdditionalAttributeData(CompoundTag compoundTag) {
    EntityAttributes entityAttributes = getEntityAttributes();
    if (entityAttributes != null) {
      entityAttributes.save(compoundTag);
    }
  }

  default void readAdditionalAttributeData(CompoundTag compoundTag) {
    this.setEntityAttributes(new EntityAttributes(compoundTag));
  }
}
