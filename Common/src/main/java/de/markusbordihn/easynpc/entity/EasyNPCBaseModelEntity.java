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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBaseModel;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.level.Level;

public class EasyNPCBaseModelEntity<E extends PathfinderMob> extends EasyNPCBaseEntity<E>
    implements EasyNPCBaseModel<E> {

  static {
    EasyNPCBaseModel.registerEasyNPCSyncedData(entityDataAccessorMap, EasyNPCBaseModelEntity.class);
  }

  public EasyNPCBaseModelEntity(
      EntityType<? extends PathfinderMob> entityType, Level level, Enum<?> variant) {
    super(entityType, level, variant);
  }

  @Override
  public EntityDimensions getDimensions(Pose pose) {
    float scaleXZ = getScaleX() > getScaleZ() ? getScaleX() : getScaleZ();
    return super.getDimensions(pose).scale(scaleXZ, getScaleY());
  }

  @Override
  protected void defineSynchedData() {
    super.defineSynchedData();
    this.defineEasyNPCBaseModelSyncedData();
  }

  @Override
  public void addAdditionalSaveData(CompoundTag compoundTag) {
    super.addAdditionalSaveData(compoundTag);
    this.addEasyNPCBaseModelAdditionalSaveData(compoundTag);
  }

  @Override
  public void readAdditionalSaveData(CompoundTag compoundTag) {
    super.readAdditionalSaveData(compoundTag);
    this.readEasyNPCBaseModelAdditionalSaveData(compoundTag);
  }
}
