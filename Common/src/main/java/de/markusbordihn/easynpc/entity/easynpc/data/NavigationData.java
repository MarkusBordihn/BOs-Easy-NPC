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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.EntityDataSerializers;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.phys.Vec3;

public interface NavigationData<T extends LivingEntity> extends EasyNPC<T> {

  EntityDataAccessor<BlockPos> DATA_HOME_POSITION =
      SynchedEntityData.defineId(
          EasyNPC.getSynchedEntityDataClass(), EntityDataSerializers.BLOCK_POS);

  String DATA_NAVIGATION_TAG = "Navigation";
  String DATA_HOME_TAG = "Home";

  default BlockPos getHomePosition() {
    return getEasyNPCData(DATA_HOME_POSITION);
  }

  default void setHomePosition(BlockPos blockPos) {
    setEasyNPCData(DATA_HOME_POSITION, blockPos);
  }

  default boolean hasHomePosition() {
    return this.getHomePosition() != null && !this.getHomePosition().equals(BlockPos.ZERO);
  }

  default void setPosition(Vec3 pos) {
    this.getEasyNPCEntity().setPos(pos);
    this.getEasyNPCEntity().moveTo(pos);
  }

  GroundPathNavigation getGroundPathNavigation();

  default void defineSynchedNavigationData() {
    this.getEasyNPCEntity().getEntityData().define(DATA_HOME_POSITION, BlockPos.ZERO);
  }

  default void addAdditionalNavigationData(CompoundTag compoundTag) {
    CompoundTag navigationTag = new CompoundTag();
    if (this.hasHomePosition()) {
      navigationTag.put(DATA_HOME_TAG, NbtUtils.writeBlockPos(this.getHomePosition()));
    }
    compoundTag.put(DATA_NAVIGATION_TAG, navigationTag);
  }

  default void readAdditionalNavigationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_NAVIGATION_TAG)) {
      return;
    }

    CompoundTag navigationTag = compoundTag.getCompound(DATA_NAVIGATION_TAG);
    if (navigationTag.contains(DATA_HOME_TAG)) {
      this.setHomePosition(NbtUtils.readBlockPos(navigationTag.getCompound(DATA_HOME_TAG)));
    }
  }
}
