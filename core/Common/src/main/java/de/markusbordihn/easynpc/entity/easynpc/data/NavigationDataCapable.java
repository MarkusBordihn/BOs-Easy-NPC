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
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;

public interface NavigationDataCapable<T extends PathfinderMob> extends EasyNPC<T> {

  String DATA_HOME_TAG = "Home";
  String DATA_NAVIGATION_TAG = "Navigation";
  int TRAVEL_EVENT_TICK = 20;

  default BlockPos getHomePosition() {
    return getSynchedEntityData(SynchedDataIndex.NAVIGATION_HOME_POSITION);
  }

  default void setHomePosition(BlockPos blockPos) {
    setSynchedEntityData(SynchedDataIndex.NAVIGATION_HOME_POSITION, blockPos);
  }

  default boolean hasHomePosition() {
    return this.getHomePosition() != null && !this.getHomePosition().equals(BlockPos.ZERO);
  }

  default void setPosition(Vec3 pos) {
    this.getLivingEntity().setPos(pos);
    this.getLivingEntity().moveTo(pos);
  }

  default void refreshGroundNavigation() {
    GroundPathNavigation groundPathNavigation = this.getGroundPathNavigation();
    if (groundPathNavigation == null) {
      return;
    }

    EntityAttributes attributeData =
        this.getEasyNPCAttributeData() != null
            ? this.getEasyNPCAttributeData().getEntityAttributes()
            : null;
    if (attributeData != null && attributeData.hasMovementAttributes()) {
      groundPathNavigation.setCanOpenDoors(attributeData.getMovementAttributes().canOpenDoor());
      groundPathNavigation.setCanPassDoors(attributeData.getMovementAttributes().canPassDoor());
      groundPathNavigation.setCanFloat(attributeData.getEnvironmentalAttributes().canFloat());
    } else {
      groundPathNavigation.setCanOpenDoors(true);
      groundPathNavigation.setCanPassDoors(true);
      groundPathNavigation.setCanFloat(true);
    }
  }

  default GroundPathNavigation getGroundPathNavigation() {
    if (this instanceof Mob mob
        && mob.getNavigation() instanceof GroundPathNavigation groundPathNavigation) {
      return groundPathNavigation;
    }
    return null;
  }

  default void defineSynchedNavigationData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(builder, SynchedDataIndex.NAVIGATION_HOME_POSITION, BlockPos.ZERO);
  }

  default boolean canFly() {
    return false;
  }

  default boolean isFlying() {
    return canFly() && !this.getEntity().onGround();
  }

  default void addAdditionalNavigationData(CompoundTag compoundTag) {
    CompoundTag navigationTag = new CompoundTag();
    if (this.hasHomePosition()) {
      navigationTag.put(DATA_HOME_TAG, CompoundTagUtils.writeBlockPos(this.getHomePosition()));
    }
    compoundTag.put(DATA_NAVIGATION_TAG, navigationTag);
  }

  default void readAdditionalNavigationData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_NAVIGATION_TAG)) {
      return;
    }

    CompoundTag navigationTag = compoundTag.getCompound(DATA_NAVIGATION_TAG);
    if (navigationTag.contains(DATA_HOME_TAG)) {
      this.setHomePosition(CompoundTagUtils.readBlockPos(navigationTag.getCompound(DATA_HOME_TAG)));
    }
  }

  default void handleNavigationTravelEvent(Vec3 vec3) {
    TickerDataCapable<?> tickerData = this.getEasyNPCTickerData();

    // Update basic movement relevant data.
    if (tickerData.checkAndIncreaseTicker(TickerType.TRAVEL_EVENT, TRAVEL_EVENT_TICK)) {

      // Define if NPC is on ground or not.
      Mob mob = this.getMob();
      Level level = this.getEntityLevel();
      BlockState blockState = level.getBlockState(mob.getOnPos());
      mob.setOnGround(
          !blockState.is(Blocks.AIR)
              && !blockState.is(Blocks.GRASS_BLOCK)
              && !blockState.is(Blocks.WHITE_CARPET)
              && !blockState.is(Blocks.RED_CARPET));

      // Handle gravity and movement logic based on environmental attributes
      ObjectiveDataCapable<?> objectiveData = this.getEasyNPCObjectiveData();
      if (!objectiveData.hasTravelTargetObjectives()) {
        AttributeDataCapable<?> attributeData = this.getEasyNPCAttributeData();
        var environmentalAttributes =
            attributeData.getEntityAttributes().getEnvironmentalAttributes();
        if (environmentalAttributes.freefall()
            && !environmentalAttributes.noGravity()
            && !mob.onGround()) {
          Vec3 currentPos = mob.position();
          BlockPos belowPos = mob.getOnPos().below();
          BlockState belowBlock = level.getBlockState(belowPos);
          if (belowBlock.isAir()) {
            mob.setPos(currentPos.x, Math.max(currentPos.y, belowPos.getY() + 1.0), currentPos.z);
          } else {
            mob.setPos(mob.getX(), Math.floor(mob.getY() - 0.1d), mob.getZ());
          }
        }
      }

      tickerData.resetTicker(TickerType.TRAVEL_EVENT);
    }
  }
}
