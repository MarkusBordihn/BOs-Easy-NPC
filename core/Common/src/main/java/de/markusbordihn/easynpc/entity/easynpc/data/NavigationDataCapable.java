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
import de.markusbordihn.easynpc.data.attribute.MovementAttributes;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.ticker.TickerType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.utils.CompoundTagUtils;
import java.util.Optional;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.tags.FluidTags;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.SpawnPlacementTypes;
import net.minecraft.world.entity.SpawnPlacements;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
import net.minecraft.world.phys.Vec3;

public interface NavigationDataCapable<T extends Mob> extends EasyNPC<T> {

  String DATA_HOME_TAG = "Home";
  String DATA_NAVIGATION_TAG = "Navigation";
  int TRAVEL_EVENT_TICK = 20;

  default BlockPos getNPCHomePosition() {
    return getSynchedEntityData(SynchedDataIndex.NAVIGATION_HOME_POSITION);
  }

  default void setNPCHomePosition(BlockPos blockPos) {
    setSynchedEntityData(SynchedDataIndex.NAVIGATION_HOME_POSITION, blockPos);
  }

  default boolean hasNPCHomePosition() {
    return this.getNPCHomePosition() != null && !this.getNPCHomePosition().equals(BlockPos.ZERO);
  }

  default void setNPCHomePositionIfMissing(BlockPos blockPos) {
    if (!this.hasNPCHomePosition()) {
      this.setNPCHomePosition(blockPos);
    }
  }

  default void applyDefaultNPCHomePosition() {
    this.setNPCHomePositionIfMissing(this.getEntity().blockPosition());
  }

  default void setPosition(Vec3 pos) {
    this.getLivingEntity().setPos(pos);
    this.getLivingEntity().snapTo(pos);
  }

  default void refreshGroundNavigation() {
    GroundPathNavigation groundPathNavigation = this.getGroundPathNavigation();
    if (groundPathNavigation == null) {
      return;
    }

    EntityAttributes attributeData = this.getNavigationEntityAttributes();
    if (attributeData != null && attributeData.hasMovementAttributes()) {
      MovementAttributes movementAttributes = attributeData.getMovementAttributes();
      boolean canOpenDoor = movementAttributes.canOpenDoor();
      boolean canCloseDoor = movementAttributes.canCloseDoor();
      // Vanilla pathfinding only routes through a door (open or closed) when canPassDoors is set,
      // so any door interaction implies door traversal.
      groundPathNavigation.setCanOpenDoors(canOpenDoor);
      groundPathNavigation
          .getNodeEvaluator()
          .setCanPassDoors(movementAttributes.canPassDoor() || canOpenDoor || canCloseDoor);
      groundPathNavigation.setCanFloat(attributeData.getEnvironmentalAttributes().canFloat());
    } else {
      groundPathNavigation.setCanOpenDoors(true);
      groundPathNavigation.setCanFloat(true);
    }
  }

  default void handleWaterEscapeTick() {
    GroundPathNavigation groundPathNavigation = this.getGroundPathNavigation();
    if (groundPathNavigation == null) {
      return;
    }

    Mob mob = this.getMob();
    if (!mob.isInWater() || mob.canBreatheUnderwater() || this.isImmovable()) {
      if (groundPathNavigation.canFloat() != this.configuredCanFloat()) {
        this.refreshGroundNavigation();
      }

      return;
    }

    if (!groundPathNavigation.canFloat()) {
      groundPathNavigation.setCanFloat(true);
      groundPathNavigation.stop();
    }

    if (mob.getFluidHeight(FluidTags.WATER) > mob.getFluidJumpThreshold()) {
      mob.getJumpControl().jump();
    }
  }

  private boolean configuredCanFloat() {
    EntityAttributes attributeData = this.getNavigationEntityAttributes();
    return attributeData == null
        || !attributeData.hasMovementAttributes()
        || attributeData.getEnvironmentalAttributes().canFloat();
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

  default NavigationType defaultNavigationType() {
    return this.rendersAsWaterCreature() ? NavigationType.AQUATIC : NavigationType.GROUND;
  }

  default boolean rendersAsWaterCreature() {
    RenderDataCapable<?> renderData = this.getEasyNPCRenderData();
    RenderDataEntry renderDataEntry = renderData != null ? renderData.getRenderDataEntry() : null;
    if (renderDataEntry == null || renderDataEntry.getRenderType() != RenderType.CUSTOM_ENTITY) {
      return false;
    }

    EntityType<? extends Entity> renderEntityType = renderDataEntry.getRenderEntityType();
    if (renderEntityType == null) {
      return false;
    }

    return hasWaterMobCategory(renderEntityType) || spawnsInWater(renderEntityType);
  }

  private static boolean hasWaterMobCategory(EntityType<? extends Entity> entityType) {
    MobCategory mobCategory = entityType.getCategory();
    return mobCategory == MobCategory.WATER_CREATURE
        || mobCategory == MobCategory.WATER_AMBIENT
        || mobCategory == MobCategory.UNDERGROUND_WATER_CREATURE
        || mobCategory == MobCategory.AXOLOTLS;
  }

  private static boolean spawnsInWater(EntityType<? extends Entity> entityType) {
    return SpawnPlacements.getPlacementType(entityType) == SpawnPlacementTypes.IN_WATER;
  }

  default NavigationType getNavigationType() {
    MovementAttributes movementAttributes = this.getMovementAttributes();
    if (movementAttributes == null
        || movementAttributes.navigationType() == NavigationType.DEFAULT) {
      return this.defaultNavigationType();
    }

    return movementAttributes.navigationType();
  }

  default double defaultHoverHeight() {
    return 0.0D;
  }

  default double getHoverHeight() {
    MovementAttributes movementAttributes = this.getMovementAttributes();
    if (movementAttributes == null || movementAttributes.hoverHeight() <= 0.0D) {
      return this.defaultHoverHeight();
    }

    return movementAttributes.hoverHeight();
  }

  default double defaultSwimDepthBelowSurface() {
    return 0.0D;
  }

  default double getSwimDepthBelowSurface() {
    MovementAttributes movementAttributes = this.getMovementAttributes();
    if (movementAttributes == null || movementAttributes.swimDepthBelowSurface() <= 0.0D) {
      return this.defaultSwimDepthBelowSurface();
    }

    return movementAttributes.swimDepthBelowSurface();
  }

  default double defaultSwimHeightAboveFloor() {
    return 0.0D;
  }

  default double getSwimHeightAboveFloor() {
    MovementAttributes movementAttributes = this.getMovementAttributes();
    if (movementAttributes == null || movementAttributes.swimHeightAboveFloor() <= 0.0D) {
      return this.defaultSwimHeightAboveFloor();
    }

    return movementAttributes.swimHeightAboveFloor();
  }

  private EntityAttributes getNavigationEntityAttributes() {
    return this.getEasyNPCAttributeData() != null
        ? this.getEasyNPCAttributeData().getEntityAttributes()
        : null;
  }

  default MovementAttributes getMovementAttributes() {
    EntityAttributes attributeData = this.getNavigationEntityAttributes();
    if (attributeData == null || !attributeData.hasMovementAttributes()) {
      return null;
    }

    return attributeData.getMovementAttributes();
  }

  default NavigationType getAppliedNavigationType() {
    return NavigationType.DEFAULT;
  }

  default void refreshNavigation() {
    this.refreshGroundNavigation();
  }

  default void restoreGravityFromAttributes() {
    EntityAttributes attributeData = this.getNavigationEntityAttributes();
    if (attributeData != null && this.getLivingEntity() != null) {
      this.getLivingEntity().setNoGravity(attributeData.getEnvironmentalAttributes().noGravity());
    }
  }

  default void refreshNavigationIfChanged() {
    if (this.getAppliedNavigationType() != this.getNavigationType()) {
      this.refreshNavigation();
    }
  }

  default boolean canFly() {
    return this.getNavigationType() == NavigationType.FLYING;
  }

  default boolean canSwim() {
    return this.getNavigationType() == NavigationType.AQUATIC;
  }

  default boolean isFlying() {
    return canFly() && !this.getEntity().onGround();
  }

  default boolean canJump() {
    return false;
  }

  default void addAdditionalNavigationData(ValueOutput valueOutput) {
    CompoundTag navigationTag = new CompoundTag();
    if (this.hasNPCHomePosition()) {
      navigationTag.put(DATA_HOME_TAG, CompoundTagUtils.writeBlockPos(this.getNPCHomePosition()));
    }
    if (!navigationTag.isEmpty()) {
      valueOutput.store(DATA_NAVIGATION_TAG, CompoundTag.CODEC, navigationTag);
    }
  }

  default void readAdditionalNavigationData(ValueInput valueInput) {
    // Early exit if no navigation data is available.
    Optional<CompoundTag> compoundTagData = valueInput.read(DATA_NAVIGATION_TAG, CompoundTag.CODEC);
    if (compoundTagData.isEmpty()) {
      return;
    }

    // Read navigation data.
    CompoundTag navigationTag = compoundTagData.get();
    if (navigationTag.contains(DATA_HOME_TAG)) {
      this.setNPCHomePosition(
          CompoundTagUtils.readBlockPos(navigationTag.getCompoundOrEmpty(DATA_HOME_TAG)));
    }
  }

  default boolean isImmovable() {
    AttributeDataCapable<?> attributeData = this.getEasyNPCAttributeData();
    return attributeData != null
        && attributeData.getEntityAttributes() != null
        && attributeData.getEntityAttributes().getMovementAttributes().isImmovable();
  }

  default boolean anchorImmovablePosition() {
    if (!this.isImmovable()) {
      return false;
    }

    Mob mob = this.getMob();
    mob.setDeltaMovement(Vec3.ZERO);
    if (!mob.getNavigation().isDone()) {
      mob.getNavigation().stop();
    }
    return true;
  }

  default void handleNavigationTravelEvent(Vec3 vec3) {
    TickerDataCapable<?> tickerData = this.getEasyNPCTickerData();

    // Update basic movement relevant data.
    if (tickerData.checkAndIncreaseTicker(TickerType.TRAVEL_EVENT, TRAVEL_EVENT_TICK)) {

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
