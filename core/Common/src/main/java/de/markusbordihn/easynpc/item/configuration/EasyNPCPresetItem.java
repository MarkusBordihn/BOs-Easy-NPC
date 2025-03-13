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

package de.markusbordihn.easynpc.item.configuration;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.BlockPos.MutableBlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.SpawnerBlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCPresetItem extends Item {

  public static final String ENTITY_TYPE_TAG = "EntityType";
  public static final String NAME = "easy_npc_preset";
  public static final String PRESET_TAG = "Preset";
  public static final String SPAWNER_UUID_TAG = "SpawnerUUID";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String FALL_DISTANCE_TAG = "FallDistance";
  private static final String FIRE_TAG = "Fire";
  private static final String MOTION_TAG = "Motion";
  private static final String ON_GROUND_TAG = "OnGround";
  private static final String CUSTOM_NAME_TAG = "CustomName";
  private static final String TEXT_TAG = "text";

  public EasyNPCPresetItem(Properties properties) {
    super(properties);
  }

  public static UUID getPresetUUID(ItemStack itemStack) {
    CompoundTag compoundTag = getPreset(itemStack);
    if (compoundTag.contains(PresetData.PRESET_UUID_TAG)) {
      return compoundTag.getUUID(PresetData.PRESET_UUID_TAG);
    }
    return null;
  }

  public static boolean hasPreset(ItemStack itemStack) {
    CompoundTag compoundTag = itemStack.getOrCreateTag();
    return compoundTag.contains(PRESET_TAG) && !compoundTag.getCompound(PRESET_TAG).isEmpty();
  }

  public static CompoundTag getPreset(ItemStack itemStack) {
    CompoundTag compoundTag = itemStack.getOrCreateTag();
    return compoundTag.getCompound(PRESET_TAG);
  }

  public static void savePreset(
      ItemStack itemStack, ResourceLocation entityType, CompoundTag presetData) {
    CompoundTag compoundTag = itemStack.getOrCreateTag();
    compoundTag.putString(ENTITY_TYPE_TAG, entityType.toString());

    // Clean up preset data
    if (presetData.contains(FIRE_TAG)) {
      presetData.remove(FIRE_TAG);
    }
    if (presetData.contains(FALL_DISTANCE_TAG)) {
      presetData.remove(FALL_DISTANCE_TAG);
    }
    if (presetData.contains(ON_GROUND_TAG)) {
      presetData.remove(ON_GROUND_TAG);
    }
    compoundTag.put(PRESET_TAG, presetData);
  }

  public static String getCustomName(ItemStack itemStack) {
    CompoundTag compoundTag = getPreset(itemStack);
    if (compoundTag.contains(CUSTOM_NAME_TAG)) {
      CompoundTag customNameTag = compoundTag.getCompound(CUSTOM_NAME_TAG);
      if (customNameTag.contains(TEXT_TAG)) {
        return customNameTag.getString(TEXT_TAG);
      }
    }
    return null;
  }

  public static boolean hasEntityType(ItemStack itemStack) {
    CompoundTag compoundTag = itemStack.getOrCreateTag();
    return compoundTag.contains(ENTITY_TYPE_TAG)
        && !compoundTag.getString(ENTITY_TYPE_TAG).isEmpty();
  }

  public static EntityType<?> getEntityType(ItemStack itemStack) {
    CompoundTag compoundTag = itemStack.getOrCreateTag();
    if (compoundTag.contains(ENTITY_TYPE_TAG)) {
      return EntityType.byString(compoundTag.getString(ENTITY_TYPE_TAG)).orElse(null);
    }
    return null;
  }

  public static boolean spawnAtPosition(BlockPos blockPos, ItemStack itemStack, Level level) {
    // Verify preset and entity type.
    if (level.isClientSide || !hasPreset(itemStack) || !hasEntityType(itemStack)) {
      return false;
    }

    // Get entity type and create entity.
    CompoundTag entityPreset = getPreset(itemStack);
    EntityType<?> entityType = getEntityType(itemStack);
    if (entityType == null) {
      log.error("No valid entity type found in {}!", itemStack);
      return false;
    }

    // Create and validate entity.
    Entity entity = entityType.create(level);
    if (entity == null) {
      log.error("Unable to create entity for {} in {}", entityType, level);
      return false;
    }

    // Remove UUID from preset, to avoid conflicts with existing entities.
    if (entityPreset.contains(Entity.UUID_TAG)) {
      entityPreset.remove(Entity.UUID_TAG);
    }
    entity.load(entityPreset);

    // Move entity to and spawn entity.
    entity.moveTo(blockPos.getX() + 0.5f, blockPos.getY(), blockPos.getZ() + 0.5f);
    if (level.addFreshEntity(entity)) {
      UUID presetUUID = getPresetUUID(itemStack);
      log.debug(
          "Spawned {} at {} with preset UUID {} and {} in {}",
          entityType,
          blockPos,
          presetUUID,
          entityPreset,
          level);
      return true;
    }
    return false;
  }

  @Override
  public InteractionResult useOn(UseOnContext useOnContext) {
    Level level = useOnContext.getLevel();

    // Ignore client side
    if (level.isClientSide) {
      return InteractionResult.SUCCESS;
    }

    // Verify item stack, preset and entity type.
    ItemStack itemStack = useOnContext.getItemInHand();
    if (itemStack.isEmpty() || !hasPreset(itemStack) || !hasEntityType(itemStack)) {
      log.warn("No valid preset found in {}!", itemStack);
      return InteractionResult.FAIL;
    }

    // Check for Spawner Block
    BlockPos blockPos = useOnContext.getClickedPos();
    BlockEntity blockEntity = level.getBlockEntity(blockPos);
    if (blockEntity instanceof SpawnerBlockEntity spawnerBlockEntity) {
      BaseSpawner baseSpawner = spawnerBlockEntity.getSpawner();
      SpawnData spawnData = new SpawnData(getPreset(itemStack), Optional.empty());
      log.debug("Set spawn data {} for spawner {} at {}", spawnData, spawnerBlockEntity, blockPos);
      baseSpawner.setNextSpawnData(level, blockPos, spawnData);
      itemStack.shrink(1);
      return InteractionResult.CONSUME;
    }

    // Check for NPC Spawner Block
    if (blockEntity instanceof EasyNPCSpawnerBlockEntity easyNPCSpawnerBlockEntity) {
      BaseSpawner baseSpawner = easyNPCSpawnerBlockEntity.getSpawner();
      SpawnData spawnData = new SpawnData(getPreset(itemStack), Optional.empty());
      log.debug(
          "Set spawn data {} for base NPC spawner {} at {}",
          spawnData,
          easyNPCSpawnerBlockEntity,
          blockPos);
      baseSpawner.setNextSpawnData(level, blockPos, spawnData);
      itemStack.shrink(1);
      return InteractionResult.CONSUME;
    }

    // Find next free position in x and z direction and spawn entity
    Iterable<MutableBlockPos> possibleSpawnPositions =
        BlockPos.spiralAround(useOnContext.getClickedPos(), 4, Direction.NORTH, Direction.EAST);
    for (MutableBlockPos possibleSpawnPosition : possibleSpawnPositions) {
      AABB aabb = new AABB(possibleSpawnPosition).inflate(0.1);
      BlockPos targetBlockPos =
          new BlockPos(
              possibleSpawnPosition.getX() + 0.5f,
              possibleSpawnPosition.getY() + 1f,
              possibleSpawnPosition.getZ() + 0.5f);
      if (level.getBlockState(targetBlockPos.above()).isAir()
          && level.getEntitiesOfClass(Entity.class, aabb).isEmpty()
          && spawnAtPosition(targetBlockPos, itemStack, level)) {
        return InteractionResult.SUCCESS;
      }
    }

    return InteractionResult.PASS;
  }

  @Override
  public boolean canAttackBlock(
      BlockState blockState, Level level, BlockPos blockPos, Player player) {
    return false;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack, Level level, List<Component> tooltipList, TooltipFlag tooltipFlag) {
    if (hasPreset(itemStack)) {

      // Add preset UUID to tooltip
      UUID presetUUID = getPresetUUID(itemStack);
      if (presetUUID != null) {
        tooltipList.add(
            TextComponent.getText(presetUUID.toString()).withStyle(ChatFormatting.GRAY));
      }

      // Add item hint
      tooltipList.add(
          TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + NAME)
              .withStyle(ChatFormatting.GREEN));

      // Add custom name and entity type to tooltip
      EntityType<?> entityType = getEntityType(itemStack);
      if (entityType != null) {
        String customName = getCustomName(itemStack);
        if (customName != null) {
          tooltipList.add(
              TextComponent.getTranslatedTextRaw(
                      Constants.TEXT_ITEM_PREFIX + NAME + ".custom_name", customName)
                  .withStyle(ChatFormatting.GRAY));
        }
        tooltipList.add(
            TextComponent.getTranslatedTextRaw(
                    Constants.TEXT_ITEM_PREFIX + NAME + ".entity_type", entityType.getDescription())
                .withStyle(ChatFormatting.GRAY));
      }
    }
  }
}
