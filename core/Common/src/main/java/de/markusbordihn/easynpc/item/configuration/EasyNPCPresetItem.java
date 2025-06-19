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
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.level.BaseEasyNPCSpawner;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.SpawnerUtils;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.BlockPos.MutableBlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
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

  public static final String NAME = "easy_npc_preset";

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final String CUSTOM_NAME_TAG = "CustomName";
  private static final String TEXT_TAG = "text";

  public EasyNPCPresetItem(Properties properties) {
    super(properties);
  }

  public static UUID getPresetUUID(ItemStack itemStack) {
    PresetData presetData = PresetData.get(itemStack);
    if (presetData == null || presetData.data() == null) {
      return null;
    }
    CompoundTag compoundTag = presetData.data();
    if (compoundTag.contains(PresetDataCapable.PRESET_UUID_TAG)) {
      return compoundTag.getUUID(PresetDataCapable.PRESET_UUID_TAG);
    }
    return null;
  }

  public static String getCustomName(ItemStack itemStack) {
    PresetData presetData = PresetData.get(itemStack);
    if (presetData == null || presetData.data() == null) {
      return null;
    }
    CompoundTag compoundTag = presetData.data();
    if (compoundTag.contains(CUSTOM_NAME_TAG)) {
      CompoundTag customNameTag = compoundTag.getCompound(CUSTOM_NAME_TAG);
      if (customNameTag.contains(TEXT_TAG)) {
        return customNameTag.getString(TEXT_TAG);
      }
    }
    return null;
  }

  public static boolean spawnAtPosition(BlockPos blockPos, ItemStack itemStack, Level level) {
    // Ignore client side
    if (level.isClientSide) {
      return false;
    }

    // Verify preset data
    PresetData presetData = PresetData.get(itemStack);
    if (presetData == null || !presetData.hasEntityType() || !presetData.hasData()) {
      log.error("No valid preset data found in {}!", itemStack);
      return false;
    }

    // Create and validate entity.
    EntityType<?> entityType = presetData.entityType();
    Entity entity = entityType.create(level);
    if (entity == null) {
      log.error("Unable to create entity for {} in {}", entityType, level);
      return false;
    }

    // Remove UUID from preset, to avoid conflicts with existing entities.
    CompoundTag entityData = presetData.data();
    if (entityData.contains(Entity.UUID_TAG)) {
      entityData.remove(Entity.UUID_TAG);
    }
    entity.load(entityData);

    // Move entity to and spawn entity.
    entity.moveTo(blockPos.getX() + 0.5f, blockPos.getY(), blockPos.getZ() + 0.5f);
    if (level.addFreshEntity(entity)) {
      UUID presetUUID = getPresetUUID(itemStack);
      log.debug(
          "Spawned {} at {} with preset UUID {} and {} in {}",
          entityType,
          blockPos,
          presetUUID,
          entityData,
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
    // Verify preset data
    PresetData presetData = PresetData.get(itemStack);
    if (presetData == null || !presetData.hasEntityType() || !presetData.hasData()) {
      log.warn("No valid preset found in {}!", itemStack);
      return InteractionResult.FAIL;
    }

    // Check for Spawner Block
    BlockPos blockPos = useOnContext.getClickedPos();
    BlockEntity blockEntity = level.getBlockEntity(blockPos);
    if (blockEntity instanceof SpawnerBlockEntity spawnerBlockEntity) {
      BaseSpawner baseSpawner = spawnerBlockEntity.getSpawner();
      SpawnData spawnData = new SpawnData(presetData.data(), Optional.empty(), Optional.empty());
      log.debug("Set spawn data {} for spawner {} at {}", spawnData, spawnerBlockEntity, blockPos);
      if (!SpawnerUtils.setNextSpawnData(baseSpawner, level, blockPos, spawnData)) {
        return InteractionResult.FAIL;
      }
      spawnerBlockEntity.setChanged();
      itemStack.shrink(1);
      return InteractionResult.CONSUME;
    }

    // Check for NPC Spawner Block
    if (blockEntity instanceof EasyNPCSpawnerBlockEntity easyNPCSpawnerBlockEntity) {
      BaseEasyNPCSpawner baseEasyNPCSpawner = easyNPCSpawnerBlockEntity.getSpawner();
      SpawnData spawnData = new SpawnData(presetData.data(), Optional.empty(), Optional.empty());
      log.debug(
          "Set spawn data {} for base NPC spawner {} at {}",
          spawnData,
          easyNPCSpawnerBlockEntity,
          blockPos);
      baseEasyNPCSpawner.setNextSpawnData(level, blockPos, spawnData);
      easyNPCSpawnerBlockEntity.setChanged();
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
              possibleSpawnPosition.getX(),
              possibleSpawnPosition.getY() + 1,
              possibleSpawnPosition.getZ());
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
      ItemStack itemStack,
      TooltipContext tooltipContext,
      List<Component> tooltip,
      TooltipFlag flag) {
    PresetData presetData = PresetData.get(itemStack);
    if (presetData == null) {
      return;
    }

    // Add preset UUID to tooltip
    UUID presetUUID = getPresetUUID(itemStack);
    if (presetUUID != null) {
      tooltip.add(TextComponent.getText(presetUUID.toString()).withStyle(ChatFormatting.GRAY));
    }

    // Add item hint
    tooltip.add(
        TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + NAME)
            .withStyle(ChatFormatting.GREEN));

    // Add custom name and entity type to tooltip
    EntityType<?> entityType = presetData.entityType();
    if (entityType != null) {
      String customName = getCustomName(itemStack);
      if (customName != null) {
        tooltip.add(
            TextComponent.getTranslatedTextRaw(
                    Constants.TEXT_ITEM_PREFIX + NAME + ".custom_name", customName)
                .withStyle(ChatFormatting.GRAY));
      }
      tooltip.add(
          TextComponent.getTranslatedTextRaw(
                  Constants.TEXT_ITEM_PREFIX + NAME + ".entity_type", entityType.getDescription())
              .withStyle(ChatFormatting.GRAY));
    }
  }
}
