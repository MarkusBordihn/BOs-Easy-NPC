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
import de.markusbordihn.easynpc.access.AccessManager;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.BaseSpawner;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.SpawnData;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCPresetEmptyItem extends Item {

  public static final String NAME = "easy_npc_preset_empty";

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String FALL_DISTANCE_TAG = "FallDistance";
  private static final String FIRE_TAG = "Fire";
  private static final String ON_GROUND_TAG = "OnGround";
  private static final String SPAWN_DATA_TAG = "SpawnData";
  private static final String ID_TAG = "id";

  public EasyNPCPresetEmptyItem(Properties properties) {
    super(properties);
  }

  @Override
  public InteractionResult interactLivingEntity(
      ItemStack itemStack, Player player, LivingEntity livingEntity, InteractionHand hand) {

    // Ignore players and dead entities for capturing.
    if (livingEntity instanceof Player) {
      return InteractionResult.FAIL;
    }
    Level level = livingEntity.level();

    if (livingEntity instanceof EasyNPC<?> easyNPC && player instanceof ServerPlayer serverPlayer) {
      // Check if player has access to the EasyNPC entity.
      if (!AccessManager.hasAccess(serverPlayer, easyNPC)) {
        return InteractionResult.FAIL;
      }

      // Place the new preset item in the player inventory or drop it.
      ItemStack presetItemStack = createPresetItemStack(easyNPC);
      if (!presetItemStack.isEmpty()) {
        if (!player.getInventory().add(presetItemStack)) {
          player.drop(presetItemStack, false);
        }
        return InteractionResult.SUCCESS;
      }
    }

    return level.isClientSide ? InteractionResult.SUCCESS : InteractionResult.CONSUME;
  }

  private ItemStack createPresetItemStack(EasyNPC<?> easyNPC) {
    // Get preset data from EasyNPC
    PresetDataCapable<?> presetData = easyNPC.getEasyNPCPresetData();
    if (presetData == null) {
      log.error("Can't export preset data from {}", easyNPC);
      return ItemStack.EMPTY;
    }

    return createPresetItemStack(
        easyNPC.getLivingEntity().getType(), presetData.serializePresetData());
  }

  private ItemStack createPresetItemStack(EntityType<?> entityType, CompoundTag compoundTag) {
    // Get new preset item from registry
    Item item =
        BuiltInRegistries.ITEM
            .getOptional(
                ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, EasyNPCPresetItem.NAME))
            .orElse(null);
    if (item == null) {
      log.error("Can't find item for storing preset {}", EasyNPCPresetItem.NAME);
      return ItemStack.EMPTY;
    }

    // Get entity type registry name
    ResourceLocation entityTypeRegistryName = EntityType.getKey(entityType);

    // Clean up preset data
    if (compoundTag.contains(FIRE_TAG)) {
      compoundTag.remove(FIRE_TAG);
    }
    if (compoundTag.contains(FALL_DISTANCE_TAG)) {
      compoundTag.remove(FALL_DISTANCE_TAG);
    }
    if (compoundTag.contains(ON_GROUND_TAG)) {
      compoundTag.remove(ON_GROUND_TAG);
    }

    // Store entity type and preset data in the item stack.
    PresetData presetData = new PresetData(entityType, compoundTag);
    ItemStack presetItemStack = PresetData.set(item, presetData);
    new ItemStack(item);
    log.debug(
        "Captured NPC preset from {} with {} to {}",
        entityTypeRegistryName,
        presetData,
        presetItemStack);

    return presetItemStack;
  }

  @Override
  public InteractionResult useOn(UseOnContext useOnContext) {

    Level level = useOnContext.getLevel();

    // Ignore client side
    if (level.isClientSide) {
      return InteractionResult.SUCCESS;
    }

    // Check for Spawner Block
    BlockPos blockPos = useOnContext.getClickedPos();
    BlockState blockState = level.getBlockState(blockPos);
    if (!blockState.isAir()) {
      BlockEntity blockEntity = level.getBlockEntity(blockPos);
      if (blockEntity instanceof EasyNPCSpawnerBlockEntity spawnerBlockEntity) {
        BaseSpawner baseSpawner = spawnerBlockEntity.getSpawner();
        CompoundTag compoundTag = baseSpawner.save(new CompoundTag());
        if (compoundTag != null && compoundTag.contains(SPAWN_DATA_TAG)) {
          SpawnData spawnData =
              SpawnData.CODEC
                  .parse(NbtOps.INSTANCE, compoundTag.getCompound(SPAWN_DATA_TAG))
                  .resultOrPartial((string) -> log.warn("Invalid SpawnData: {}", string))
                  .orElseGet(SpawnData::new);
          CompoundTag entitySpawnData = spawnData.getEntityToSpawn();
          if (entitySpawnData.contains(ID_TAG)) {
            ResourceLocation entityRegistryName =
                ResourceLocation.tryParse(entitySpawnData.getString(ID_TAG));
            EntityType<?> entityType = BuiltInRegistries.ENTITY_TYPE.get(entityRegistryName);
            if (entityType != null) {
              ItemStack presetItemStack = createPresetItemStack(entityType, entitySpawnData.copy());
              if (!presetItemStack.isEmpty()) {
                Player player = useOnContext.getPlayer();
                if (!player.getInventory().add(presetItemStack)) {
                  player.drop(presetItemStack, false);
                }
                return InteractionResult.SUCCESS;
              }
            }
          }
        }
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
    tooltip.add(
        TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + NAME)
            .withStyle(ChatFormatting.RED));
  }
}
