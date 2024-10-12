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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCPresetEmptyItem extends Item {

  public static final String NAME = "easy_npc_preset_empty";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

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

    if (livingEntity instanceof EasyNPC<?> easyNPC) {
      if (level.isClientSide) {
        return InteractionResult.SUCCESS;
      }

      // Get new preset item from registry
      Item item =
          BuiltInRegistries.ITEM
              .getOptional(new ResourceLocation(Constants.MOD_ID, EasyNPCPresetItem.NAME))
              .orElse(null);
      if (item == null) {
        log.error("Can't find item for storing preset {}", EasyNPCPresetItem.NAME);
        return InteractionResult.FAIL;
      }

      // Get preset data from entity
      PresetData<?> presetData = easyNPC.getEasyNPCPresetData();
      if (presetData == null) {
        log.error("Can't export preset data from {}", easyNPC);
        return InteractionResult.FAIL;
      }

      // Store preset data in compound tag.
      CompoundTag compoundTag = presetData.exportPresetData();

      // Store entity type in preset to easier recreate the entity.
      EntityType<?> entityType = livingEntity.getType();
      ResourceLocation entityTypeRegistryName = EntityType.getKey(entityType);

      // Store entity type and preset data in the item stack.
      ItemStack presetItemStack = new ItemStack(item);
      EasyNPCPresetItem.savePreset(presetItemStack, entityTypeRegistryName, compoundTag);
      log.info("Captured NPC preset from {} with {} to {}", easyNPC, compoundTag, presetItemStack);

      // Place the new preset item in the player inventory or drop it.
      if (!player.getInventory().add(presetItemStack)) {
        player.drop(presetItemStack, false);
      }

      return InteractionResult.SUCCESS;
    }

    return InteractionResult.sidedSuccess(level.isClientSide);
  }

  @Override
  public boolean canAttackBlock(
      BlockState blockState, Level level, BlockPos blockPos, Player player) {
    return false;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack, Level level, List<Component> tooltipList, TooltipFlag tooltipFlag) {
    tooltipList.add(TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + NAME));
  }
}
