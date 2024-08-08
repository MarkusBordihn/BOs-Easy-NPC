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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public class MoveEasyNPCItem extends Item {

  public static final String ID = "move_easy_npc";
  private final Map<Player, LivingEntity> targetedLivingEntityMap = new HashMap<>();

  public MoveEasyNPCItem(Item.Properties properties) {
    super(properties);
  }

  @Override
  public InteractionResult interactLivingEntity(
      ItemStack itemStack, Player player, LivingEntity livingEntity, InteractionHand hand) {
    if (!player.level.isClientSide
        && livingEntity instanceof EasyNPC<?> easyNPC
        && player instanceof ServerPlayer serverPlayer
        && (serverPlayer.isCreative()
            || (easyNPC.getEasyNPCOwnerData() == null
                && easyNPC.getEasyNPCOwnerData().isOwner(serverPlayer)))) {
      targetedLivingEntityMap.put(player, livingEntity);
      return InteractionResult.SUCCESS;
    }
    return InteractionResult.PASS;
  }

  @Override
  public boolean canAttackBlock(
      BlockState blockState, Level level, BlockPos blockPos, Player player) {
    if (!player.level.isClientSide && targetedLivingEntityMap.containsKey(player)) {
      LivingEntity targetedLivingEntity = targetedLivingEntityMap.get(player);
      targetedLivingEntity.moveTo(
          blockPos.getX() + 0.5, blockPos.above().getY(), blockPos.getZ() + 0.5);
    }
    return false;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack, Level level, List<Component> tooltipList, TooltipFlag tooltipFlag) {
    tooltipList.add(new TranslatableComponent(Constants.TEXT_ITEM_PREFIX + ID));
  }
}
