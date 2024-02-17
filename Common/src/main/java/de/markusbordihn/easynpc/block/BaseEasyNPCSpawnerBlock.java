/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.block;

import de.markusbordihn.easynpc.block.entity.BaseEasyNPCSpawnerBlockEntity;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseEntityBlock;
import net.minecraft.world.level.block.RenderShape;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;

public class BaseEasyNPCSpawnerBlock extends BaseEntityBlock {

  public static final String NAME = "easy_npc_spawner";

  public BaseEasyNPCSpawnerBlock(Properties properties) {
    super(properties);
  }

  @Override
  public BlockEntity newBlockEntity(BlockPos blockPos, BlockState blockState) {
    return new BaseEasyNPCSpawnerBlockEntity(null, blockPos, blockState);
  }

  protected void openMenu(Level level, BlockPos blockPos, Player player) {
    BlockEntity blockEntity = level.getBlockEntity(blockPos);
    if (blockEntity instanceof BaseEasyNPCSpawnerBlockEntity baseEasyNPCSpawnerBlockEntity) {
      player.openMenu(baseEasyNPCSpawnerBlockEntity);
    }
  }

  @Override
  public void setPlacedBy(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      @Nullable LivingEntity livingEntity,
      ItemStack itemStack) {
    BlockEntity blockEntity = level.getBlockEntity(blockPos);
    if (blockEntity instanceof BaseEasyNPCSpawnerBlockEntity blockEntityInstance
        && livingEntity != null) {
      blockEntityInstance.setSpawnerUUID(UUID.randomUUID());
      blockEntityInstance.setOwner(livingEntity);
    }
  }

  @Override
  public InteractionResult use(
      BlockState blockState,
      Level level,
      BlockPos blockPos,
      Player player,
      InteractionHand interactionHand,
      BlockHitResult blockHitResult) {
    if (level.isClientSide) {
      return InteractionResult.SUCCESS;
    }
    this.openMenu(level, blockPos, player);
    return InteractionResult.CONSUME;
  }

  @Override
  public RenderShape getRenderShape(BlockState blockstate) {
    return RenderShape.MODEL;
  }
}
