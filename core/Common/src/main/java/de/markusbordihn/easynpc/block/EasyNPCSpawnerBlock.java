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

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SpawnerBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.EnumProperty;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCSpawnerBlock extends SpawnerBlock {

  public static final EnumProperty<SpawnerType> SPAWNER_TYPE =
      EnumProperty.create("spawner_type", SpawnerType.class);
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPCSpawnerBlock(final Properties properties, final SpawnerType spawnerType) {
    super(properties);
    this.registerDefaultState(this.stateDefinition.any().setValue(SPAWNER_TYPE, spawnerType));
  }

  public static SpawnerType getSpawnerType(BlockState blockState) {
    return blockState.getValue(SPAWNER_TYPE);
  }

  @Override
  public BlockEntity newBlockEntity(BlockPos blockPos, BlockState blockState) {
    return new EasyNPCSpawnerBlockEntity(null, blockPos, blockState, getSpawnerType(blockState));
  }

  @Override
  protected void createBlockStateDefinition(
      final StateDefinition.Builder<Block, BlockState> blockState) {
    blockState.add(SPAWNER_TYPE);
  }

  @Override
  public void setPlacedBy(
      Level level,
      BlockPos blockPos,
      BlockState blockState,
      LivingEntity livingEntity,
      ItemStack itemStack) {
    if (level.getBlockEntity(blockPos) instanceof EasyNPCSpawnerBlockEntity blockEntityInstance
        && livingEntity != null) {
      blockEntityInstance.setSpawnerUUID(UUID.randomUUID());
      blockEntityInstance.setOwner(livingEntity);
      log.debug(
          "Registered new NPC spawner with UUID {} for owner {} at {}",
          blockEntityInstance.getSpawnerUUID(),
          blockEntityInstance.getOwner(),
          blockPos);
    }
  }
}
