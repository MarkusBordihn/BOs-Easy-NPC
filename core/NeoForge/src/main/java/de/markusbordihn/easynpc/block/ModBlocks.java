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
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntityWrapper;
import de.markusbordihn.easynpc.data.spawner.SpawnerType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour.Properties;
import net.minecraft.world.level.material.MapColor;
import net.neoforged.neoforge.registries.DeferredBlock;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

public class ModBlocks {

  public static final DeferredRegister.Blocks BLOCKS =
      DeferredRegister.createBlocks(Constants.MOD_ID);
  public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES =
      DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE, Constants.MOD_ID);

  public static final DeferredBlock<Block> EASY_NPC_SPAWNER_BOSS =
      BLOCKS.register(
          SpawnerType.BOSS_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockWrapper(
                  Properties.of()
                      .mapColor(MapColor.STONE)
                      .requiresCorrectToolForDrops()
                      .strength(5.0F)
                      .sound(SoundType.METAL)
                      .noOcclusion(),
                  SpawnerType.BOSS_SPAWNER));
  public static final DeferredBlock<Block> EASY_NPC_SPAWNER_DEFAULT =
      BLOCKS.register(
          SpawnerType.DEFAULT_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockWrapper(
                  Properties.of()
                      .mapColor(MapColor.STONE)
                      .requiresCorrectToolForDrops()
                      .strength(5.0F)
                      .sound(SoundType.METAL)
                      .noOcclusion(),
                  SpawnerType.DEFAULT_SPAWNER));
  public static final DeferredBlock<Block> EASY_NPC_SPAWNER_GROUP =
      BLOCKS.register(
          SpawnerType.GROUP_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockWrapper(
                  Properties.of()
                      .mapColor(MapColor.STONE)
                      .requiresCorrectToolForDrops()
                      .strength(5.0F)
                      .sound(SoundType.METAL)
                      .noOcclusion(),
                  SpawnerType.GROUP_SPAWNER));
  public static final DeferredBlock<Block> EASY_NPC_SPAWNER_SINGLE =
      BLOCKS.register(
          SpawnerType.SINGLE_SPAWNER.getId(),
          () ->
              new EasyNPCSpawnerBlockWrapper(
                  Properties.of()
                      .mapColor(MapColor.STONE)
                      .requiresCorrectToolForDrops()
                      .strength(5.0F)
                      .sound(SoundType.METAL)
                      .noOcclusion(),
                  SpawnerType.SINGLE_SPAWNER));

  private ModBlocks() {}

  public static final DeferredHolder<
          BlockEntityType<?>, BlockEntityType<EasyNPCSpawnerBlockEntityWrapper>>
      EASY_NPC_SPAWNER_ENTITY =
          BLOCK_ENTITY_TYPES.register(
              EasyNPCSpawnerBlockEntity.NAME,
              () ->
                  BlockEntityType.Builder.of(
                          EasyNPCSpawnerBlockEntityWrapper::new,
                          EASY_NPC_SPAWNER_DEFAULT.get(),
                          EASY_NPC_SPAWNER_BOSS.get(),
                          EASY_NPC_SPAWNER_GROUP.get(),
                          EASY_NPC_SPAWNER_SINGLE.get())
                      .build(null));
}
