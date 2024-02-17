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
import de.markusbordihn.easynpc.block.entity.BaseEasyNPCSpawnerBlockEntity;
import de.markusbordihn.easynpc.block.entity.EasyNPCSpawnerBlockEntity;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour.Properties;
import net.minecraft.world.level.material.MapColor;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

public class ModBlocks {

  public static final DeferredRegister<Block> BLOCKS =
      DeferredRegister.create(ForgeRegistries.BLOCKS, Constants.MOD_ID);
  public static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES =
      DeferredRegister.create(ForgeRegistries.BLOCK_ENTITY_TYPES, Constants.MOD_ID);
  public static final RegistryObject<Block> EASY_NPC_SPAWNER =
      BLOCKS.register(
          BaseEasyNPCSpawnerBlock.NAME,
          () ->
              new EasyNPCSpawnerBlock(
                  Properties.of()
                      .mapColor(MapColor.STONE)
                      .requiresCorrectToolForDrops()
                      .strength(5.0F)
                      .sound(SoundType.METAL)
                      .noOcclusion()));

  protected ModBlocks() {}

  public static final RegistryObject<BlockEntityType<EasyNPCSpawnerBlockEntity>>
      EASY_NPC_SPAWNER_ENTITY =
          BLOCK_ENTITY_TYPES.register(
              BaseEasyNPCSpawnerBlockEntity.NAME,
              () ->
                  BlockEntityType.Builder.of(EasyNPCSpawnerBlockEntity::new, EASY_NPC_SPAWNER.get())
                      .build(null));
}
