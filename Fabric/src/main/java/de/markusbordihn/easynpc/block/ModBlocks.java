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
import net.fabricmc.fabric.api.object.builder.v1.block.entity.FabricBlockEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour.Properties;
import net.minecraft.world.level.material.Material;

public class ModBlocks {

  public static final Block EASY_NPC_SPAWNER =
      new EasyNPCSpawnerBlock(
          Properties.of(Material.STONE)
              .requiresCorrectToolForDrops()
              .strength(5.0F)
              .sound(SoundType.METAL)
              .noOcclusion());

  private ModBlocks() {
  }

  public static void registerModBlocks() {
    registerBlock(BaseEasyNPCSpawnerBlock.NAME, EASY_NPC_SPAWNER);
  }

  public static void registerModBlockEntities() {
    Registry.register(
        Registry.BLOCK_ENTITY_TYPE,
        new ResourceLocation(Constants.MOD_ID, BaseEasyNPCSpawnerBlockEntity.NAME),
        EASY_NPC_SPAWNER_ENTITY);
  }

  private static void registerBlock(String id, Block block) {
    Registry.register(Registry.BLOCK, new ResourceLocation(Constants.MOD_ID, id), block);
  }

  public static final BlockEntityType<EasyNPCSpawnerBlockEntity> EASY_NPC_SPAWNER_ENTITY =
      FabricBlockEntityTypeBuilder.create(EasyNPCSpawnerBlockEntity::new, EASY_NPC_SPAWNER).build();
}
