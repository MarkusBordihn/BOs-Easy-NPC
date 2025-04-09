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
import net.fabricmc.fabric.api.object.builder.v1.block.entity.FabricBlockEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour.Properties;
import net.minecraft.world.level.material.MapColor;

public class ModBlocks {

  public static final Block EASY_NPC_SPAWNER_BOSS =
      new EasyNPCSpawnerBlockWrapper(
          Properties.of()
              .mapColor(MapColor.STONE)
              .requiresCorrectToolForDrops()
              .strength(5.0F)
              .sound(SoundType.METAL)
              .noOcclusion(),
          SpawnerType.BOSS_SPAWNER);
  public static final Block EASY_NPC_SPAWNER_DEFAULT =
      new EasyNPCSpawnerBlockWrapper(
          Properties.of()
              .mapColor(MapColor.STONE)
              .requiresCorrectToolForDrops()
              .strength(5.0F)
              .sound(SoundType.METAL)
              .noOcclusion(),
          SpawnerType.DEFAULT_SPAWNER);
  public static final Block EASY_NPC_SPAWNER_GROUP =
      new EasyNPCSpawnerBlockWrapper(
          Properties.of()
              .mapColor(MapColor.STONE)
              .requiresCorrectToolForDrops()
              .strength(5.0F)
              .sound(SoundType.METAL)
              .noOcclusion(),
          SpawnerType.GROUP_SPAWNER);
  public static final Block EASY_NPC_SPAWNER_SINGLE =
      new EasyNPCSpawnerBlockWrapper(
          Properties.of()
              .mapColor(MapColor.STONE)
              .requiresCorrectToolForDrops()
              .strength(5.0F)
              .sound(SoundType.METAL)
              .noOcclusion(),
          SpawnerType.SINGLE_SPAWNER);

  private ModBlocks() {}

  public static void registerModBlocks() {
    registerBlock(SpawnerType.BOSS_SPAWNER.getId(), EASY_NPC_SPAWNER_BOSS);
    registerBlock(SpawnerType.DEFAULT_SPAWNER.getId(), EASY_NPC_SPAWNER_DEFAULT);
    registerBlock(SpawnerType.GROUP_SPAWNER.getId(), EASY_NPC_SPAWNER_GROUP);
    registerBlock(SpawnerType.SINGLE_SPAWNER.getId(), EASY_NPC_SPAWNER_SINGLE);
  }

  public static void registerModBlockEntities() {
    Registry.register(
        BuiltInRegistries.BLOCK_ENTITY_TYPE,
        ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, EasyNPCSpawnerBlockEntity.NAME),
        EASY_NPC_SPAWNER_ENTITY);
  }

  private static void registerBlock(String id, Block block) {
    Registry.register(
        BuiltInRegistries.BLOCK,
        ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, id),
        block);
  }

  public static final BlockEntityType<EasyNPCSpawnerBlockEntityWrapper> EASY_NPC_SPAWNER_ENTITY =
      FabricBlockEntityTypeBuilder.create(
              EasyNPCSpawnerBlockEntityWrapper::new,
              EASY_NPC_SPAWNER_BOSS,
              EASY_NPC_SPAWNER_DEFAULT,
              EASY_NPC_SPAWNER_GROUP,
              EASY_NPC_SPAWNER_SINGLE)
          .build();
}
