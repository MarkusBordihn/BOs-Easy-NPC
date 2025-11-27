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

package de.markusbordihn.easynpc.entity;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.skin.variant.HorseSkinVariant;
import de.markusbordihn.easynpc.data.skin.variant.SpiderSkinVariant;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.AllayNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.BoggedNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.CatNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.ChickenNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.CreeperNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.DrownedNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.EnderManNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.EvokerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.FoxNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HorseNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidSlimNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HuskNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.IllusionerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.IronGolemNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.PigNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.PiglinBruteNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.PiglinNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.PillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.SkeletonNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.SpiderNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.StrayNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.VexNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.VillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.VindicatorNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.WitchNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.WitherSkeletonNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.WolfNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.ZombieNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.ZombieVillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.ZombifiedPiglinNPC;
import java.util.function.Supplier;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.level.Level;

public enum ModNPCEntityType implements ModEntityTypeProvider {
  ALLAY(
      AllayNPC.ID,
      EntityType.Builder.of(
              (EntityType<AllayNPC> type, Level level) -> new AllayNPC(type, level),
              MobCategory.MISC)
          .sized(0.35F, 0.6F)
          .clientTrackingRange(12),
      AllayNPC::createAttributes),
  BOGGED(
      BoggedNPC.ID,
      EntityType.Builder.of(
              (EntityType<BoggedNPC> type, Level level) -> new BoggedNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      BoggedNPC::createAttributes),
  CAT(
      CatNPC.ID,
      EntityType.Builder.of(
              (EntityType<CatNPC> type, Level level) -> new CatNPC(type, level), MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      CatNPC::createAttributes),
  CHICKEN(
      ChickenNPC.ID,
      EntityType.Builder.of(
              (EntityType<ChickenNPC> type, Level level) -> new ChickenNPC(type, level),
              MobCategory.MISC)
          .sized(0.4F, 0.7F)
          .clientTrackingRange(12),
      ChickenNPC::createAttributes),
  CREEPER(
      CreeperNPC.ID,
      EntityType.Builder.of(
              (EntityType<CreeperNPC> type, Level level) -> new CreeperNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.7F)
          .clientTrackingRange(12),
      CreeperNPC::createAttributes),
  DROWNED(
      DrownedNPC.ID,
      EntityType.Builder.of(
              (EntityType<DrownedNPC> type, Level level) -> new DrownedNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      DrownedNPC::createAttributes),
  ENDERMAN(
      EnderManNPC.ID,
      EntityType.Builder.of(
              (EntityType<EnderManNPC> type, Level level) -> new EnderManNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 2.9F)
          .clientTrackingRange(12),
      EnderManNPC::createAttributes),
  EVOKER(
      EvokerNPC.ID,
      EntityType.Builder.of(
              (EntityType<EvokerNPC> type, Level level) -> new EvokerNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      EvokerNPC::createAttributes),
  FOX(
      FoxNPC.ID,
      EntityType.Builder.of(
              (EntityType<FoxNPC> type, Level level) -> new FoxNPC(type, level), MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      FoxNPC::createAttributes),
  HORSE(
      HorseNPC.ID,
      EntityType.Builder.of(
              (EntityType<HorseNPC> type, Level level) -> new HorseNPC(type, level),
              MobCategory.MISC)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HORSE_SKELETON(
      HorseNPC.ID_SKELETON,
      EntityType.Builder.of(
              (EntityType<HorseNPC> type, Level level) ->
                  new HorseNPC(type, level, HorseSkinVariant.SKELETON),
              MobCategory.MONSTER)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HORSE_ZOMBIE(
      HorseNPC.ID_ZOMBIE,
      EntityType.Builder.of(
              (EntityType<HorseNPC> type, Level level) ->
                  new HorseNPC(type, level, HorseSkinVariant.ZOMBIE),
              MobCategory.MONSTER)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HUMANOID(
      HumanoidNPC.ID,
      EntityType.Builder.of(
              (EntityType<HumanoidNPC> type, Level level) -> new HumanoidNPC(type, level),
              MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidNPC::createAttributes),
  HUMANOID_SLIM(
      HumanoidSlimNPC.ID,
      EntityType.Builder.of(
              (EntityType<HumanoidSlimNPC> type, Level level) -> new HumanoidSlimNPC(type, level),
              MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidSlimNPC::createAttributes),
  ILLUSIONER(
      IllusionerNPC.ID,
      EntityType.Builder.of(
              (EntityType<IllusionerNPC> type, Level level) -> new IllusionerNPC(type, level),
              MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      IllusionerNPC::createAttributes),
  IRON_GOLEM(
      IronGolemNPC.ID,
      EntityType.Builder.of(
              (EntityType<IronGolemNPC> type, Level level) -> new IronGolemNPC(type, level),
              MobCategory.MISC)
          .sized(1.4F, 2.7F)
          .clientTrackingRange(12),
      IronGolemNPC::createAttributes),
  PIGLIN(
      PiglinNPC.ID,
      EntityType.Builder.of(
              (EntityType<PiglinNPC> type, Level level) -> new PiglinNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinNPC::createAttributes),
  PIGLIN_BRUTE(
      PiglinBruteNPC.ID,
      EntityType.Builder.of(
              (EntityType<PiglinBruteNPC> type, Level level) -> new PiglinBruteNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinBruteNPC::createAttributes),
  PIGLIN_ZOMBIFIED(
      ZombifiedPiglinNPC.ID,
      EntityType.Builder.of(
              (EntityType<ZombifiedPiglinNPC> type, Level level) ->
                  new ZombifiedPiglinNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombifiedPiglinNPC::createAttributes),
  PIG(
      PigNPC.ID,
      EntityType.Builder.of(
              (EntityType<PigNPC> type, Level level) -> new PigNPC(type, level), MobCategory.MISC)
          .sized(0.9F, 0.9F)
          .clientTrackingRange(12),
      PigNPC::createAttributes),
  PILLAGER(
      PillagerNPC.ID,
      EntityType.Builder.of(
              (EntityType<PillagerNPC> type, Level level) -> new PillagerNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PillagerNPC::createAttributes),
  SKELETON(
      SkeletonNPC.ID,
      EntityType.Builder.of(
              (EntityType<SkeletonNPC> type, Level level) -> new SkeletonNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      SkeletonNPC::createAttributes),
  STRAY(
      StrayNPC.ID,
      EntityType.Builder.of(
              (EntityType<StrayNPC> type, Level level) -> new StrayNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      StrayNPC::createAttributes),
  WITHER_SKELETON(
      WitherSkeletonNPC.ID,
      EntityType.Builder.of(
              (EntityType<WitherSkeletonNPC> type, Level level) ->
                  new WitherSkeletonNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.7F, 2.4F)
          .clientTrackingRange(12),
      WitherSkeletonNPC::createAttributes),
  SPIDER(
      SpiderNPC.ID,
      EntityType.Builder.of(
              (EntityType<SpiderNPC> type, Level level) -> new SpiderNPC(type, level),
              MobCategory.MONSTER)
          .sized(1.4F, 0.9F)
          .clientTrackingRange(12),
      SpiderNPC::createAttributes),
  CAVE_SPIDER(
      SpiderNPC.CAVE_SPIDER_ID,
      EntityType.Builder.of(
              (EntityType<SpiderNPC> type, Level level) ->
                  new SpiderNPC(type, level, SpiderSkinVariant.CAVE_SPIDER),
              MobCategory.MONSTER)
          .sized(0.7F, 0.5F)
          .clientTrackingRange(12),
      SpiderNPC::createAttributes),
  VILLAGER(
      VillagerNPC.ID,
      EntityType.Builder.of(
              (EntityType<VillagerNPC> type, Level level) -> new VillagerNPC(type, level),
              MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VillagerNPC::createAttributes),
  VEX(
      VexNPC.ID,
      EntityType.Builder.of(
              (EntityType<VexNPC> type, Level level) -> new VexNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.4F, 0.8F)
          .clientTrackingRange(12),
      VexNPC::createAttributes),
  VINDICATOR(
      VindicatorNPC.ID,
      EntityType.Builder.of(
              (EntityType<VindicatorNPC> type, Level level) -> new VindicatorNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VindicatorNPC::createAttributes),
  WITCH(
      WitchNPC.ID,
      EntityType.Builder.of(
              (EntityType<WitchNPC> type, Level level) -> new WitchNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      WitchNPC::createAttributes),
  WOLF(
      WolfNPC.ID,
      EntityType.Builder.of(
              (EntityType<WolfNPC> type, Level level) -> new WolfNPC(type, level), MobCategory.MISC)
          .sized(0.6F, 0.85F)
          .clientTrackingRange(12),
      WolfNPC::createAttributes),
  ZOMBIE(
      ZombieNPC.ID,
      EntityType.Builder.of(
              (EntityType<ZombieNPC> type, Level level) -> new ZombieNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieNPC::createAttributes),
  ZOMBIE_HUSK(
      HuskNPC.ID,
      EntityType.Builder.of(
              (EntityType<HuskNPC> type, Level level) -> new HuskNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HuskNPC::createAttributes),
  ZOMBIE_VILLAGER(
      ZombieVillagerNPC.ID,
      EntityType.Builder.of(
              (EntityType<ZombieVillagerNPC> type, Level level) ->
                  new ZombieVillagerNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieVillagerNPC::createAttributes);

  private final String id;
  private final EntityType.Builder<? extends Entity> builder;
  private final Supplier<AttributeSupplier.Builder> attributes;
  private final ResourceKey<EntityType<?>> resourceKey;

  ModNPCEntityType(
      String id,
      EntityType.Builder<? extends Entity> builder,
      Supplier<AttributeSupplier.Builder> attributes) {
    this.id = id;
    this.builder = builder;
    this.attributes = attributes;
    this.resourceKey =
        ResourceKey.create(
            Registries.ENTITY_TYPE, ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, id));
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public EntityType.Builder<? extends Entity> getBuilder() {
    return builder;
  }

  @Override
  public ResourceKey<EntityType<?>> getResourceKey() {
    return resourceKey;
  }

  public AttributeSupplier.Builder getAttributes() {
    return attributes.get();
  }
}
