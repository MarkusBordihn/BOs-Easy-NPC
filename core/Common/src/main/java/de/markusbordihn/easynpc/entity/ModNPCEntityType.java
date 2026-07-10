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
import de.markusbordihn.easynpc.data.npc.DefaultNPCType;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.AllayNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.CatNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.ChickenNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.CreeperNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.EnderManNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.FoxNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.GhastNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidSlimNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.IronGolemNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.PigNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.SlimeNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.VexNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.WitchNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.WolfNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.horse.HorseNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.horse.SkeletonHorseNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.horse.ZombieHorseNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.illager.EvokerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.illager.IllusionerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.illager.PillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.illager.VindicatorNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.piglin.PiglinBruteNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.piglin.PiglinNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.piglin.ZombifiedPiglinNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.skeleton.BoggedNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.skeleton.SkeletonNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.skeleton.StrayNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.skeleton.WitherSkeletonNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.spider.CaveSpiderNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.spider.SpiderNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.villager.VillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.villager.WanderingTraderNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.villager.ZombieVillagerNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.zombie.DrownedNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.zombie.HuskNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.zombie.ZombieNPC;
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
      DefaultNPCType.ALLAY.getRegistryId(),
      EntityType.Builder.of(AllayNPC::new, MobCategory.MISC)
          .sized(0.35F, 0.6F)
          .clientTrackingRange(12),
      AllayNPC::createAttributes),
  BOGGED(
      DefaultNPCType.BOGGED.getRegistryId(),
      EntityType.Builder.of(
              (EntityType<BoggedNPC> type, Level level) -> new BoggedNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      BoggedNPC::createAttributes),
  CAT(
      DefaultNPCType.CAT.getRegistryId(),
      EntityType.Builder.of(CatNPC::new, MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      CatNPC::createAttributes),
  CHICKEN(
      DefaultNPCType.CHICKEN.getRegistryId(),
      EntityType.Builder.of(ChickenNPC::new, MobCategory.MISC)
          .sized(0.4F, 0.7F)
          .clientTrackingRange(12),
      ChickenNPC::createAttributes),
  CREEPER(
      DefaultNPCType.CREEPER.getRegistryId(),
      EntityType.Builder.of(CreeperNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.7F)
          .clientTrackingRange(12),
      CreeperNPC::createAttributes),
  DROWNED(
      DefaultNPCType.DROWNED.getRegistryId(),
      EntityType.Builder.of(DrownedNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      DrownedNPC::createAttributes),
  ENDERMAN(
      DefaultNPCType.ENDERMAN.getRegistryId(),
      EntityType.Builder.of(EnderManNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 2.9F)
          .clientTrackingRange(12),
      EnderManNPC::createAttributes),
  EVOKER(
      DefaultNPCType.EVOKER.getRegistryId(),
      EntityType.Builder.of(EvokerNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      EvokerNPC::createAttributes),
  FOX(
      DefaultNPCType.FOX.getRegistryId(),
      EntityType.Builder.of(FoxNPC::new, MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      FoxNPC::createAttributes),
  GHAST(
      DefaultNPCType.GHAST.getRegistryId(),
      EntityType.Builder.of(GhastNPC::new, MobCategory.MONSTER)
          .sized(4.0F, 4.0F)
          .clientTrackingRange(12),
      GhastNPC::createAttributes),
  HORSE(
      DefaultNPCType.HORSE.getRegistryId(),
      EntityType.Builder.of(HorseNPC::new, MobCategory.MISC)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HORSE_SKELETON(
      DefaultNPCType.SKELETON_HORSE.getRegistryId(),
      EntityType.Builder.of(SkeletonHorseNPC::new, MobCategory.MONSTER)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HORSE_ZOMBIE(
      DefaultNPCType.ZOMBIE_HORSE.getRegistryId(),
      EntityType.Builder.of(ZombieHorseNPC::new, MobCategory.MONSTER)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseNPC::createBaseHorseAttributes),
  HUMANOID(
      DefaultNPCType.HUMANOID.getRegistryId(),
      EntityType.Builder.of(HumanoidNPC::new, MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidNPC::createAttributes),
  HUMANOID_SLIM(
      DefaultNPCType.HUMANOID_SLIM.getRegistryId(),
      EntityType.Builder.of(HumanoidSlimNPC::new, MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidSlimNPC::createAttributes),
  ILLUSIONER(
      DefaultNPCType.ILLUSIONER.getRegistryId(),
      EntityType.Builder.of(IllusionerNPC::new, MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      IllusionerNPC::createAttributes),
  IRON_GOLEM(
      DefaultNPCType.IRON_GOLEM.getRegistryId(),
      EntityType.Builder.of(IronGolemNPC::new, MobCategory.MISC)
          .sized(1.4F, 2.7F)
          .clientTrackingRange(12),
      IronGolemNPC::createAttributes),
  PIGLIN(
      DefaultNPCType.PIGLIN.getRegistryId(),
      EntityType.Builder.of(PiglinNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinNPC::createAttributes),
  PIGLIN_BRUTE(
      DefaultNPCType.PIGLIN_BRUTE.getRegistryId(),
      EntityType.Builder.of(PiglinBruteNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinBruteNPC::createAttributes),
  PIGLIN_ZOMBIFIED(
      DefaultNPCType.ZOMBIFIED_PIGLIN.getRegistryId(),
      EntityType.Builder.of(ZombifiedPiglinNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombifiedPiglinNPC::createAttributes),
  PIG(
      DefaultNPCType.PIG.getRegistryId(),
      EntityType.Builder.of(PigNPC::new, MobCategory.MISC)
          .sized(0.9F, 0.9F)
          .clientTrackingRange(12),
      PigNPC::createAttributes),
  PILLAGER(
      DefaultNPCType.PILLAGER.getRegistryId(),
      EntityType.Builder.of(PillagerNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PillagerNPC::createAttributes),
  SKELETON(
      DefaultNPCType.SKELETON.getRegistryId(),
      EntityType.Builder.of(SkeletonNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      SkeletonNPC::createAttributes),
  STRAY(
      DefaultNPCType.STRAY.getRegistryId(),
      EntityType.Builder.of(StrayNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      StrayNPC::createAttributes),
  WITHER_SKELETON(
      DefaultNPCType.WITHER_SKELETON.getRegistryId(),
      EntityType.Builder.of(WitherSkeletonNPC::new, MobCategory.MONSTER)
          .sized(0.7F, 2.4F)
          .clientTrackingRange(12),
      WitherSkeletonNPC::createAttributes),
  SLIME(
      DefaultNPCType.SLIME.getRegistryId(),
      EntityType.Builder.of(
              (EntityType<SlimeNPC> type, Level level) -> new SlimeNPC(type, level),
              MobCategory.MONSTER)
          .sized(0.52F, 0.52F)
          .clientTrackingRange(12),
      SlimeNPC::createAttributes),
  SPIDER(
      DefaultNPCType.SPIDER.getRegistryId(),
      EntityType.Builder.of(SpiderNPC::new, MobCategory.MONSTER)
          .sized(1.4F, 0.9F)
          .clientTrackingRange(12),
      SpiderNPC::createAttributes),
  CAVE_SPIDER(
      DefaultNPCType.CAVE_SPIDER.getRegistryId(),
      EntityType.Builder.of(CaveSpiderNPC::new, MobCategory.MONSTER)
          .sized(0.7F, 0.5F)
          .clientTrackingRange(12),
      SpiderNPC::createAttributes),
  VILLAGER(
      DefaultNPCType.VILLAGER.getRegistryId(),
      EntityType.Builder.of(VillagerNPC::new, MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VillagerNPC::createAttributes),
  WANDERING_TRADER(
      DefaultNPCType.WANDERING_TRADER.getRegistryId(),
      EntityType.Builder.of(WanderingTraderNPC::new, MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      WanderingTraderNPC::createAttributes),
  VEX(
      DefaultNPCType.VEX.getRegistryId(),
      EntityType.Builder.of(VexNPC::new, MobCategory.MONSTER)
          .sized(0.4F, 0.8F)
          .clientTrackingRange(12),
      VexNPC::createAttributes),
  VINDICATOR(
      DefaultNPCType.VINDICATOR.getRegistryId(),
      EntityType.Builder.of(VindicatorNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VindicatorNPC::createAttributes),
  WITCH(
      DefaultNPCType.WITCH.getRegistryId(),
      EntityType.Builder.of(WitchNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      WitchNPC::createAttributes),
  WOLF(
      DefaultNPCType.WOLF.getRegistryId(),
      EntityType.Builder.of(WolfNPC::new, MobCategory.MISC)
          .sized(0.6F, 0.85F)
          .clientTrackingRange(12),
      WolfNPC::createAttributes),
  ZOMBIE(
      DefaultNPCType.ZOMBIE.getRegistryId(),
      EntityType.Builder.of(ZombieNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieNPC::createAttributes),
  ZOMBIE_HUSK(
      DefaultNPCType.HUSK.getRegistryId(),
      EntityType.Builder.of(HuskNPC::new, MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HuskNPC::createAttributes),
  ZOMBIE_VILLAGER(
      DefaultNPCType.ZOMBIE_VILLAGER.getRegistryId(),
      EntityType.Builder.of(ZombieVillagerNPC::new, MobCategory.MONSTER)
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
