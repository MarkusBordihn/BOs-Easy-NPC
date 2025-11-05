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
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.CaveSpiderEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.CreeperEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.DrownedEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.EnderManEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.EvokerEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.HumanoidEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.HumanoidSlimEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.HuskEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.IronGolemEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.PiglinBruteEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.PiglinEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.PillagerEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.SkeletonEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.SpiderEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.StrayEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.VexEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.VindicatorEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.WitchEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.WitherSkeletonEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.ZombieEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.ZombieVillagerEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.epicfight.ZombifiedPiglinEpicFight;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidNPC;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidSlimNPC;
import java.util.function.Supplier;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.level.Level;

public enum EpicFightEntityType implements ModEntityTypeProvider {
  ZOMBIE(
      ZombieEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<ZombieEpicFight> type, Level level) -> new ZombieEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieEpicFight::createAttributes),
  CREEPER(
      CreeperEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<CreeperEpicFight> type, Level level) -> new CreeperEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.7F)
          .clientTrackingRange(12),
      CreeperEpicFight::createAttributes),
  ENDERMAN(
      EnderManEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<EnderManEpicFight> type, Level level) ->
                  new EnderManEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 2.9F)
          .clientTrackingRange(12),
      EnderManEpicFight::createAttributes),
  SKELETON(
      SkeletonEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<SkeletonEpicFight> type, Level level) ->
                  new SkeletonEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      SkeletonEpicFight::createAttributes),
  WITHER_SKELETON(
      WitherSkeletonEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<WitherSkeletonEpicFight> type, Level level) ->
                  new WitherSkeletonEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.7F, 2.4F)
          .clientTrackingRange(12),
      WitherSkeletonEpicFight::createAttributes),
  STRAY(
      StrayEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<StrayEpicFight> type, Level level) -> new StrayEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      StrayEpicFight::createAttributes),
  ZOMBIFIED_PIGLIN(
      ZombifiedPiglinEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<ZombifiedPiglinEpicFight> type, Level level) ->
                  new ZombifiedPiglinEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombifiedPiglinEpicFight::createAttributes),
  ZOMBIE_VILLAGER(
      ZombieVillagerEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<ZombieVillagerEpicFight> type, Level level) ->
                  new ZombieVillagerEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieVillagerEpicFight::createAttributes),
  HUSK(
      HuskEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<HuskEpicFight> type, Level level) -> new HuskEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HuskEpicFight::createAttributes),
  SPIDER(
      SpiderEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<SpiderEpicFight> type, Level level) -> new SpiderEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(1.4F, 0.9F)
          .clientTrackingRange(12),
      SpiderEpicFight::createAttributes),
  CAVE_SPIDER(
      CaveSpiderEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<CaveSpiderEpicFight> type, Level level) ->
                  new CaveSpiderEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.7F, 0.5F)
          .clientTrackingRange(12),
      CaveSpiderEpicFight::createAttributes),
  IRON_GOLEM(
      IronGolemEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<IronGolemEpicFight> type, Level level) ->
                  new IronGolemEpicFight(type, level),
              MobCategory.MISC)
          .sized(1.4F, 2.7F)
          .clientTrackingRange(12),
      IronGolemEpicFight::createAttributes),
  VINDICATOR(
      VindicatorEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<VindicatorEpicFight> type, Level level) ->
                  new VindicatorEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VindicatorEpicFight::createAttributes),
  EVOKER(
      EvokerEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<EvokerEpicFight> type, Level level) -> new EvokerEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      EvokerEpicFight::createAttributes),
  WITCH(
      WitchEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<WitchEpicFight> type, Level level) -> new WitchEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      WitchEpicFight::createAttributes),
  DROWNED(
      DrownedEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<DrownedEpicFight> type, Level level) -> new DrownedEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      DrownedEpicFight::createAttributes),
  PILLAGER(
      PillagerEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<PillagerEpicFight> type, Level level) ->
                  new PillagerEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PillagerEpicFight::createAttributes),
  VEX(
      VexEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<VexEpicFight> type, Level level) -> new VexEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.4F, 0.8F)
          .clientTrackingRange(12),
      VexEpicFight::createAttributes),
  PIGLIN(
      PiglinEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<PiglinEpicFight> type, Level level) -> new PiglinEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinEpicFight::createAttributes),
  PIGLIN_BRUTE(
      PiglinBruteEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<PiglinBruteEpicFight> type, Level level) ->
                  new PiglinBruteEpicFight(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinBruteEpicFight::createAttributes),
  HUMANOID(
      HumanoidEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<HumanoidEpicFight> type, Level level) ->
                  new HumanoidEpicFight(type, level),
              MobCategory.CREATURE)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidNPC::createAttributes),
  HUMANOID_SLIM(
      HumanoidSlimEpicFight.ID,
      EntityType.Builder.of(
              (EntityType<HumanoidSlimEpicFight> type, Level level) ->
                  new HumanoidSlimEpicFight(type, level),
              MobCategory.CREATURE)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HumanoidSlimNPC::createAttributes);

  private final String id;
  private final EntityType.Builder<? extends Entity> builder;
  private final Supplier<AttributeSupplier.Builder> attributes;
  private final ResourceKey<EntityType<?>> resourceKey;

  EpicFightEntityType(
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
