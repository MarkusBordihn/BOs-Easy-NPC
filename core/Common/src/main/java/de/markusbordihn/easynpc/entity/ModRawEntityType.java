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
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.AllayRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.BoggedRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.CatRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ChickenRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.CreeperRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.DrownedRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.EnderManRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.EvokerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.FoxRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.HorseRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.HuskRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.IllusionerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.IronGolemRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PigRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PiglinBruteRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PiglinRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.PillagerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.SkeletonRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.SpiderRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.StrayRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VexRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VillagerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.VindicatorRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WitchRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WitherSkeletonRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.WolfRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ZombieRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ZombieVillagerRaw;
import de.markusbordihn.easynpc.entity.easynpc.npc.raw.ZombifiedPiglinRaw;
import java.util.function.Supplier;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.attributes.AttributeSupplier;
import net.minecraft.world.level.Level;

public enum ModRawEntityType implements ModEntityTypeProvider {
  ALLAY(
      AllayRaw.ID,
      EntityType.Builder.of(
              (EntityType<AllayRaw> type, Level level) -> new AllayRaw(type, level),
              MobCategory.MISC)
          .sized(0.35F, 0.6F)
          .clientTrackingRange(12),
      AllayRaw::createAttributes),
  BOGGED(
      BoggedRaw.ID,
      EntityType.Builder.of(
              (EntityType<BoggedRaw> type, Level level) -> new BoggedRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.99F)
          .clientTrackingRange(12),
      BoggedRaw::createAttributes),
  CAT(
      CatRaw.ID,
      EntityType.Builder.of(
              (EntityType<CatRaw> type, Level level) -> new CatRaw(type, level), MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      CatRaw::createAttributes),
  CHICKEN(
      ChickenRaw.ID,
      EntityType.Builder.of(
              (EntityType<ChickenRaw> type, Level level) -> new ChickenRaw(type, level),
              MobCategory.MISC)
          .sized(0.4F, 0.7F)
          .clientTrackingRange(12),
      ChickenRaw::createAttributes),
  CREEPER(
      CreeperRaw.ID,
      EntityType.Builder.of(
              (EntityType<CreeperRaw> type, Level level) -> new CreeperRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.7F)
          .clientTrackingRange(12),
      CreeperRaw::createAttributes),
  DROWNED(
      DrownedRaw.ID,
      EntityType.Builder.of(
              (EntityType<DrownedRaw> type, Level level) -> new DrownedRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      DrownedRaw::createAttributes),
  FOX(
      FoxRaw.ID,
      EntityType.Builder.of(
              (EntityType<FoxRaw> type, Level level) -> new FoxRaw(type, level), MobCategory.MISC)
          .sized(0.6F, 0.7F)
          .clientTrackingRange(12),
      FoxRaw::createAttributes),
  HORSE(
      HorseRaw.ID,
      EntityType.Builder.of(
              (EntityType<HorseRaw> type, Level level) -> new HorseRaw(type, level),
              MobCategory.MISC)
          .sized(1.4F, 1.6F)
          .clientTrackingRange(12),
      HorseRaw::createBaseHorseAttributes),
  ILLUSIONER(
      IllusionerRaw.ID,
      EntityType.Builder.of(
              (EntityType<IllusionerRaw> type, Level level) -> new IllusionerRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      IllusionerRaw::createAttributes),
  IRON_GOLEM(
      IronGolemRaw.ID,
      EntityType.Builder.of(
              (EntityType<IronGolemRaw> type, Level level) -> new IronGolemRaw(type, level),
              MobCategory.MISC)
          .sized(1.4F, 2.7F)
          .clientTrackingRange(12),
      IronGolemRaw::createAttributes),
  ENDER_MAN(
      EnderManRaw.ID,
      EntityType.Builder.of(
              (EntityType<EnderManRaw> type, Level level) -> new EnderManRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 2.9F)
          .clientTrackingRange(12),
      EnderManRaw::createAttributes),
  PIGLIN(
      PiglinRaw.ID,
      EntityType.Builder.of(
              (EntityType<PiglinRaw> type, Level level) -> new PiglinRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinRaw::createAttributes),
  PIGLIN_BRUTE(
      PiglinBruteRaw.ID,
      EntityType.Builder.of(
              (EntityType<PiglinBruteRaw> type, Level level) -> new PiglinBruteRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PiglinBruteRaw::createAttributes),
  ZOMBIFIED_PIGLIN(
      ZombifiedPiglinRaw.ID,
      EntityType.Builder.of(
              (EntityType<ZombifiedPiglinRaw> type, Level level) ->
                  new ZombifiedPiglinRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombifiedPiglinRaw::createAttributes),
  EVOKER(
      EvokerRaw.ID,
      EntityType.Builder.of(
              (EntityType<EvokerRaw> type, Level level) -> new EvokerRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      EvokerRaw::createAttributes),
  PIG(
      PigRaw.ID,
      EntityType.Builder.of(
              (EntityType<PigRaw> type, Level level) -> new PigRaw(type, level), MobCategory.MISC)
          .sized(0.9F, 0.9F)
          .clientTrackingRange(12),
      PigRaw::createAttributes),
  PILLAGER(
      PillagerRaw.ID,
      EntityType.Builder.of(
              (EntityType<PillagerRaw> type, Level level) -> new PillagerRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      PillagerRaw::createAttributes),
  SKELETON(
      SkeletonRaw.ID,
      EntityType.Builder.of(
              (EntityType<SkeletonRaw> type, Level level) -> new SkeletonRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      SkeletonRaw::createAttributes),
  STRAY(
      StrayRaw.ID,
      EntityType.Builder.of(
              (EntityType<StrayRaw> type, Level level) -> new StrayRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      StrayRaw::createAttributes),
  WITHER_SKELETON(
      WitherSkeletonRaw.ID,
      EntityType.Builder.of(
              (EntityType<WitherSkeletonRaw> type, Level level) ->
                  new WitherSkeletonRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.7F, 2.4F)
          .clientTrackingRange(12),
      WitherSkeletonRaw::createAttributes),
  SPIDER(
      SpiderRaw.ID,
      EntityType.Builder.of(
              (EntityType<SpiderRaw> type, Level level) -> new SpiderRaw(type, level),
              MobCategory.MONSTER)
          .sized(1.4F, 0.9F)
          .clientTrackingRange(12),
      SpiderRaw::createAttributes),
  VEX(
      VexRaw.ID,
      EntityType.Builder.of(
              (EntityType<VexRaw> type, Level level) -> new VexRaw(type, level), MobCategory.MISC)
          .sized(0.4F, 0.8F)
          .clientTrackingRange(12),
      VexRaw::createAttributes),
  VILLAGER(
      VillagerRaw.ID,
      EntityType.Builder.of(
              (EntityType<VillagerRaw> type, Level level) -> new VillagerRaw(type, level),
              MobCategory.MISC)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VillagerRaw::createAttributes),
  VINDICATOR(
      VindicatorRaw.ID,
      EntityType.Builder.of(
              (EntityType<VindicatorRaw> type, Level level) -> new VindicatorRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      VindicatorRaw::createAttributes),
  WITCH(
      WitchRaw.ID,
      EntityType.Builder.of(
              (EntityType<WitchRaw> type, Level level) -> new WitchRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      WitchRaw::createAttributes),
  WOLF(
      WolfRaw.ID,
      EntityType.Builder.of(
              (EntityType<WolfRaw> type, Level level) -> new WolfRaw(type, level), MobCategory.MISC)
          .sized(0.6F, 0.85F)
          .clientTrackingRange(12),
      WolfRaw::createAttributes),
  ZOMBIE(
      ZombieRaw.ID,
      EntityType.Builder.of(
              (EntityType<ZombieRaw> type, Level level) -> new ZombieRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieRaw::createAttributes),
  HUSK(
      HuskRaw.ID,
      EntityType.Builder.of(
              (EntityType<HuskRaw> type, Level level) -> new HuskRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      HuskRaw::createAttributes),
  ZOMBIE_VILLAGER(
      ZombieVillagerRaw.ID,
      EntityType.Builder.of(
              (EntityType<ZombieVillagerRaw> type, Level level) ->
                  new ZombieVillagerRaw(type, level),
              MobCategory.MONSTER)
          .sized(0.6F, 1.95F)
          .clientTrackingRange(12),
      ZombieVillagerRaw::createAttributes);

  private final String id;
  private final EntityType.Builder<? extends Entity> builder;
  private final Supplier<AttributeSupplier.Builder> attributes;
  private final ResourceKey<EntityType<?>> resourceKey;

  ModRawEntityType(
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
