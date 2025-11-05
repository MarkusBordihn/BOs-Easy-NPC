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

package de.markusbordihn.easynpc.client.renderer.entity;

import de.markusbordihn.easynpc.client.renderer.entity.raw.AllayRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.BoggedRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.CatRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ChickenRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.CreeperRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.DrownedRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.EnderManRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.EvokerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.FoxRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.HorseRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.HuskRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.IllusionerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.IronGolemRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PigRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PiglinBruteRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PiglinRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.SkeletonRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.SpiderRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.StrayRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VexRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VindicatorRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.WitchRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.WitherSkeletonRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.WolfRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieVillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombifiedPiglinRawRenderer;
import de.markusbordihn.easynpc.entity.ModRawEntityType;
import java.util.function.Function;
import java.util.function.Supplier;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider.Context;
import net.minecraft.world.entity.Entity;

public enum ModRawEntityRenderer {
  ALLAY(ModRawEntityType.ALLAY, () -> AllayRawRenderer::new),
  BOGGED(ModRawEntityType.BOGGED, () -> BoggedRawRenderer::new),
  CAT(ModRawEntityType.CAT, () -> CatRawRenderer::new),
  CHICKEN(ModRawEntityType.CHICKEN, () -> ChickenRawRenderer::new),
  CREEPER(ModRawEntityType.CREEPER, () -> CreeperRawRenderer::new),
  DROWNED(ModRawEntityType.DROWNED, () -> DrownedRawRenderer::new),
  ENDER_MAN(ModRawEntityType.ENDER_MAN, () -> EnderManRawRenderer::new),
  EVOKER(ModRawEntityType.EVOKER, () -> EvokerRawRenderer::new),
  FOX(ModRawEntityType.FOX, () -> FoxRawRenderer::new),
  HORSE(ModRawEntityType.HORSE, () -> HorseRawRenderer::new),
  HUSK(ModRawEntityType.HUSK, () -> HuskRawRenderer::new),
  ILLUSIONER(ModRawEntityType.ILLUSIONER, () -> IllusionerRawRenderer::new),
  IRON_GOLEM(ModRawEntityType.IRON_GOLEM, () -> IronGolemRawRenderer::new),
  PIG(ModRawEntityType.PIG, () -> PigRawRenderer::new),
  PIGLIN_BRUTE(ModRawEntityType.PIGLIN_BRUTE, () -> PiglinBruteRawRenderer::new),
  PIGLIN(ModRawEntityType.PIGLIN, () -> PiglinRawRenderer::new),
  PILLAGER(ModRawEntityType.PILLAGER, () -> PillagerRawRenderer::new),
  SKELETON(ModRawEntityType.SKELETON, () -> SkeletonRawRenderer::new),
  SPIDER(ModRawEntityType.SPIDER, () -> SpiderRawRenderer::new),
  STRAY(ModRawEntityType.STRAY, () -> StrayRawRenderer::new),
  VEX(ModRawEntityType.VEX, () -> VexRawRenderer::new),
  VILLAGER(ModRawEntityType.VILLAGER, () -> VillagerRawRenderer::new),
  VINDICATOR(ModRawEntityType.VINDICATOR, () -> VindicatorRawRenderer::new),
  WITCH(ModRawEntityType.WITCH, () -> WitchRawRenderer::new),
  WITHER_SKELETON(ModRawEntityType.WITHER_SKELETON, () -> WitherSkeletonRawRenderer::new),
  WOLF(ModRawEntityType.WOLF, () -> WolfRawRenderer::new),
  ZOMBIE(ModRawEntityType.ZOMBIE, () -> ZombieRawRenderer::new),
  ZOMBIFIED_PIGLIN(ModRawEntityType.ZOMBIFIED_PIGLIN, () -> ZombifiedPiglinRawRenderer::new),
  ZOMBIE_VILLAGER(ModRawEntityType.ZOMBIE_VILLAGER, () -> ZombieVillagerRawRenderer::new);

  private final ModRawEntityType entityType;
  private final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer;

  ModRawEntityRenderer(
      final ModRawEntityType entityType,
      final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer) {
    this.entityType = entityType;
    this.renderer = renderer;
  }

  public ModRawEntityType getEntityType() {
    return entityType;
  }

  public Function<Context, EntityRenderer<? extends Entity>> getRenderer() {
    return renderer.get();
  }
}
