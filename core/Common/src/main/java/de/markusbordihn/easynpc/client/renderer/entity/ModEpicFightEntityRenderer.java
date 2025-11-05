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

import de.markusbordihn.easynpc.client.renderer.entity.raw.CreeperRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.DrownedRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.EnderManRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.EvokerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.HuskRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.IronGolemRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PiglinBruteRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PiglinRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.SkeletonRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.SpiderRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VexRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VindicatorRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.WitchRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieVillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombifiedPiglinRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PlayerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PlayerSlimRenderer;
import de.markusbordihn.easynpc.entity.EpicFightEntityType;
import java.util.function.Function;
import java.util.function.Supplier;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider.Context;
import net.minecraft.world.entity.Entity;

public enum ModEpicFightEntityRenderer {
  CAVE_SPIDER(EpicFightEntityType.CAVE_SPIDER, () -> SpiderRawRenderer::new),
  CREEPER(EpicFightEntityType.CREEPER, () -> CreeperRawRenderer::new),
  DROWNED(EpicFightEntityType.DROWNED, () -> DrownedRawRenderer::new),
  ENDERMAN(EpicFightEntityType.ENDERMAN, () -> EnderManRawRenderer::new),
  EVOKER(EpicFightEntityType.EVOKER, () -> EvokerRawRenderer::new),
  HUSK(EpicFightEntityType.HUSK, () -> HuskRawRenderer::new),
  HUMANOID_SLIM(EpicFightEntityType.HUMANOID_SLIM, () -> PlayerSlimRenderer::new),
  HUMANOID(EpicFightEntityType.HUMANOID, () -> PlayerRenderer::new),
  IRON_GOLEM(EpicFightEntityType.IRON_GOLEM, () -> IronGolemRawRenderer::new),
  PIGLIN_BRUTE(EpicFightEntityType.PIGLIN_BRUTE, () -> PiglinBruteRawRenderer::new),
  PIGLIN(EpicFightEntityType.PIGLIN, () -> PiglinRawRenderer::new),
  PILLAGER(EpicFightEntityType.PILLAGER, () -> PillagerRawRenderer::new),
  SKELETON(EpicFightEntityType.SKELETON, () -> SkeletonRawRenderer::new),
  SPIDER(EpicFightEntityType.SPIDER, () -> SpiderRawRenderer::new),
  STRAY(EpicFightEntityType.STRAY, () -> SkeletonRawRenderer::new),
  VEX(EpicFightEntityType.VEX, () -> VexRawRenderer::new),
  VINDICATOR(EpicFightEntityType.VINDICATOR, () -> VindicatorRawRenderer::new),
  WITCH(EpicFightEntityType.WITCH, () -> WitchRawRenderer::new),
  WITHER_SKELETON(EpicFightEntityType.WITHER_SKELETON, () -> SkeletonRawRenderer::new),
  ZOMBIE_VILLAGER(EpicFightEntityType.ZOMBIE_VILLAGER, () -> ZombieVillagerRawRenderer::new),
  ZOMBIE(EpicFightEntityType.ZOMBIE, () -> ZombieRawRenderer::new),
  ZOMBIFIED_PIGLIN(EpicFightEntityType.ZOMBIFIED_PIGLIN, () -> ZombifiedPiglinRawRenderer::new);

  private final EpicFightEntityType entityType;
  private final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer;

  ModEpicFightEntityRenderer(
      final EpicFightEntityType entityType,
      final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer) {
    this.entityType = entityType;
    this.renderer = renderer;
  }

  public EpicFightEntityType getEntityType() {
    return entityType;
  }

  public Function<Context, EntityRenderer<? extends Entity>> getRenderer() {
    return renderer.get();
  }
}
