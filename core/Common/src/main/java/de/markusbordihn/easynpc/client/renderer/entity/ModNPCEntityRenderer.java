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
import de.markusbordihn.easynpc.client.renderer.entity.standard.PlayerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PlayerSlimRenderer;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import java.util.function.Function;
import java.util.function.Supplier;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider.Context;
import net.minecraft.world.entity.Entity;

public enum ModNPCEntityRenderer {
  ALLAY(ModNPCEntityType.ALLAY, () -> AllayRawRenderer::new),
  BOGGED(ModNPCEntityType.BOGGED, () -> BoggedRawRenderer::new),
  CAT(ModNPCEntityType.CAT, () -> CatRawRenderer::new),
  CHICKEN(ModNPCEntityType.CHICKEN, () -> ChickenRawRenderer::new),
  CREEPER(ModNPCEntityType.CREEPER, () -> CreeperRawRenderer::new),
  DROWNED(ModNPCEntityType.DROWNED, () -> DrownedRawRenderer::new),
  ENDERMAN(ModNPCEntityType.ENDERMAN, () -> EnderManRawRenderer::new),
  EVOKER(ModNPCEntityType.EVOKER, () -> EvokerRawRenderer::new),
  FOX(ModNPCEntityType.FOX, () -> FoxRawRenderer::new),
  HORSE_SKELETON(ModNPCEntityType.HORSE_SKELETON, () -> HorseRawRenderer::new),
  HORSE_ZOMBIE(ModNPCEntityType.HORSE_ZOMBIE, () -> HorseRawRenderer::new),
  HORSE(ModNPCEntityType.HORSE, () -> HorseRawRenderer::new),
  HUMANOID_SLIM(ModNPCEntityType.HUMANOID_SLIM, () -> PlayerSlimRenderer::new),
  HUMANOID(ModNPCEntityType.HUMANOID, () -> PlayerRenderer::new),
  ILLUSIONER(ModNPCEntityType.ILLUSIONER, () -> IllusionerRawRenderer::new),
  IRON_GOLEM(ModNPCEntityType.IRON_GOLEM, () -> IronGolemRawRenderer::new),
  PIG(ModNPCEntityType.PIG, () -> PigRawRenderer::new),
  PIGLIN_BRUTE(ModNPCEntityType.PIGLIN_BRUTE, () -> PiglinBruteRawRenderer::new),
  PIGLIN_ZOMBIFIED(ModNPCEntityType.PIGLIN_ZOMBIFIED, () -> ZombifiedPiglinRawRenderer::new),
  PIGLIN(ModNPCEntityType.PIGLIN, () -> PiglinRawRenderer::new),
  PILLAGER(ModNPCEntityType.PILLAGER, () -> PillagerRawRenderer::new),
  SKELETON(ModNPCEntityType.SKELETON, () -> SkeletonRawRenderer::new),
  CAVE_SPIDER(ModNPCEntityType.CAVE_SPIDER, () -> SpiderRawRenderer::new),
  SPIDER(ModNPCEntityType.SPIDER, () -> SpiderRawRenderer::new),
  STRAY(ModNPCEntityType.STRAY, () -> StrayRawRenderer::new),
  VEX(ModNPCEntityType.VEX, () -> VexRawRenderer::new),
  VILLAGER(ModNPCEntityType.VILLAGER, () -> VillagerRawRenderer::new),
  VINDICATOR(ModNPCEntityType.VINDICATOR, () -> VindicatorRawRenderer::new),
  WITCH(ModNPCEntityType.WITCH, () -> WitchRawRenderer::new),
  WITHER_SKELETON(ModNPCEntityType.WITHER_SKELETON, () -> WitherSkeletonRawRenderer::new),
  WOLF(ModNPCEntityType.WOLF, () -> WolfRawRenderer::new),
  ZOMBIE_HUSK(ModNPCEntityType.ZOMBIE_HUSK, () -> HuskRawRenderer::new),
  ZOMBIE_VILLAGER(ModNPCEntityType.ZOMBIE_VILLAGER, () -> ZombieVillagerRawRenderer::new),
  ZOMBIE(ModNPCEntityType.ZOMBIE, () -> ZombieRawRenderer::new);

  private final ModNPCEntityType entityType;
  private final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer;

  ModNPCEntityRenderer(
      final ModNPCEntityType entityType,
      final Supplier<Function<Context, EntityRenderer<? extends Entity>>> renderer) {
    this.entityType = entityType;
    this.renderer = renderer;
  }

  public ModNPCEntityType getEntityType() {
    return entityType;
  }

  public Function<Context, EntityRenderer<? extends Entity>> getRenderer() {
    return renderer.get();
  }
}
