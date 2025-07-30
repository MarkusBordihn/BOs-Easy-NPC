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
import de.markusbordihn.easynpc.client.renderer.entity.raw.CatRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ChickenRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.DrownedRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.EvokerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.FoxRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.HorseRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.IllusionerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.IronGolemRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PigRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PiglinRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.PillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.SkeletonRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VillagerRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.VindicatorRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.WolfRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieVillagerRawRenderer;
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
  CAT(ModNPCEntityType.CAT, () -> CatRawRenderer::new),
  CHICKEN(ModNPCEntityType.CHICKEN, () -> ChickenRawRenderer::new),
  DROWNED(ModNPCEntityType.DROWNED, () -> DrownedRawRenderer::new),
  EVOKER(ModNPCEntityType.EVOKER, () -> EvokerRawRenderer::new),
  FOX(ModNPCEntityType.FOX, () -> FoxRawRenderer::new),
  HORSE(ModNPCEntityType.HORSE, () -> HorseRawRenderer::new),
  HORSE_SKELETON(ModNPCEntityType.HORSE_SKELETON, () -> HorseRawRenderer::new),
  HORSE_ZOMBIE(ModNPCEntityType.HORSE_ZOMBIE, () -> HorseRawRenderer::new),
  HUMANOID(ModNPCEntityType.HUMANOID, () -> PlayerRenderer::new),
  HUMANOID_SLIM(ModNPCEntityType.HUMANOID_SLIM, () -> PlayerSlimRenderer::new),
  ILLUSIONER(ModNPCEntityType.ILLUSIONER, () -> IllusionerRawRenderer::new),
  IRON_GOLEM(ModNPCEntityType.IRON_GOLEM, () -> IronGolemRawRenderer::new),
  PIGLIN(ModNPCEntityType.PIGLIN, () -> PiglinRawRenderer::new),
  PIGLIN_BRUTE(ModNPCEntityType.PIGLIN_BRUTE, () -> PiglinRawRenderer::new),
  PIGLIN_ZOMBIFIED(ModNPCEntityType.PIGLIN_ZOMBIFIED, () -> PiglinRawRenderer::new),
  PIG(ModNPCEntityType.PIG, () -> PigRawRenderer::new),
  PILLAGER(ModNPCEntityType.PILLAGER, () -> PillagerRawRenderer::new),
  SKELETON(ModNPCEntityType.SKELETON, () -> SkeletonRawRenderer::new),
  SKELETON_BOGGED(ModNPCEntityType.SKELETON_BOGGED, () -> SkeletonRawRenderer::new),
  SKELETON_STRAY(ModNPCEntityType.SKELETON_STRAY, () -> SkeletonRawRenderer::new),
  SKELETON_WITHER(ModNPCEntityType.SKELETON_WITHER, () -> SkeletonRawRenderer::new),
  VILLAGER(ModNPCEntityType.VILLAGER, () -> VillagerRawRenderer::new),
  VINDICATOR(ModNPCEntityType.VINDICATOR, () -> VindicatorRawRenderer::new),
  WOLF(ModNPCEntityType.WOLF, () -> WolfRawRenderer::new),
  ZOMBIE(ModNPCEntityType.ZOMBIE, () -> ZombieRawRenderer::new),
  ZOMBIE_HUSK(ModNPCEntityType.ZOMBIE_HUSK, () -> ZombieRawRenderer::new),
  ZOMBIE_VILLAGER(ModNPCEntityType.ZOMBIE_VILLAGER, () -> ZombieVillagerRawRenderer::new);

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
