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

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import java.util.function.Function;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.SkeletonRenderer;
import net.minecraft.client.renderer.entity.VillagerRenderer;
import net.minecraft.client.renderer.entity.ZombieRenderer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityRendererUtils {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final Map<
          EntityType<?>,
          Function<EntityRendererProvider.Context, EntityRenderer<? extends LivingEntity>>>
      VANILLA_RENDERER_FACTORIES =
          Map.of(
              EntityType.VILLAGER,
              VillagerRenderer::new,
              EntityType.ZOMBIE,
              ZombieRenderer::new,
              EntityType.SKELETON,
              SkeletonRenderer::new);

  private EntityRendererUtils() {}

  public static Function<EntityRendererProvider.Context, EntityRenderer<? extends LivingEntity>>
      getVanillaRendererFactory(EntityType<?> entityType) {
    return VANILLA_RENDERER_FACTORIES.get(entityType);
  }

  public static EntityRenderer<? extends LivingEntity> createFallbackRenderer(
      EntityRendererProvider.Context context, EntityType<?> entityType) {
    log.warn(
        "No specific renderer found for entity type {}, using villager renderer as fallback",
        entityType);
    return new VillagerRenderer(context);
  }
}
