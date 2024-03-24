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

package de.markusbordihn.easynpc.client.renderer;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.client.renderer.entity.AllayRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.HumanoidRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.custom.FairyRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.AllayRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.CatRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ChickenRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidSlimRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IronGolemRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.SkeletonRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.VillagerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieVillagerRenderer;
import de.markusbordihn.easynpc.entity.ModEntityType;
import net.fabricmc.fabric.api.client.rendering.v1.EntityRendererRegistry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientRenderer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ClientRenderer() {}

  public static void registerEntityRenderers() {
    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);

    EntityRendererRegistry.register(
        ModEntityType.ALLAY, context -> new AllayRenderer(context, ModModelLayers.ALLAY));
    EntityRendererRegistry.register(ModEntityType.CAT, CatRenderer::new);
    EntityRendererRegistry.register(ModEntityType.CHICKEN, ChickenRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.DROWNED, context -> new ZombieRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.FAIRY, context -> new FairyRenderer(context, ModModelLayers.FAIRY));
    EntityRendererRegistry.register(
        ModEntityType.HUMANOID, context -> new HumanoidRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.HUMANOID_SLIM, context -> new HumanoidSlimRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.STRAY, context -> new SkeletonRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.WITHER_SKELETON, context -> new SkeletonRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.HUSK, context -> new ZombieRenderer(context, null));
    EntityRendererRegistry.register(ModEntityType.IRON_GOLEM, IronGolemRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.SKELETON, context -> new SkeletonRenderer(context, null));
    EntityRendererRegistry.register(ModEntityType.VILLAGER, VillagerRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.ZOMBIE, context -> new ZombieRenderer(context, null));
    EntityRendererRegistry.register(
        ModEntityType.ZOMBIE_VILLAGER, context -> new ZombieVillagerRenderer(context, null));
  }
}
