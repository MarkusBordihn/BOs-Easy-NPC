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
import de.markusbordihn.easynpc.client.renderer.entity.custom.FairyModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.custom.OrcModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.AllayModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.CatModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ChickenModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HorseModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidSlimModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IllagerModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IronGolemModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PigModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.SkeletonModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.VillagerModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.WolfModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieVillagerModelRenderer;
import de.markusbordihn.easynpc.client.renderer.layers.CustomHumanoidArmorLayer;
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
        ModEntityType.ALLAY, context -> new AllayModelRenderer(context, ModModelLayers.ALLAY));
    EntityRendererRegistry.register(ModEntityType.CAT, CatModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.CHICKEN, ChickenModelRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.BOGGED,
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.DROWNED,
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.FAIRY, context -> new FairyModelRenderer(context, ModModelLayers.FAIRY));
    EntityRendererRegistry.register(
        ModEntityType.HUMANOID,
        context -> new HumanoidModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.HUMANOID_SLIM,
        context -> new HumanoidSlimModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.STRAY,
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.WITHER_SKELETON,
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.HUSK,
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(ModEntityType.IRON_GOLEM, IronGolemModelRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.SKELETON,
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(ModEntityType.VILLAGER, VillagerModelRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.ZOMBIE,
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(
        ModEntityType.ZOMBIE_VILLAGER,
        context -> new ZombieVillagerModelRenderer(context, CustomHumanoidArmorLayer.class));
    EntityRendererRegistry.register(ModEntityType.PIG, PigModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.EVOKER, IllagerModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.ILLUSIONER, IllagerModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.PILLAGER, IllagerModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.VINDICATOR, IllagerModelRenderer::new);
    EntityRendererRegistry.register(
        ModEntityType.ORC, context -> new OrcModelRenderer(context, ModModelLayers.ORC));
    EntityRendererRegistry.register(
        ModEntityType.ORC_WARRIOR, context -> new OrcModelRenderer(context, ModModelLayers.ORC));
    EntityRendererRegistry.register(ModEntityType.WOLF, WolfModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.HORSE, HorseModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.SKELETON_HORSE, HorseModelRenderer::new);
    EntityRendererRegistry.register(ModEntityType.ZOMBIE_HORSE, HorseModelRenderer::new);
  }
}
