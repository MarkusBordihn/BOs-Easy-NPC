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
import de.markusbordihn.easynpc.client.renderer.entity.custom.FairyRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.custom.OrcRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.layers.CustomHumanoidArmorLayer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.AllayRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.CatRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ChickenRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidSlimRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IllagerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IronGolemRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PigRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.SkeletonRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.VillagerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieVillagerRenderer;
import de.markusbordihn.easynpc.entity.ModEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.EntityRenderersEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class ClientRenderer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ClientRenderer() {
  }

  public static void registerEntityRenderers(EntityRenderersEvent.RegisterRenderers event) {
    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);

    event.registerEntityRenderer(
        ModEntityType.ALLAY.get(), context -> new AllayRenderer(context, ModModelLayers.ALLAY));
    event.registerEntityRenderer(ModEntityType.CAT.get(), CatRenderer::new);
    event.registerEntityRenderer(ModEntityType.CHICKEN.get(), ChickenRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.DROWNED.get(),
        context -> new ZombieRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.FAIRY.get(), context -> new FairyRenderer(context, ModModelLayers.FAIRY));
    event.registerEntityRenderer(
        ModEntityType.HUMANOID.get(),
        context -> new HumanoidRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.HUMANOID_SLIM.get(),
        context -> new HumanoidSlimRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.STRAY.get(),
        context -> new SkeletonRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.WITHER_SKELETON.get(),
        context -> new SkeletonRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.HUSK.get(),
        context -> new ZombieRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.IRON_GOLEM.get(), IronGolemRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.SKELETON.get(),
        context -> new SkeletonRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.VILLAGER.get(), VillagerRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.ZOMBIE.get(),
        context -> new ZombieRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.ZOMBIE_VILLAGER.get(),
        context -> new ZombieVillagerRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.PIG.get(), PigRenderer::new);
    event.registerEntityRenderer(ModEntityType.EVOKER.get(), IllagerRenderer::new);
    event.registerEntityRenderer(ModEntityType.ILLUSIONER.get(), IllagerRenderer::new);
    event.registerEntityRenderer(ModEntityType.PILLAGER.get(), IllagerRenderer::new);
    event.registerEntityRenderer(ModEntityType.VINDICATOR.get(), IllagerRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.ORC.get(), context -> new OrcRenderer(context, ModModelLayers.ORC));
  }
}
