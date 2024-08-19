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
import de.markusbordihn.easynpc.client.renderer.entity.layers.CustomHumanoidArmorLayer;
import de.markusbordihn.easynpc.client.renderer.entity.raw.ZombieRawRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.AllayModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.CatModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ChickenModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HorseModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.HumanoidSlimModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IllagerModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.IronGolemModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PigModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.PiglinModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.SkeletonModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.VillagerModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.WolfModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieModelRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.standard.ZombieVillagerModelRenderer;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.entity.ModEntityType;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.EntityRenderersEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class ClientRenderer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ClientRenderer() {}

  public static void registerEntityRenderers(EntityRenderersEvent.RegisterRenderers event) {
    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);

    event.registerEntityRenderer(
        ModEntityType.ALLAY.get(),
        context -> new AllayModelRenderer(context, ModModelLayers.ALLAY));
    event.registerEntityRenderer(ModEntityType.CAT.get(), CatModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.CHICKEN.get(), ChickenModelRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.BOGGED.get(),
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.DROWNED.get(),
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.FAIRY.get(),
        context -> new FairyModelRenderer(context, ModModelLayers.FAIRY));
    event.registerEntityRenderer(
        ModEntityType.HUMANOID.get(),
        context -> new HumanoidModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.HUMANOID_SLIM.get(),
        context -> new HumanoidSlimModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.STRAY.get(),
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.WITHER_SKELETON.get(),
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.HUSK.get(),
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.IRON_GOLEM.get(), IronGolemModelRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.SKELETON.get(),
        context -> new SkeletonModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.VILLAGER.get(), VillagerModelRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.ZOMBIE.get(),
        context -> new ZombieModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.ZOMBIE_VILLAGER.get(),
        context -> new ZombieVillagerModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.PIG.get(), PigModelRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.PIGLIN.get(),
        context -> new PiglinModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.PIGLIN_BRUTE.get(),
        context -> new PiglinModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(
        ModEntityType.PIGLIN_ZOMBIFIED.get(),
        context -> new PiglinModelRenderer(context, CustomHumanoidArmorLayer.class));
    event.registerEntityRenderer(ModEntityType.EVOKER.get(), IllagerModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.ILLUSIONER.get(), IllagerModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.PILLAGER.get(), IllagerModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.VINDICATOR.get(), IllagerModelRenderer::new);
    event.registerEntityRenderer(
        ModEntityType.ORC.get(), context -> new OrcModelRenderer(context, ModModelLayers.ORC));
    event.registerEntityRenderer(
        ModEntityType.ORC_WARRIOR.get(),
        context -> new OrcModelRenderer(context, ModModelLayers.ORC));
    event.registerEntityRenderer(ModEntityType.WOLF.get(), WolfModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.HORSE.get(), HorseModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.SKELETON_HORSE.get(), HorseModelRenderer::new);
    event.registerEntityRenderer(ModEntityType.ZOMBIE_HORSE.get(), HorseModelRenderer::new);

    // Raw entities (for modding only)
    event.registerEntityRenderer(ModEntityType.ZOMBIE_RAW.get(), ZombieRawRenderer::new);

    // Optional: Epic Fight entities
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      event.registerEntityRenderer(ModEntityType.EPIC_FIGHT_ZOMBIE.get(), ZombieRawRenderer::new);
    }
  }
}
