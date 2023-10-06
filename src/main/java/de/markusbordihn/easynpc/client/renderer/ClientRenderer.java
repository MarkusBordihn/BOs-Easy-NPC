/**
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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.EntityRenderersEvent;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.entity.CatRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ChickenRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.FairyRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.HumanoidRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.HumanoidSlimRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.SkeletonRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.VillagerRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ZombieRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ZombieVillagerRenderer;
import de.markusbordihn.easynpc.entity.npc.ModEntityType;

@OnlyIn(Dist.CLIENT)
public class ClientRenderer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ClientRenderer() {}

  public static void registerEntityRenderers(EntityRenderersEvent.RegisterRenderers event) {
    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);

    // Default NPC Entity
    event.registerEntityRenderer(ModEntityType.CAT.get(), CatRenderer::new);
    event.registerEntityRenderer(ModEntityType.CHICKEN.get(), ChickenRenderer::new);
    event.registerEntityRenderer(ModEntityType.FAIRY.get(), FairyRenderer::new);
    event.registerEntityRenderer(ModEntityType.HUMANOID.get(), HumanoidRenderer::new);
    event.registerEntityRenderer(ModEntityType.HUMANOID_SLIM.get(), HumanoidSlimRenderer::new);
    event.registerEntityRenderer(ModEntityType.SKELETON.get(), SkeletonRenderer::new);
    event.registerEntityRenderer(ModEntityType.VILLAGER.get(), VillagerRenderer::new);
    event.registerEntityRenderer(ModEntityType.ZOMBIE.get(), ZombieRenderer::new);
    event.registerEntityRenderer(ModEntityType.ZOMBIE_VILLAGER.get(), ZombieVillagerRenderer::new);
  }

}
