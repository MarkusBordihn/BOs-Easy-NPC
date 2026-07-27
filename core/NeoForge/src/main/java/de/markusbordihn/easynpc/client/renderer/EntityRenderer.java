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
import de.markusbordihn.easynpc.client.renderer.entity.ModCustomEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ModEpicFightEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ModNPCEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.ModRawEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.cobblemon.CobblemonNPCRenderer;
import de.markusbordihn.easynpc.client.renderer.entity.easymodelentities.EasyModelNPCRenderer;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.entity.CobblemonEntityType;
import de.markusbordihn.easynpc.entity.EasyModelEntitiesEntityType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.neoforge.client.event.EntityRenderersEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class EntityRenderer {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EntityRenderer() {}

  public static void register(EntityRenderersEvent.RegisterRenderers event) {
    log.info("{} Entity Renders ...", Constants.LOG_REGISTER_PREFIX);

    // Raw entities (for modding only)
    for (ModRawEntityRenderer renderer : ModRawEntityRenderer.values()) {
      event.registerEntityRenderer(
          ModEntityType.getEntityType(renderer.getEntityType()),
          context -> renderer.getRenderer().apply(context));
    }

    // Pre-defined NPCs
    for (ModNPCEntityRenderer renderer : ModNPCEntityRenderer.values()) {
      event.registerEntityRenderer(
          ModEntityType.getEntityType(renderer.getEntityType()),
          context -> renderer.getRenderer().apply(context));
    }

    // Custom NPCs
    for (ModCustomEntityRenderer renderer : ModCustomEntityRenderer.values()) {
      event.registerEntityRenderer(
          ModEntityType.getEntityType(renderer.getEntityType()),
          context -> renderer.getRenderer().apply(context));
    }

    // Epic Fight NPCs
    if (CompatConstants.MOD_EPIC_FIGHT_LOADED) {
      for (ModEpicFightEntityRenderer renderer : ModEpicFightEntityRenderer.values()) {
        event.registerEntityRenderer(
            ModEntityType.getEntityType(renderer.getEntityType()),
            context -> renderer.getRenderer().apply(context));
      }
    }

    if (CompatConstants.MOD_COBBLEMON_LOADED) {
      event.registerEntityRenderer(
          ModEntityType.getEntityType(CobblemonEntityType.COBBLEMON_NPC),
          context -> new CobblemonNPCRenderer<>(context, ModModelLayers.DOPPLER));
    }

    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      event.registerEntityRenderer(
          ModEntityType.getEntityType(EasyModelEntitiesEntityType.EASY_MODEL_NPC),
          context -> new EasyModelNPCRenderer<>(context, ModModelLayers.DOPPLER));
    }
  }
}
