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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.compat.cobblemon.CobblemonSpeciesManager;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class RenderHandler {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private RenderHandler() {}

  public static boolean setRenderType(EasyNPC<?> easyNPC, RenderType renderType) {
    if (easyNPC == null || renderType == null) {
      log.error("[{}] Error setting render type to {}", easyNPC, renderType);
      return false;
    }

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null) {
      log.error("[{}] No render data available for setting render type {}!", easyNPC, renderType);
      return false;
    }

    log.debug("[{}] Setting render type to {}", easyNPC, renderType);
    renderData.setRenderData(renderData.getRenderDataEntry().withRenderType(renderType));
    return true;
  }

  public static boolean setRenderEntity(
      EasyNPC<?> easyNPC, EntityType<? extends Entity> entityType) {
    if (easyNPC == null || entityType == null) {
      log.error("[{}] Error setting render entity to {}", easyNPC, entityType);
      return false;
    }

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null || renderData.getRenderDataEntry() == null) {
      log.error("[{}] No render data available for setting render entity {}!", easyNPC, entityType);
      return false;
    }

    log.debug("[{}] Setting render entity to {}", easyNPC, entityType);
    renderData.setRenderData(renderData.getRenderDataEntry().withRenderEntityType(entityType));
    return true;
  }

  public static boolean setRenderEntityModel(EasyNPC<?> easyNPC, String entityModel) {
    if (easyNPC == null || entityModel == null || entityModel.isEmpty()) {
      log.error("[{}] Error setting render entity model to {}", easyNPC, entityModel);
      return false;
    }

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData == null || renderData.getRenderDataEntry() == null) {
      log.error(
          "[{}] No render data available for setting render entity model {}!",
          easyNPC,
          entityModel);
      return false;
    }

    // Determine the integration from the NPC type (not the mutable render type) so the correct
    // model list is validated and the correct render type is applied.
    boolean easyModelNPC =
        easyNPC instanceof ConfigurationDataCapable<?> configurable
            && configurable.getConfigurationData() == ConfigurationData.EASY_MODEL;
    String integrationId =
        easyModelNPC
            ? EasyModelEntitiesManager.INTEGRATION_ID
            : CobblemonSpeciesManager.INTEGRATION_ID;
    RenderType renderType =
        easyModelNPC ? RenderType.EASY_MODEL_ENTITY : RenderType.COBBLEMON_ENTITY;

    if (!IntegrationRegistry.hasModels(integrationId)) {
      log.warn(
          "[{}] Model list for integration '{}' not loaded yet, accepting {} without validation.",
          easyNPC,
          integrationId,
          entityModel);
    } else {
      ResourceLocation modelId = ResourceLocation.tryParse(entityModel);
      if (modelId == null || !IntegrationRegistry.getModels(integrationId).contains(modelId)) {
        log.error(
            "[{}] Unknown model '{}' for integration '{}', rejecting.",
            easyNPC,
            entityModel,
            integrationId);
        return false;
      }
    }

    log.debug("[{}] Setting render entity model to {}", easyNPC, entityModel);
    renderData.setRenderData(new RenderDataEntry(renderType, null, entityModel));
    return true;
  }
}
