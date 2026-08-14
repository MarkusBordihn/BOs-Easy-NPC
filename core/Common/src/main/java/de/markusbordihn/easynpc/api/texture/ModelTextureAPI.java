/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.api.texture;

import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.render.ModelTextureBlend;
import de.markusbordihn.easynpc.data.render.ModelTextureSetting;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.easymodelentities.EasyModelNPC;
import java.util.List;
import java.util.function.UnaryOperator;
import net.minecraft.resources.ResourceLocation;

public final class ModelTextureAPI {

  private ModelTextureAPI() {}

  public static boolean supportsTextures(EasyNPC<?> npc) {
    return CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED && npc instanceof EasyModelNPC;
  }

  public static ModelTextureSetting getTextureSetting(EasyNPC<?> npc) {
    RenderDataEntry renderDataEntry = getRenderDataEntry(npc);
    return renderDataEntry != null
        ? renderDataEntry.getRenderTextureSetting()
        : ModelTextureSetting.EMPTY;
  }

  public static boolean setTexture(EasyNPC<?> npc, String slot, ResourceLocation texture) {
    return updateTextureSetting(npc, setting -> setting.withSlot(slot, texture));
  }

  public static boolean setTexture(
      EasyNPC<?> npc, String slot, ResourceLocation texture, ModelTextureBlend blend) {
    return updateTextureSetting(npc, setting -> setting.withSlot(slot, texture, blend));
  }

  public static boolean setBlend(EasyNPC<?> npc, String slot, ModelTextureBlend blend) {
    return updateTextureSetting(npc, setting -> setting.withBlend(slot, blend));
  }

  public static boolean clearTexture(EasyNPC<?> npc, String slot) {
    return updateTextureSetting(npc, setting -> setting.withoutSlot(slot));
  }

  public static boolean clearTextures(EasyNPC<?> npc) {
    return updateTextureSetting(npc, ModelTextureSetting::withoutSlots);
  }

  public static List<String> listTextureSlots(EasyNPC<?> npc) {
    if (!(npc instanceof EasyModelNPC easyModelNPC) || !supportsTextures(npc)) {
      return List.of();
    }
    return EasyModelEntitiesManager.listTextureSlots(easyModelNPC.getEasyModelProfileId());
  }

  public static List<ResourceLocation> listTextureVariants(EasyNPC<?> npc, String slot) {
    if (!(npc instanceof EasyModelNPC easyModelNPC) || !supportsTextures(npc)) {
      return List.of();
    }
    return EasyModelEntitiesManager.listTextureVariants(easyModelNPC.getEasyModelProfileId(), slot);
  }

  private static boolean updateTextureSetting(
      EasyNPC<?> npc, UnaryOperator<ModelTextureSetting> update) {
    if (!supportsTextures(npc)) {
      return false;
    }

    RenderDataCapable<?> renderData = npc.getEasyNPCRenderData();
    RenderDataEntry renderDataEntry = renderData != null ? renderData.getRenderDataEntry() : null;
    if (renderDataEntry == null) {
      return false;
    }

    ModelTextureSetting updated = update.apply(renderDataEntry.getRenderTextureSetting());
    if (updated.equals(renderDataEntry.getRenderTextureSetting())) {
      return false;
    }

    renderData.setRenderData(renderDataEntry.withRenderTextureSetting(updated));
    return true;
  }

  private static RenderDataEntry getRenderDataEntry(EasyNPC<?> npc) {
    RenderDataCapable<?> renderData = npc != null ? npc.getEasyNPCRenderData() : null;
    return renderData != null ? renderData.getRenderDataEntry() : null;
  }
}
