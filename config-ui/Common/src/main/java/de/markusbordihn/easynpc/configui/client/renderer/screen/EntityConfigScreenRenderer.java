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

package de.markusbordihn.easynpc.configui.client.renderer.screen;

import de.markusbordihn.easynpc.client.renderer.screen.EntityScreenRenderer;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.profession.Profession;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelAnimationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.client.gui.GuiGraphics;

public class EntityConfigScreenRenderer extends EntityScreenRenderer {

  private EntityConfigScreenRenderer() {}

  public static void renderEntity(
      GuiGraphics guiGraphics,
      EasyNPC<?> easyNPC,
      EntityRenderConfig config,
      float mouseX,
      float mouseY) {
    if (easyNPC == null || easyNPC.getLivingEntity() == null) {
      return;
    }

    ConfigRenderState backupState = new ConfigRenderState(easyNPC);
    applyConfigOverrides(easyNPC, config);

    EntityScreenRenderer.renderEntity(guiGraphics, easyNPC, config, mouseX, mouseY);

    restoreConfigState(easyNPC, backupState);
  }

  private static void applyConfigOverrides(EasyNPC<?> easyNPC, EntityRenderConfig config) {
    EntityRenderOverrides overrides = config.overrides();

    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData != null && renderData.getRenderDataEntry() != null) {
      RenderDataEntry renderDataSet = renderData.getRenderDataEntry();
      if (overrides.renderType() != null) {
        renderDataSet = renderDataSet.withRenderType(overrides.renderType());
      }
      if (overrides.renderEntityType() != null) {
        renderDataSet = renderDataSet.withRenderEntityType(overrides.renderEntityType());
      }
      renderData.setRenderData(renderDataSet);
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null && (overrides.skinType() != null || overrides.skinUUID() != null)) {
      SkinDataEntry skinDataEntry = skinData.getSkinDataEntry();
      if (overrides.skinType() != null) {
        skinDataEntry = skinDataEntry.withType(overrides.skinType());
      }
      if (overrides.skinUUID() != null) {
        skinDataEntry = skinDataEntry.withUUID(overrides.skinUUID());
      }
      skinData.setSkinDataEntry(skinDataEntry);
    }

    VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
    if (variantData != null && overrides.variant() != null) {
      variantData.setSkinVariantType(overrides.variant());
    }

    ProfessionDataCapable<?> professionData = easyNPC.getEasyNPCProfessionData();
    if (professionData != null && overrides.profession() != null) {
      professionData.setProfession(overrides.profession());
    }
  }

  private static void restoreConfigState(EasyNPC<?> easyNPC, ConfigRenderState backupState) {
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData != null && backupState.renderDataEntry != null) {
      renderData.setRenderData(backupState.renderDataEntry);
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null && backupState.skinDataEntry != null) {
      skinData.setSkinDataEntry(backupState.skinDataEntry);
    }

    VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
    if (variantData != null && backupState.variantType != null) {
      variantData.setSkinVariantType(backupState.variantType);
    }

    ProfessionDataCapable<?> professionData = easyNPC.getEasyNPCProfessionData();
    if (professionData != null && backupState.profession != null) {
      professionData.setProfession(backupState.profession);
    }

    ModelAnimationDataCapable<?> animationData = easyNPC.getEasyNPCModelData();
    if (animationData != null && backupState.modelAnimationData != null) {
      animationData.setModelAnimationData(backupState.modelAnimationData);
    }
  }

  private static class ConfigRenderState {
    final SkinDataEntry skinDataEntry;
    final Enum<?> variantType;
    final Profession profession;
    final RenderDataEntry renderDataEntry;
    final ModelAnimationData modelAnimationData;

    ConfigRenderState(EasyNPC<?> easyNPC) {
      RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
      this.renderDataEntry = renderData != null ? renderData.getRenderDataEntry() : null;
      SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
      this.skinDataEntry = skinData != null ? skinData.getSkinDataEntry() : null;
      VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
      this.variantType = variantData != null ? variantData.getSkinVariantType() : null;
      ProfessionDataCapable<?> professionData = easyNPC.getEasyNPCProfessionData();
      this.profession = professionData != null ? professionData.getProfession() : null;
      ModelAnimationDataCapable<?> animationData = easyNPC.getEasyNPCModelData();
      this.modelAnimationData =
          animationData != null ? animationData.getModelAnimationData() : null;
    }
  }
}
