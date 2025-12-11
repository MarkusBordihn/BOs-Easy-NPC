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

package de.markusbordihn.easynpc.client.renderer.screen;

import de.markusbordihn.easynpc.client.gui.InventoryScreenHandler;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public class EntityScreenRenderer {

  protected EntityScreenRenderer() {}

  public static void renderEntity(
      GuiGraphics guiGraphics,
      EasyNPC<?> easyNPC,
      EntityRenderConfig config,
      float mouseX,
      float mouseY) {
    if (easyNPC == null || easyNPC.getLivingEntity() == null) {
      return;
    }

    LivingEntity livingEntity = easyNPC.getLivingEntity();

    EntityRenderState backupState = new EntityRenderState(livingEntity, easyNPC);
    applyRenderModifications(easyNPC, config);

    int renderScale = config.scale();
    if (config.scissorBox() != null) {
      float multiplier = config.scissorBox().scaleMultiplier();
      renderScale = (int) (config.scale() * multiplier);
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null) {
        CustomScale originalScale = modelData.getModelPartScale(ModelPartType.ROOT);
        if (originalScale != null) {
          float adjustment = 1.0f / multiplier;
          modelData.setModelPartScale(
              ModelPartType.ROOT,
              new CustomScale(
                  originalScale.x() * adjustment,
                  originalScale.y() * adjustment,
                  originalScale.z() * adjustment));
        }
      }
    }

    int left, right, top, bottom;
    if (config.scissorBox() != null && config.scissorBox().hasCustomScissor()) {
      left = config.x() + config.scissorBox().left();
      right = left + config.scissorBox().width();
      top = config.y() + config.scissorBox().top();
      bottom = top + config.scissorBox().height();
    } else {
      int entityWidth = (int) (renderScale * 2.5f);
      int entityHeight = (int) (renderScale * 3.0f);
      left = config.x() - entityWidth / 2;
      right = config.x() + entityWidth / 2;
      top = config.y() - entityHeight;
      bottom = config.y() + (int) (renderScale * 0.5f);
    }

    try {
      RendererManager.setScreenRendering(true);
      InventoryScreen.renderEntityInInventoryFollowsMouse(
          guiGraphics, left, top, right, bottom, renderScale, 0.0f, mouseX, mouseY, livingEntity);
    } finally {
      RendererManager.setScreenRendering(false);
    }

    restoreEntityState(easyNPC, backupState);
  }

  public static void renderEntityRaw(
      GuiGraphics guiGraphics,
      EasyNPC<?> easyNPC,
      EntityRenderConfig config,
      float mouseX,
      float mouseY) {
    if (easyNPC == null || easyNPC.getLivingEntity() == null) {
      return;
    }

    try {
      InventoryScreenHandler.setBypassMixin(true);
      renderEntity(guiGraphics, easyNPC, config, mouseX, mouseY);
    } finally {
      InventoryScreenHandler.setBypassMixin(false);
    }
  }

  protected static void applyRenderModifications(EasyNPC<?> easyNPC, EntityRenderConfig config) {
    Entity entity = easyNPC.getEntity();
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    EntityRenderOverrides overrides = config.overrides();

    if (overrides.invisible() != null) {
      entity.setInvisible(overrides.invisible());
    }

    if (overrides.hideNameTag() != null && overrides.hideNameTag()) {
      Minecraft minecraft = Minecraft.getInstance();
      if (minecraft != null) {
        minecraft.options.hideGui = true;
      } else {
        livingEntity.setCustomName(null);
        livingEntity.setCustomNameVisible(false);
      }
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null) {
      if (overrides.rootRotation() != null) {
        modelData.setModelPartRotation(ModelPartType.ROOT, overrides.rootRotation());
      }
      if (overrides.rootScale() != null) {
        modelData.setModelPartScale(ModelPartType.ROOT, overrides.rootScale());
      }
      if (overrides.modelPose() != null) {
        modelData.setModelPose(overrides.modelPose());
      }
    }

    if (overrides.entityPose() != null) {
      entity.setPose(overrides.entityPose());
    }
  }

  protected static void restoreEntityState(EasyNPC<?> easyNPC, EntityRenderState backupState) {
    Entity entity = easyNPC.getEntity();
    LivingEntity livingEntity = easyNPC.getLivingEntity();

    livingEntity.setCustomName(backupState.customName);
    livingEntity.setCustomNameVisible(backupState.shouldShowName);

    Minecraft minecraft = Minecraft.getInstance();
    if (minecraft != null) {
      minecraft.options.hideGui = backupState.minecraftHideGui;
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null) {
      if (backupState.rootRotation != null) {
        modelData.setModelPartRotation(ModelPartType.ROOT, backupState.rootRotation);
      }
      if (backupState.rootScale != null) {
        modelData.setModelPartScale(ModelPartType.ROOT, backupState.rootScale);
      }
      if (backupState.modelPose != null) {
        modelData.setModelPose(backupState.modelPose);
      }
    }

    if (backupState.entityPose != null) {
      entity.setPose(backupState.entityPose);
    }
  }

  protected static class EntityRenderState {
    final Component customName;
    final boolean shouldShowName;
    final boolean minecraftHideGui;
    final CustomRotation rootRotation;
    final CustomScale rootScale;
    final ModelPose modelPose;
    final Pose entityPose;

    EntityRenderState(LivingEntity livingEntity, EasyNPC<?> easyNPC) {
      this.customName = livingEntity.getCustomName();
      this.shouldShowName = livingEntity.shouldShowName();
      Minecraft minecraft = Minecraft.getInstance();
      this.minecraftHideGui = minecraft != null ? minecraft.options.hideGui : false;
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null) {
        this.rootRotation = modelData.getModelPartRotation(ModelPartType.ROOT);
        this.rootScale = modelData.getModelPartScale(ModelPartType.ROOT);
        this.modelPose = modelData.getModelPose();
      } else {
        this.rootRotation = null;
        this.rootScale = null;
        this.modelPose = null;
      }
      this.entityPose = easyNPC.getEntity().getPose();
    }
  }
}
