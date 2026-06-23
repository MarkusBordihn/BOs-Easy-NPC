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
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.render.EntityRenderOverrides;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.util.concurrent.atomic.AtomicInteger;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Pose;

public class EntityScreenRenderer {

  private static final AtomicInteger RENDER_ENTITY_ID =
      new AtomicInteger(Integer.MAX_VALUE - 100000);

  protected EntityScreenRenderer() {}

  /**
   * Assigns a unique render-only entity ID, if none has been assigned yet. GUI preview entities are
   * never added to a level, so in MC 26.2 {@code Entity.getId()} throws {@code
   * IllegalStateException} until an ID is set. The vanilla render-state extraction (e.g. {@code
   * ItemModelResolver#updateForLiving} for held items) reads {@code getId()}, so we assign a
   * render-only ID once to avoid crashing.
   */
  public static void assignRenderEntityId(Entity entity) {
    try {
      entity.getId();
    } catch (IllegalStateException exception) {
      entity.setId(RENDER_ENTITY_ID.getAndDecrement());
    }
  }

  public static void renderEntity(
      GuiGraphicsExtractor guiGraphics,
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

    InventoryScreen.extractEntityInInventoryFollowsMouse(
        guiGraphics,
        config.left(),
        config.top(),
        config.right(),
        config.bottom(),
        config.scale(),
        config.yOffset(),
        mouseX,
        mouseY,
        livingEntity);

    restoreEntityState(easyNPC, backupState);
  }

  public static void renderEntityRaw(
      GuiGraphicsExtractor guiGraphics,
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

    assignRenderEntityId(entity);

    if (overrides.invisible() != null) {
      entity.setInvisible(overrides.invisible());
    }

    if (overrides.hideNameTag() != null && overrides.hideNameTag()) {
      Minecraft minecraft = Minecraft.getInstance();
      if (minecraft != null) {
        if (!minecraft.gui.hud.isHidden()) {
          minecraft.gui.hud.toggle();
        }
      } else {
        livingEntity.setCustomName(null);
        livingEntity.setCustomNameVisible(false);
      }
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null) {
      if (overrides.rootRotation() != null) {
        modelData.setModelRootRotation(overrides.rootRotation());
      }
      if (overrides.rootScale() != null) {
        modelData.setModelRootScale(overrides.rootScale());
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
    if (minecraft != null && minecraft.gui.hud.isHidden() != backupState.minecraftHideGui) {
      minecraft.gui.hud.toggle();
    }

    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    if (modelData != null) {
      if (backupState.rootData != null) {
        modelData.setModelRootData(backupState.rootData);
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
    final RootModelData rootData;
    final ModelPose modelPose;
    final Pose entityPose;

    EntityRenderState(LivingEntity livingEntity, EasyNPC<?> easyNPC) {
      this.customName = livingEntity.getCustomName();
      this.shouldShowName = livingEntity.shouldShowName();
      Minecraft minecraft = Minecraft.getInstance();
      this.minecraftHideGui = minecraft != null && minecraft.gui.hud.isHidden();
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData != null) {
        this.rootData = modelData.getModelRootData();
        this.modelPose = modelData.getModelPose();
      } else {
        this.rootData = null;
        this.modelPose = null;
      }
      this.entityPose = easyNPC.getEntity().getPose();
    }
  }
}
