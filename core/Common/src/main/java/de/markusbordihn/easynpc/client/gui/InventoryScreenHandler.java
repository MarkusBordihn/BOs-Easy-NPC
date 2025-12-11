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

package de.markusbordihn.easynpc.client.gui;

import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.renderer.manager.RendererManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.entity.CatRenderer;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.VillagerRenderer;
import net.minecraft.client.renderer.entity.state.CatRenderState;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.VillagerRenderState;
import net.minecraft.client.renderer.entity.state.ZombieVillagerRenderState;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.animal.Cat;
import net.minecraft.world.entity.monster.ZombieVillager;
import net.minecraft.world.entity.npc.Villager;
import org.joml.Quaternionf;
import org.joml.Vector3f;

public class InventoryScreenHandler {

  private static final ThreadLocal<Boolean> BYPASS_MIXIN = ThreadLocal.withInitial(() -> false);

  public static boolean isBypassMixin() {
    return BYPASS_MIXIN.get();
  }

  public static void setBypassMixin(boolean bypass) {
    BYPASS_MIXIN.set(bypass);
  }

  public static boolean onRenderEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      float scale,
      Vector3f translation,
      Quaternionf rotation,
      Quaternionf entityRotation,
      LivingEntity entity,
      EasyNPC<?> easyNPC) {
    if (isBypassMixin()) {
      return false;
    }

    // Get render data and render custom entity if avaible.
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData != null
        && renderData.getRenderDataSet() != null
        && renderData.getRenderDataSet().getRenderType() == RenderType.CUSTOM_ENTITY
        && renderData.getRenderDataSet().getRenderEntityType() != null) {
      return renderCustomEntityInInventory(
          guiGraphics,
          left,
          top,
          right,
          bottom,
          scale,
          translation,
          rotation,
          entityRotation,
          renderData.getRenderDataSet().getRenderEntityType(),
          easyNPC);
    }

    // Get skin data and render custom entity if available.
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null && skinData.getSkinType() != SkinType.NONE) {
      return renderSkinEntityInInventory(
          guiGraphics,
          left,
          top,
          right,
          bottom,
          scale,
          translation,
          rotation,
          entityRotation,
          skinData,
          easyNPC);
    }

    // Fallback to default entity rendering.
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super LivingEntity, ?> entityrenderer =
        entityRenderDispatcher.getRenderer(entity);
    EntityRenderState entityrenderstate = entityrenderer.createRenderState(entity, 1.0F);
    entityrenderstate.hitboxesRenderState = null;
    guiGraphics.submitEntityRenderState(
        entityrenderstate, scale, translation, rotation, entityRotation, left, top, right, bottom);
    return true;
  }

  public static boolean renderCustomEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      float scale,
      Vector3f translation,
      Quaternionf rotation,
      Quaternionf entityRotation,
      EntityType<? extends Entity> entityType,
      EasyNPC<?> easyNPC) {

    // Create custom entity for rendering and copy data.
    PathfinderMob customEntity =
        EntityTypeManager.getPathfinderMob(entityType, easyNPC.getEntityLevel());
    if (customEntity == null) {
      return false;
    }
    RendererManager.copyCustomLivingEntityData(easyNPC.getPathfinderMob(), customEntity);

    // Get entity renderer and render state.
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super Entity, ?> entityrenderer =
        entityRenderDispatcher.getRenderer(customEntity);
    EntityRenderState entityrenderstate = entityrenderer.createRenderState(customEntity, 1.0F);
    entityrenderstate.hitboxesRenderState = null;
    guiGraphics.submitEntityRenderState(
        entityrenderstate, scale, translation, rotation, entityRotation, left, top, right, bottom);
    return true;
  }

  public static boolean renderSkinEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      float scale,
      Vector3f translation,
      Quaternionf rotation,
      Quaternionf entityRotation,
      SkinDataCapable<?> skinData,
      EasyNPC<?> easyNPC) {
    // Get entity renderer.
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super Entity, ?> entityRenderer =
        entityRenderDispatcher.getRenderer(livingEntity);

    // Handle player renderer with custom skin.
    EntityRenderState baseRenderState = entityRenderer.createRenderState(livingEntity, 1.0F);
    if (baseRenderState instanceof HumanoidRenderState humanoidRenderState) {
      guiGraphics.submitEntityRenderState(
          humanoidRenderState,
          scale,
          translation,
          rotation,
          entityRotation,
          left,
          top,
          right,
          bottom);
      return true;
    }

    // Handle cat renderer - create fresh render state to avoid caching issues.
    if (baseRenderState instanceof CatRenderState && livingEntity instanceof Cat) {
      CatRenderState customCatRenderState = (CatRenderState) entityRenderer.createRenderState();
      CatRenderer catRenderer = (CatRenderer) (EntityRenderer<? super Cat, ?>) entityRenderer;
      catRenderer.extractRenderState((Cat) livingEntity, customCatRenderState, 1.0F);
      customCatRenderState.texture = catRenderer.getTextureLocation(customCatRenderState);
      customCatRenderState.hitboxesRenderState = null;
      guiGraphics.submitEntityRenderState(
          customCatRenderState,
          scale,
          translation,
          rotation,
          entityRotation,
          left,
          top,
          right,
          bottom);
      return true;
    }

    // Handle villager renderer - create fresh render state to avoid caching issues.
    if (baseRenderState instanceof VillagerRenderState && livingEntity instanceof Villager) {
      VillagerRenderState customVillagerRenderState =
          (VillagerRenderState) entityRenderer.createRenderState();
      VillagerRenderer villagerRenderer =
          (VillagerRenderer) (EntityRenderer<? super Villager, ?>) entityRenderer;
      villagerRenderer.extractRenderState((Villager) livingEntity, customVillagerRenderState, 1.0F);
      customVillagerRenderState.hitboxesRenderState = null;
      guiGraphics.submitEntityRenderState(
          customVillagerRenderState,
          scale,
          translation,
          rotation,
          entityRotation,
          left,
          top,
          right,
          bottom);
      return true;
    }

    // Handle zombie villager renderer - create fresh render state to avoid caching issues.
    if (baseRenderState instanceof ZombieVillagerRenderState
        && livingEntity instanceof ZombieVillager) {
      ZombieVillagerRenderState customZombieVillagerRenderState =
          (ZombieVillagerRenderState) entityRenderer.createRenderState();
      // Override villagerData with current entity data to show correct type and profession
      customZombieVillagerRenderState.villagerData =
          ((ZombieVillager) livingEntity).getVillagerData();
      customZombieVillagerRenderState.hitboxesRenderState = null;
      guiGraphics.submitEntityRenderState(
          customZombieVillagerRenderState,
          scale,
          translation,
          rotation,
          entityRotation,
          left,
          top,
          right,
          bottom);
      return true;
    }

    // Handle humanoid mob renderer - create fresh render state to avoid caching issues.
    if (entityRenderer instanceof HumanoidMobRenderer humanoidRenderer
        && baseRenderState instanceof HumanoidRenderState) {
      HumanoidRenderState customHumanoidRenderState =
          (HumanoidRenderState) entityRenderer.createRenderState();
      humanoidRenderer.extractRenderState(easyNPC.getMob(), customHumanoidRenderState, 1.0F);
      customHumanoidRenderState.hitboxesRenderState = null;
      guiGraphics.submitEntityRenderState(
          customHumanoidRenderState,
          scale,
          translation,
          rotation,
          entityRotation,
          left,
          top,
          right,
          bottom);
      return true;
    }

    // Fallback to default rendering.
    baseRenderState.hitboxesRenderState = null;
    guiGraphics.submitEntityRenderState(
        baseRenderState, scale, translation, rotation, entityRotation, left, top, right, bottom);
    return true;
  }
}
