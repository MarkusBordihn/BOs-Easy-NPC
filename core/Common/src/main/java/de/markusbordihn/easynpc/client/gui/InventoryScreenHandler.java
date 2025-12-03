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
import de.markusbordihn.easynpc.client.texture.CustomTextureManager;
import de.markusbordihn.easynpc.client.texture.PlayerTextureManager;
import de.markusbordihn.easynpc.client.texture.RemoteTextureManager;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.skin.VariantTexture;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.VillagerRenderer;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.PlayerRenderState;
import net.minecraft.client.renderer.entity.state.VillagerRenderState;
import net.minecraft.client.renderer.entity.state.ZombieVillagerRenderState;
import net.minecraft.client.resources.PlayerSkin;
import net.minecraft.client.resources.PlayerSkin.Model;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.monster.ZombieVillager;
import net.minecraft.world.entity.npc.Villager;
import net.minecraft.world.entity.npc.VillagerData;
import org.joml.Quaternionf;
import org.joml.Vector3f;

public class InventoryScreenHandler {

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

    // Get render data and render custom entity if available.
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

  public static PlayerRenderState getCustomPlayerRenderState(
      EntityRenderer<? super Entity, ?> entityRenderer,
      PlayerRenderState playerRenderState,
      SkinDataCapable<?> skinData,
      EasyNPC<?> easyNPC) {
    PlayerRenderState cumstomPlayerRenderState =
        (PlayerRenderState) entityRenderer.createRenderState();
    cumstomPlayerRenderState.scale = playerRenderState.scale;
    cumstomPlayerRenderState.mainArm = playerRenderState.mainArm;
    cumstomPlayerRenderState.x = playerRenderState.x;
    cumstomPlayerRenderState.y = playerRenderState.y;
    cumstomPlayerRenderState.z = playerRenderState.z;
    cumstomPlayerRenderState.bodyRot = playerRenderState.bodyRot;

    Model playerSkinModel =
        skinData.getSkinModel() == SkinModel.HUMANOID_SLIM ? Model.SLIM : Model.WIDE;
    if (skinData.getSkinType() == SkinType.NONE) {
      return cumstomPlayerRenderState;
    } else if (skinData.getSkinType() == SkinType.DEFAULT) {
      VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
      if (variantData.getSkinVariantType() instanceof VariantTexture variantTexture) {
        cumstomPlayerRenderState.skin =
            new PlayerSkin(
                variantTexture.getTextureLocation(), null, null, null, playerSkinModel, false);
      }
    } else if (skinData.getSkinType() == SkinType.CUSTOM) {
      cumstomPlayerRenderState.skin =
          new PlayerSkin(
              CustomTextureManager.getOrCreateTextureWithDefault(
                  skinData, playerRenderState.skin.texture()),
              null,
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.PLAYER_SKIN) {
      cumstomPlayerRenderState.skin =
          new PlayerSkin(
              PlayerTextureManager.getOrCreateTextureWithDefault(
                  skinData, playerRenderState.skin.texture()),
              null,
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.INSECURE_REMOTE_URL) {
      cumstomPlayerRenderState.skin =
          new PlayerSkin(
              RemoteTextureManager.getOrCreateTextureWithDefault(
                  skinData, playerRenderState.skin.texture()),
              skinData.getSkinURL(),
              null,
              null,
              playerSkinModel,
              false);
    } else if (skinData.getSkinType() == SkinType.SECURE_REMOTE_URL) {
      cumstomPlayerRenderState.skin =
          new PlayerSkin(
              RemoteTextureManager.getOrCreateTextureWithDefault(
                  skinData, playerRenderState.skin.texture()),
              skinData.getSkinURL(),
              null,
              null,
              playerSkinModel,
              true);
    }
    return cumstomPlayerRenderState;
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

    // Handle player renderer with custom skin - check render state type first.
    EntityRenderState baseRenderState = entityRenderer.createRenderState(livingEntity, 1.0F);
    if (baseRenderState instanceof PlayerRenderState playerRenderState) {
      PlayerRenderState customPlayerRenderState =
          getCustomPlayerRenderState(entityRenderer, playerRenderState, skinData, easyNPC);
      guiGraphics.submitEntityRenderState(
          customPlayerRenderState,
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
      VillagerData zombieVillagerData = ((ZombieVillager) livingEntity).getVillagerData();
      customZombieVillagerRenderState.villagerData = zombieVillagerData;

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
    if (entityRenderer instanceof HumanoidMobRenderer
        && baseRenderState instanceof HumanoidRenderState) {
      HumanoidRenderState customHumanoidRenderState =
          (HumanoidRenderState) entityRenderer.createRenderState();
      HumanoidMobRenderer humanoidRenderer = (HumanoidMobRenderer) entityRenderer;
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
