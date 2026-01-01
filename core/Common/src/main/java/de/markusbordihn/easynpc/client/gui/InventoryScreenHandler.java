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
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.data.skin.VariantTexture;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.entity.CatRenderer;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.HumanoidMobRenderer;
import net.minecraft.client.renderer.entity.VillagerRenderer;
import net.minecraft.client.renderer.entity.state.AvatarRenderState;
import net.minecraft.client.renderer.entity.state.CatRenderState;
import net.minecraft.client.renderer.entity.state.EntityRenderState;
import net.minecraft.client.renderer.entity.state.HumanoidRenderState;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.client.renderer.entity.state.VillagerRenderState;
import net.minecraft.client.renderer.entity.state.ZombieVillagerRenderState;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.entity.animal.feline.Cat;
import net.minecraft.world.entity.monster.zombie.ZombieVillager;
import net.minecraft.world.entity.npc.villager.Villager;
import net.minecraft.world.entity.player.PlayerModelType;
import net.minecraft.world.entity.player.PlayerSkin;
import org.joml.Quaternionf;
import org.joml.Vector3f;

public class InventoryScreenHandler {

  private static final ThreadLocal<Boolean> BYPASS_MIXIN = ThreadLocal.withInitial(() -> false);
  private static final int FULL_BRIGHT = 15728880;

  public static boolean isBypassMixin() {
    return BYPASS_MIXIN.get();
  }

  public static void setBypassMixin(boolean bypass) {
    BYPASS_MIXIN.set(bypass);
  }

  private static EntityRenderState extractRenderState(LivingEntity livingEntity) {
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super LivingEntity, ?> entityRenderer =
        entityRenderDispatcher.getRenderer(livingEntity);
    return entityRenderer.createRenderState(livingEntity, 1.0F);
  }

  private static void applyRotationsAndScale(
      EntityRenderState renderState, float xRotation, float yRotation) {
    if (!(renderState instanceof LivingEntityRenderState livingEntityRenderState)) {
      return;
    }

    // Apply rotations
    livingEntityRenderState.bodyRot = 180.0F + xRotation * 20.0F;
    livingEntityRenderState.yRot = xRotation * 20.0F;
    if (livingEntityRenderState.pose != Pose.FALL_FLYING) {
      livingEntityRenderState.xRot = -yRotation * 20.0F;
    } else {
      livingEntityRenderState.xRot = 0.0F;
    }

    // Normalize scale
    livingEntityRenderState.boundingBoxWidth /= livingEntityRenderState.scale;
    livingEntityRenderState.boundingBoxHeight /= livingEntityRenderState.scale;
    livingEntityRenderState.scale = 1.0F;
  }

  private static void submitEntityRenderState(
      GuiGraphics guiGraphics,
      EntityRenderState renderState,
      int size,
      Vector3f translation,
      Quaternionf rotation,
      Quaternionf entityRotation,
      int left,
      int top,
      int right,
      int bottom) {
    renderState.lightCoords = FULL_BRIGHT;
    guiGraphics.submitEntityRenderState(
        renderState, size, translation, rotation, entityRotation, left, top, right, bottom);
  }

  public static boolean onRenderEntityInInventoryFollowsMouse(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      int size,
      float yOffset,
      float mouseX,
      float mouseY,
      LivingEntity entity,
      EasyNPC<?> easyNPC) {
    if (isBypassMixin()) {
      return false;
    }

    // Get render data and render custom entity if available.
    RenderDataCapable<?> renderData = easyNPC.getEasyNPCRenderData();
    if (renderData != null
        && renderData.getRenderDataEntry() != null
        && renderData.getRenderDataEntry().getRenderType() == RenderType.CUSTOM_ENTITY
        && renderData.getRenderDataEntry().getRenderEntityType() != null) {
      return renderCustomEntityInInventory(
          guiGraphics,
          left,
          top,
          right,
          bottom,
          size,
          yOffset,
          mouseX,
          mouseY,
          renderData.getRenderDataEntry().getRenderEntityType(),
          easyNPC);
    }

    // Get skin data and render custom entity if available.
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData != null && skinData.getSkinType() != SkinType.NONE) {
      return renderSkinEntityInInventory(
          guiGraphics, left, top, right, bottom, size, yOffset, mouseX, mouseY, skinData, easyNPC);
    }

    // Fallback to default entity rendering with mouse tracking.
    renderDefaultEntityInInventory(
        guiGraphics, left, top, right, bottom, size, yOffset, mouseX, mouseY, entity);
    return true;
  }

  private static void renderDefaultEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      int size,
      float yOffset,
      float mouseX,
      float mouseY,
      LivingEntity entity) {
    float centerX = (left + right) / 2.0F;
    float centerY = (top + bottom) / 2.0F;
    float xRotation = (float) Math.atan((centerX - mouseX) / 40.0F);
    float yRotation = (float) Math.atan((centerY - mouseY) / 40.0F);

    Quaternionf rotation = (new Quaternionf()).rotateZ((float) Math.PI);
    Quaternionf entityRotation =
        (new Quaternionf()).rotateX(yRotation * 20.0F * ((float) Math.PI / 180F));
    rotation.mul(entityRotation);

    EntityRenderState entityRenderState = extractRenderState(entity);
    Vector3f translation =
        new Vector3f(0.0F, entityRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
    applyRotationsAndScale(entityRenderState, xRotation, yRotation);

    submitEntityRenderState(
        guiGraphics,
        entityRenderState,
        size,
        translation,
        rotation,
        entityRotation,
        left,
        top,
        right,
        bottom);
  }

  public static boolean renderCustomEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      int size,
      float yOffset,
      float mouseX,
      float mouseY,
      EntityType<? extends Entity> entityType,
      EasyNPC<?> easyNPC) {

    // Create custom entity for rendering and copy data.
    PathfinderMob customEntity =
        EntityTypeManager.getPathfinderMob(entityType, easyNPC.getEntityLevel());
    if (customEntity == null) {
      return false;
    }
    RendererManager.copyCustomLivingEntityData(easyNPC.getPathfinderMob(), customEntity);

    // Calculate rotations based on mouse position
    float centerX = (left + right) / 2.0F;
    float centerY = (top + bottom) / 2.0F;
    float xRotation = (float) Math.atan((centerX - mouseX) / 40.0F);
    float yRotation = (float) Math.atan((centerY - mouseY) / 40.0F);

    Quaternionf rotation = (new Quaternionf()).rotateZ((float) Math.PI);
    Quaternionf entityRotation =
        (new Quaternionf()).rotateX(yRotation * 20.0F * ((float) Math.PI / 180F));
    rotation.mul(entityRotation);

    // Get entity renderer and render state.
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super Entity, ?> entityRenderer =
        entityRenderDispatcher.getRenderer(customEntity);
    EntityRenderState entityRenderState = entityRenderer.createRenderState(customEntity, 1.0F);

    Vector3f translation =
        new Vector3f(0.0F, entityRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
    applyRotationsAndScale(entityRenderState, xRotation, yRotation);

    submitEntityRenderState(
        guiGraphics,
        entityRenderState,
        size,
        translation,
        rotation,
        entityRotation,
        left,
        top,
        right,
        bottom);
    return true;
  }

  public static boolean renderSkinEntityInInventory(
      GuiGraphics guiGraphics,
      int left,
      int top,
      int right,
      int bottom,
      int size,
      float yOffset,
      float mouseX,
      float mouseY,
      SkinDataCapable<?> skinData,
      EasyNPC<?> easyNPC) {

    // Calculate rotations based on mouse position
    float centerX = (left + right) / 2.0F;
    float centerY = (top + bottom) / 2.0F;
    float xRotation = (float) Math.atan((centerX - mouseX) / 40.0F);
    float yRotation = (float) Math.atan((centerY - mouseY) / 40.0F);

    Quaternionf rotation = (new Quaternionf()).rotateZ((float) Math.PI);
    Quaternionf entityRotation =
        (new Quaternionf()).rotateX(yRotation * 20.0F * ((float) Math.PI / 180F));
    rotation.mul(entityRotation);

    // Get entity renderer.
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    EntityRenderDispatcher entityRenderDispatcher =
        Minecraft.getInstance().getEntityRenderDispatcher();
    EntityRenderer<? super Entity, ?> entityRenderer =
        entityRenderDispatcher.getRenderer(livingEntity);
    EntityRenderState baseRenderState = entityRenderer.createRenderState(livingEntity, 1.0F);

    // Handle player renderer with custom skin - check render state type first.
    if (baseRenderState instanceof AvatarRenderState avatarRenderState) {
      AvatarRenderState customAvatarRenderState =
          getCustomPlayerRenderState(entityRenderer, avatarRenderState, skinData, easyNPC);

      Vector3f translation =
          new Vector3f(0.0F, customAvatarRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
      applyRotationsAndScale(customAvatarRenderState, xRotation, yRotation);

      submitEntityRenderState(
          guiGraphics,
          customAvatarRenderState,
          size,
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

      Vector3f translation =
          new Vector3f(0.0F, customCatRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
      applyRotationsAndScale(customCatRenderState, xRotation, yRotation);

      submitEntityRenderState(
          guiGraphics,
          customCatRenderState,
          size,
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

      Vector3f translation =
          new Vector3f(0.0F, customVillagerRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
      applyRotationsAndScale(customVillagerRenderState, xRotation, yRotation);

      submitEntityRenderState(
          guiGraphics,
          customVillagerRenderState,
          size,
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
      customZombieVillagerRenderState.villagerData =
          ((ZombieVillager) livingEntity).getVillagerData();

      Vector3f translation =
          new Vector3f(
              0.0F, customZombieVillagerRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
      applyRotationsAndScale(customZombieVillagerRenderState, xRotation, yRotation);

      submitEntityRenderState(
          guiGraphics,
          customZombieVillagerRenderState,
          size,
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
    @SuppressWarnings({"rawtypes", "unchecked"})
    HumanoidMobRenderer humanoidRenderer =
        entityRenderer instanceof HumanoidMobRenderer ? (HumanoidMobRenderer) entityRenderer : null;
    if (humanoidRenderer != null && baseRenderState instanceof HumanoidRenderState) {
      HumanoidRenderState customHumanoidRenderState =
          (HumanoidRenderState) entityRenderer.createRenderState();
      humanoidRenderer.extractRenderState(easyNPC.getMob(), customHumanoidRenderState, 1.0F);

      Vector3f translation =
          new Vector3f(0.0F, customHumanoidRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
      applyRotationsAndScale(customHumanoidRenderState, xRotation, yRotation);

      submitEntityRenderState(
          guiGraphics,
          customHumanoidRenderState,
          size,
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
    Vector3f translation =
        new Vector3f(0.0F, baseRenderState.boundingBoxHeight / 2.0F + yOffset, 0.0F);
    applyRotationsAndScale(baseRenderState, xRotation, yRotation);

    submitEntityRenderState(
        guiGraphics,
        baseRenderState,
        size,
        translation,
        rotation,
        entityRotation,
        left,
        top,
        right,
        bottom);
    return true;
  }

  public static AvatarRenderState getCustomPlayerRenderState(
      EntityRenderer<? super Entity, ?> entityRenderer,
      AvatarRenderState avatarRenderState,
      SkinDataCapable<?> skinData,
      EasyNPC<?> easyNPC) {
    AvatarRenderState cumstomAvatarRenderState =
        (AvatarRenderState) entityRenderer.createRenderState();
    cumstomAvatarRenderState.scale = avatarRenderState.scale;
    cumstomAvatarRenderState.boundingBoxHeight = avatarRenderState.boundingBoxHeight;
    cumstomAvatarRenderState.boundingBoxWidth = avatarRenderState.boundingBoxWidth;
    cumstomAvatarRenderState.mainArm = avatarRenderState.mainArm;
    cumstomAvatarRenderState.x = avatarRenderState.x;
    cumstomAvatarRenderState.y = avatarRenderState.y;
    cumstomAvatarRenderState.z = avatarRenderState.z;
    cumstomAvatarRenderState.bodyRot = avatarRenderState.bodyRot;

    PlayerModelType playerModelType =
        skinData.getSkinModel() == SkinModel.HUMANOID_SLIM
            ? PlayerModelType.SLIM
            : PlayerModelType.WIDE;
    if (skinData.getSkinType() == SkinType.NONE) {
      return cumstomAvatarRenderState;
    } else if (skinData.getSkinType() == SkinType.DEFAULT) {
      VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
      if (variantData.getSkinVariantType() instanceof VariantTexture variantTexture) {
        cumstomAvatarRenderState.skin =
            new PlayerSkin(variantTexture.getResourceTexture(), null, null, playerModelType, false);
      }
    } else if (skinData.getSkinType() == SkinType.CUSTOM) {
      cumstomAvatarRenderState.skin =
          new PlayerSkin(avatarRenderState.skin.body(), null, null, playerModelType, false);
    } else if (skinData.getSkinType() == SkinType.PLAYER_SKIN) {
      cumstomAvatarRenderState.skin =
          new PlayerSkin(avatarRenderState.skin.body(), null, null, playerModelType, false);
    } else if (skinData.getSkinType() == SkinType.INSECURE_REMOTE_URL) {
      cumstomAvatarRenderState.skin =
          new PlayerSkin(avatarRenderState.skin.body(), null, null, playerModelType, false);
    } else if (skinData.getSkinType() == SkinType.SECURE_REMOTE_URL) {
      cumstomAvatarRenderState.skin =
          new PlayerSkin(avatarRenderState.skin.body(), null, null, playerModelType, true);
    }
    return cumstomAvatarRenderState;
  }
}
